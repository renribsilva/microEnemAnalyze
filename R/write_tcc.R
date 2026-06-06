#' @title Exportar TCC Consolidado em Arquivo Único (Streaming)
#'
#' @param data Data.table com colunas NU_NOTA_ de todas as áreas (Microdados)
#' @param score Lista de data.tables (um por área) com NU_SCORE e NU_NOTA_
#' @param path_json Caminho base ou nome do arquivo para salvar
#' @param ano Ano do exame
#'
#' @export
write_tcc <- function(data, score, path_json, ano) {
  cli::cli_h1("Processamento Consolidado: TCC Teorico + Empirico (Streaming)")

  cli::cli_process_start("Validando argumentos")

  if (missing(data)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg data} e obrigatorio.",
      "i" = "Por favor, forneca os microdados do ENEM."
    ))
  }

  if (missing(path_json)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg path_csv} e obrigatorio.",
      "i" = "Por favor, forneca o caminho onde o csv sera gravado."
    ))
  }

  if (!is.character(path_json)) {
    cli::cli_abort("{.arg path_csv} precisa ser do tipo character.")
  }

  # Normaliza os microdados
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  cli::cli_process_done()

  # ------------------------------------------------------------------
  # Objetos globais
  # ------------------------------------------------------------------
  cli::cli_process_start("Recuperando objetos do Global Env")
  itens_df <- get(paste0("itens_", ano), envir = .GlobalEnv)
  dic_df <- get(paste0("dic_", ano), envir = .GlobalEnv)
  consts <- get("constantes", envir = .GlobalEnv)
  cli::cli_process_done()

  dic_df_p1 <- dic_df[dic_df$tipo == "1", ]
  cod_selected <- dic_df_p1$codigo

  # ------------------------------------------------------------------
  # Arquivo de saída (streaming)
  # ------------------------------------------------------------------
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, paste0("tcc_", ano, ".json"))
  }

  dir.create(dirname(final_file), recursive = TRUE, showWarnings = FALSE)

  con <- file(final_file, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  writeLines("{", con)
  first_entry <- TRUE

  # ------------------------------------------------------------------
  # Loop por área
  # ------------------------------------------------------------------
  for (area_dt in score) {
    col_nota <- names(area_dt)[grepl("^NU_NOTA_", names(area_dt))]
    if (length(col_nota) == 0) {
      next
    }

    area_nome <- sub("^NU_NOTA_", "", col_nota[1])

    cli::cli_h3("Area: {.field {area_nome}}")

    const_row <- consts[consts$area == area_nome, ]
    if (nrow(const_row) != 1) {
      cli::cli_abort("Constantes invalidas para a area: {.val {area_nome}}")
    }

    col_prova_area <- paste0("CO_PROVA_", area_nome)
    new_col_score <- "NU_SCORE"

    tabela_real <- area_dt[
      get(col_prova_area) %in% cod_selected & get(col_nota) > 0,
      list(media = mean(get(new_col_score), na.rm = TRUE)),
      keyby = list(x = as.integer(round(get(col_nota), 0)))
    ]

    codigos <- unique(dic_df_p1$codigo[dic_df_p1$area == area_nome])

    pbar <- cli::cli_progress_bar(
      total = length(codigos),
      format = "  {cli::pb_spin} Processando
      cadernos [{pb_current}/{pb_total}] {pb_percent} | ETA: {pb_eta}"
    )

    for (codigo in codigos) {
      cli::cli_progress_update(id = pbar)

      col_nota_area <- paste0("NU_NOTA_", area_nome)

      data_filtrado <- data[
        get(col_prova_area) %in%
          cod_selected &
          !is.na(get(col_nota_area)) &
          get(col_nota_area) > 0
      ]

      notas <- data_filtrado[[col_nota_area]]

      if (length(notas) == 0) {
        # CLI: Evita que alertas quebrem a visualização da barra
        cli::cli_inform("Sem dados para caderno {codigo}. Pulando...")
        next
      }

      nota_min <- min(notas)
      nota_max <- max(notas)
      escala_x <- seq(floor(nota_min), ceiling(nota_max), by = 1)

      df_merge <- merge(
        data.table::data.table(x = escala_x),
        tabela_real,
        by = "x",
        all.x = TRUE
      )

      theta_metrico <- matrix(
        (escala_x - const_row$d) / const_row$k,
        ncol = 1
      )

      versoes <- "X"

      linguas <- if (area_nome == "LC") c(0, 1) else "X"

      for (v_digital in versoes) {
        for (lingua in linguas) {
          cor_name_base <- dic_df$cor[dic_df$codigo == codigo][1]

          v_digital_ajustada <- v_digital
          if (grepl("\\(Digital\\)", cor_name_base, ignore.case = TRUE)) {
            v_digital_ajustada <- "D"
          }

          cor_name <- if (area_nome == "LC") {
            if (lingua == 0) {
              paste0(cor_name_base, " (Ingles)")
            } else if (lingua == 1) {
              paste0(cor_name_base, " (Espanhol)")
            } else {
              cor_name_base
            }
          } else {
            cor_name_base
          }

          itens_caderno <- itens_df[itens_df$CO_PROVA == codigo, ]

          col_lingua <- "TP_LINGUA"
          col_posicao <- "CO_POSICAO"

          if (area_nome == "LC") {
            itens_caderno <- itens_caderno[
              is.na(get(col_lingua)) | get(col_lingua) == lingua,
            ]
          }

          itens_caderno <- itens_caderno[order(get(col_posicao)), ]

          if (nrow(itens_caderno) != 45) {
            cli::cli_abort(
              "ERRO CRITICO: caderno invalido (n != 45) |
              codigo={codigo} | area={area_nome} |
              versao={v_digital} | lingua={lingua} |
              n_itens={nrow(itens_caderno)}"
            )
          }

          key_name <- paste(codigo, lingua, v_digital_ajustada, sep = "_")

          itens_mirt <- data.table::data.table(
            a1 = as.numeric(itens_caderno$NU_PARAM_A),
            d = -as.numeric(itens_caderno$NU_PARAM_A) *
              as.numeric(itens_caderno$NU_PARAM_B),
            g = as.numeric(itens_caderno$NU_PARAM_C)
          )

          mod_test <- mirtCAT::generate.mirt_object(itens_mirt, "3PL")
          escore <- mirt::expected.test(mod_test, theta_metrico)

          den <- max(escore) - min(escore)

          if (den == 0) {
            cli::cli_abort("Escore teorico constante")
          }

          escore <- (escore - min(escore)) / den * nrow(itens_mirt)

          obj <- list(
            area = area_nome,
            labels_x = escala_x,
            metadata = list(
              codigo = codigo,
              area = area_nome,
              cor = cor_name,
              min = nota_min,
              max = nota_max,
              lingua = lingua,
              versao_digital = v_digital_ajustada,
              b_medio_enem = round(
                mean(itens_caderno$NU_PARAM_B, na.rm = TRUE) *
                  const_row$k +
                  const_row$d,
                1
              )
            ),
            data_teorico = round(as.vector(escore), 2),
            data_empirico = round(df_merge$media, 2)
          )

          json_entry <- jsonlite::toJSON(
            obj,
            auto_unbox = TRUE,
            pretty = TRUE,
            na = "null"
          )

          prefix <- if (first_entry) "" else ",\n"

          # 3. Escreve tudo de uma vez
          cat(
            paste0(prefix, "\"", key_name, "\": ", json_entry),
            file = con
          )

          first_entry <- FALSE
        }
      }
    }
    cli::cli_progress_done(id = pbar)
  }

  writeLines("\n}", con)
  cli::cli_rule()
  cli::cli_alert_success("Processamento completo (streaming JSON).")
}
