#' Exportar TCC Consolidado em Arquivo Único (Streaming)
#' @param data Data.table com colunas NU_NOTA_ de todas as áreas (Microdados)
#' @param score Lista de data.tables (um por área) com NU_SCORE e NU_NOTA_
#' @param path_json Caminho base ou nome do arquivo para salvar
#' @param ano Ano do exame
#' @export
write_tcc <- function(data, score, path_json, ano) {

  cli::cli_h2("Processamento Consolidado: TCC Teórico + Empírico (Streaming)")

  # ------------------------------------------------------------------
  # Objetos globais
  # ------------------------------------------------------------------
  cli::cli_process_start("Recuperando objetos do Global Env")
  itens_df <- get(paste0("itens_", ano), envir = .GlobalEnv)
  dic_df   <- get(paste0("dic_", ano),   envir = .GlobalEnv)
  consts   <- get("constantes",           envir = .GlobalEnv)
  cli::cli_process_done()

  dic_df_P1 <- dic_df[dic_df$aplicacao == "P1", ]

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
    if (length(col_nota) == 0) next

    area_nome <- sub("^NU_NOTA_", "", col_nota[1])
    cli::cli_alert_info("Processando área: {.val {area_nome}}")

    const_row <- consts[consts$area == area_nome, ]
    if (nrow(const_row) != 1) {
      stop("Constantes inválidas para a área: ", area_nome, call. = FALSE)
    }

    codigos <- unique(dic_df_P1$codigo[dic_df_P1$area == area_nome])

    for (codigo in codigos) {

      col_nota_area <- paste0("NU_NOTA_", area_nome)

      notas <- data[[col_nota_area]]
      notas <- notas[!is.na(notas) & notas > 0]
      if (length(notas) == 0) next

      nota_min <- min(notas)
      nota_max <- max(notas)
      escala_x <- seq(floor(nota_min), ceiling(nota_max), by = 1)

      tabela_real <- area_dt[
        get(col_nota) > 0,
        .(media = mean(NU_SCORE, na.rm = TRUE)),
        keyby = .(x = as.integer(round(get(col_nota), 0)))
      ]

      df_merge <- merge(
        data.table::data.table(x = escala_x),
        tabela_real,
        by = "x",
        all.x = TRUE
      )

      Theta_metrico <- matrix(
        (escala_x - const_row$d) / const_row$k,
        ncol = 1
      )

      tem_digital <- "TP_VERSAO_DIGITAL" %in% names(itens_df)
      versoes <- if (tem_digital) {
        unique(na.omit(itens_df$TP_VERSAO_DIGITAL[itens_df$CO_PROVA == codigo]))
      } else {
        "X"
      }
      if (length(versoes) == 0) versoes <- "X"

      linguas <- if (area_nome == "LC") c(0, 1) else "X"

      for (v_digital in versoes) {

        for (lingua in linguas) {

          cor_name_base <- dic_df$cor[dic_df$codigo == codigo][1]
          cor_name <- if (area_nome == "LC") {
            if (lingua == 0) {
              paste0(cor_name_base, " (Inglês)")
            } else if (lingua == 1) {
              paste0(cor_name_base, " (Espanhol)")
            } else {
              cor_name_base
            }
          } else {
            cor_name_base
          }

          itens_caderno <- itens_df[itens_df$CO_PROVA == codigo, ]

          if (tem_digital && (v_digital != "X")) {
            itens_caderno <- itens_caderno[itens_caderno$TP_VERSAO_DIGITAL == v_digital, ]
          }

          if (area_nome == "LC") {
            itens_caderno <- itens_caderno[
              is.na(TP_LINGUA) | TP_LINGUA == lingua,
            ]
            itens_caderno <- itens_caderno[order(CO_POSICAO), ]
          }

          if (nrow(itens_caderno) != 45) {
            if (tem_digital && (v_digital != "X") && (lingua != v_digital)) {
              next("Pulando carderno inválido (n != 45)\n  codigo=%s | area=%s | versao=%s | lingua=%s | n_itens=%s")
            } else {
              stop(
                sprintf(
                  "ERRO CRÍTICO: caderno inválido (n != 45)\n  codigo=%s | area=%s | versao=%s | lingua=%s | n_itens=%s",
                  codigo, area_nome, v_digital, lingua, nrow(itens_caderno)
                ),
                call. = FALSE
              )
            }
          }

          key_name <- paste(codigo, lingua, v_digital, sep = "_")

          itens_mirt <- data.frame(
            a1 = as.numeric(itens_caderno$NU_PARAM_A),
            d  = -as.numeric(itens_caderno$NU_PARAM_A) *
              as.numeric(itens_caderno$NU_PARAM_B),
            g  = as.numeric(itens_caderno$NU_PARAM_C)
          )

          mod_test <- mirtCAT::generate.mirt_object(itens_mirt, "3PL")
          escore <- mirt::expected.test(mod_test, Theta_metrico)

          den <- max(escore) - min(escore)
          if (den == 0) stop("Escore teórico constante", call. = FALSE)

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
              versao_digital = v_digital,
              b_medio_enem = round(
                mean(itens_caderno$NU_PARAM_B, na.rm = TRUE) *
                  const_row$k + const_row$d, 1
              )
            ),
            data_teorico  = round(as.vector(escore), 2),
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
  }

  writeLines("\n}", con)

  cli::cli_alert_success("Processamento completo (streaming JSON).")
}
