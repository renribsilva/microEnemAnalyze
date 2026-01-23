#' Exportar TCC Consolidado em Arquivo Único
#' @param data Data.table com colunas NU_NOTA_ de todas as áreas (Microdados)
#' @param score Lista de data.tables (um por área) com NU_SCORE e NU_NOTA_
#' @param path_json Caminho base ou nome do arquivo para salvar
#' @param ano Ano do exame
#' @export
write_tcc <- function(data, score, path_json, ano) {

  cli::cli_h2("Processamento Consolidado: TCC Teórico + Empírico")

  # 1. Recuperar objetos do Global Env
  cli::cli_process_start("Recuperando objetos do Global Env")
  tryCatch({
    itens_df <- get(paste0("itens_", as.character(ano)), envir = .GlobalEnv)
    dic_df   <- get(paste0("dic_", as.character(ano)), envir = .GlobalEnv)
    consts   <- get("constantes", envir = .GlobalEnv)
    cli::cli_process_done()
  }, error = function(e) {
    cli::cli_alert_danger("Erro: Objetos não encontrados no Global Env.")
    stop(e)
  })

  dic_df_P1 <- dic_df[dic_df$aplicacao == "P1", ]

  # --- INICIALIZAÇÃO DO STREAMING ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, paste0("tcc_", ano,".json"))
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  con <- file(final_file, open = "w")
  writeLines("{", con)

  previous_key <- NULL

  for (area_dt in score) {
    cols_notas_emp <- names(area_dt)[grepl("NU_NOTA_", names(area_dt))]
    if(length(cols_notas_emp) == 0) next
    area_nome <- gsub("NU_NOTA_", "", cols_notas_emp[1])

    cli::cli_alert_info("Processando área: {.val {area_nome}}")
    const_row <- consts[consts$area == area_nome, ]
    codigos <- unique(dic_df_P1$codigo[dic_df_P1$area == area_nome])

    for (codigo in codigos) {
      cor_name <- dic_df$cor[dic_df$codigo == codigo][1]
      col_nota <- paste0("NU_NOTA_", area_nome)

      notas_area_ref <- data[[col_nota]]
      notas_area_ref <- notas_area_ref[notas_area_ref > 0 & !is.na(notas_area_ref)]
      if(length(notas_area_ref) == 0) next

      nota_min <- min(notas_area_ref, na.rm = TRUE)
      nota_max <- max(notas_area_ref, na.rm = TRUE)
      escala_x <- as.numeric(seq(floor(nota_min), ceiling(nota_max), by = 1))

      tabela_real <- area_dt[get(col_nota) > 0, .(
        media = mean(NU_SCORE, na.rm = TRUE)
      ), keyby = .(x = as.integer(round(get(col_nota), 0)))]

      df_merge <- merge(data.frame(x = escala_x), tabela_real, by = "x", all.x = TRUE)
      Theta_metrico <- matrix((escala_x - const_row$d) / const_row$k)

      # --- LÓGICA DE VERSÕES (Digital vs Impresso) ---
      if (ano == 2020) {
        v_bruto <- unique(itens_df$TP_VERSAO_DIGITAL[itens_df$CO_PROVA == codigo])
        versoes_codificadas <- ifelse(is.na(v_bruto) | v_bruto == 0, "I", "D") |> unique()
      } else {
        versoes_codificadas <- "I" # Padrão para outros anos
      }

      for (v_tag in versoes_codificadas) {
        linguas <- if(area_nome == "LC") c(0, 1) else list(NULL)

        for (lingua in linguas) {
          # Filtro por versão para evitar somar 90 itens na TCC
          if (ano == 2020) {
            if (v_tag == "I") {
              itens_caderno <- itens_df[itens_df$CO_PROVA == codigo & (itens_df$TP_VERSAO_DIGITAL == 0 | is.na(itens_df$TP_VERSAO_DIGITAL)), ]
            } else {
              itens_caderno <- itens_df[itens_df$CO_PROVA == codigo & itens_df$TP_VERSAO_DIGITAL == 1, ]
            }
          } else {
            itens_caderno <- itens_df[itens_df$CO_PROVA == codigo, ]
          }

          if (nrow(itens_caderno) == 0) next

          # Identificação da Chave
          if (area_nome == "LC") {
            itens_caderno <- itens_caderno[order(itens_caderno$TP_LINGUA, itens_caderno$CO_POSICAO), ]
            condicao <- is.na(itens_caderno$TP_LINGUA) | itens_caderno$TP_LINGUA == "" | itens_caderno$TP_LINGUA == lingua
            itens_caderno <- itens_caderno[condicao, ]
            key_name <- paste0(codigo, "_", lingua, "_", v_tag)
          } else {
            key_name <- paste0(codigo, "_X_", v_tag)
          }

          if (nrow(itens_caderno) == 0) next

          # Cálculo MIRT (Baseado nos 45 itens da versão/língua)
          itens_mirt <- data.frame(
            a1 = as.numeric(itens_caderno$NU_PARAM_A),
            d  = as.numeric(itens_caderno$NU_PARAM_A) * -as.numeric(itens_caderno$NU_PARAM_B),
            g  = as.numeric(itens_caderno$NU_PARAM_C)
          )
          mod_test <- mirtCAT::generate.mirt_object(itens_mirt, '3PL')

          escore_esperado <- mirt::expected.test(mod_test, Theta_metrico)
          escore_esperado <- (escore_esperado - min(escore_esperado)) /
            (max(escore_esperado) - min(escore_esperado)) * nrow(itens_mirt)

          if (!is.null(previous_key)) {
            writeLines(",", con, sep = "\n")
          }

          tcc_item <- list(
            area = area_nome,
            labels_x = escala_x,
            metadata = list(
              codigo = as.numeric(codigo),
              versao = v_tag,
              area = area_nome,
              cor = cor_name,
              max = nota_max,
              min = nota_min,
              lingua = if(area_nome == "LC") as.numeric(lingua) else "N/A",
              b_medio_enem = round(mean(as.numeric(itens_caderno$NU_PARAM_B), na.rm = TRUE) * const_row$k + const_row$d, 1)
            ),
            data_teorico = round(as.vector(escore_esperado), 2),
            data_empirico = round(df_merge$media, 2)
          )

          json_string <- jsonlite::toJSON(tcc_item, pretty = TRUE, auto_unbox = TRUE, na = "null")
          cat(paste0("\"", key_name, "\": ", json_string), file = con)
          previous_key <- key_name
        }
      }
    }
  }

  writeLines("", con)
  writeLines("}", con)
  close(con)
  cli::cli_alert_success("Processamento completo para {ano}. JSON salvo via streaming.")
}
