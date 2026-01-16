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

  # --- INICIALIZAÇÃO DO ARQUIVO ÚNICO ---
  # tcc_consolidated acumulará todos os códigos de todas as áreas
  tcc_consolidated <- list()

  # 2. Loop sobre a lista de áreas
  for (area_dt in score) {

    # Identificar área
    cols_notas_emp <- names(area_dt)[grepl("NU_NOTA_", names(area_dt))]
    if(length(cols_notas_emp) == 0) next
    area_nome <- gsub("NU_NOTA_", "", cols_notas_emp[1])

    cli::cli_alert_info("Processando área: {.val {area_nome}}")
    const_row <- consts[consts$area == area_nome, ]
    codigos <- unique(dic_df_P1$codigo[dic_df_P1$area == area_nome])

    for (codigo in codigos) {
      cor_name <- dic_df$cor[dic_df$codigo == codigo][1]
      col_nota <- paste0("NU_NOTA_", area_nome)

      # --- ESCALA DE INTEIROS ---
      notas_area_ref <- data[[col_nota]]
      notas_area_ref <- notas_area_ref[notas_area_ref > 0 & !is.na(notas_area_ref)]
      if(length(notas_area_ref) == 0) next

      nota_min <- min(notas_area_ref, na.rm = TRUE)
      nota_max <- max(notas_area_ref, na.rm = TRUE)
      escala_x <- as.numeric(seq(floor(nota_min), ceiling(nota_max), by = 1))

      # --- LÓGICA EMPÍRICA ---
      tabela_real <- area_dt[get(col_nota) > 0, .(
        media = mean(NU_SCORE, na.rm = TRUE)
      ), keyby = .(x = as.integer(round(get(col_nota), 0)))]

      df_merge <- merge(data.frame(x = escala_x), tabela_real, by = "x", all.x = TRUE)

      # --- LÓGICA TEÓRICA ---
      Theta_metrico <- matrix((escala_x - const_row$d) / const_row$k)
      linguas <- if(area_nome == "LC") c(0, 1) else list(NULL)

      for (lingua in linguas) {
        itens_caderno <- itens_df[itens_df$CO_PROVA == codigo, ]
        key_name <- as.character(codigo)

        if (area_nome == "LC") {
          itens_caderno <- itens_caderno[order(itens_caderno$TP_LINGUA, itens_caderno$CO_POSICAO), ]
          condicao <- is.na(itens_caderno$TP_LINGUA) | itens_caderno$TP_LINGUA == "" | itens_caderno$TP_LINGUA == lingua
          itens_caderno <- itens_caderno[condicao, ]
          key_name <- paste0(codigo, "_", lingua)
        }

        # Cálculo MIRT
        itens_mirt <- data.frame(
          a1 = as.numeric(itens_caderno$NU_PARAM_A),
          d  = as.numeric(itens_caderno$NU_PARAM_A) * -as.numeric(itens_caderno$NU_PARAM_B),
          g  = as.numeric(itens_caderno$NU_PARAM_C)
        )
        mod_test <- mirtCAT::generate.mirt_object(itens_mirt, '3PL')

        escore_esperado <- mirt::expected.test(mod_test, Theta_metrico)
        escore_esperado <- (escore_esperado - min(escore_esperado)) /
          (max(escore_esperado) - min(escore_esperado)) * nrow(itens_mirt)

        # --- ALIMENTANDO A LISTA ÚNICA ---
        # Chave principal: key_name | Chave interna: area
        tcc_consolidated[[key_name]] <- list(
          area = area_nome,
          labels_x = escala_x,
          metadata = list(
            codigo = as.numeric(codigo),
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
      }
    }
  }

  # --- EXPORTAÇÃO FINAL DO ARQUIVO ÚNICO ---
  cli::cli_process_start("Preparando diretórios")
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, paste0("tcc_", ano,".json"))
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  cli::cli_process_start("Exportando JSON Único: {.path {basename(final_file)}}")
  jsonlite::write_json(tcc_consolidated, path = final_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
  cli::cli_process_done()

  cli::cli_alert_success("Processamento completo. Todos os códigos salvos em um único arquivo.")
}
