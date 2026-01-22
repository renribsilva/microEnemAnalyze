#' Exportar Top 2000 Scores Achatados com Ranking
#' @param data Um data.table processado (com MEDIA_GERAL e TX_RESPOSTAS)
#' @param path Caminho do arquivo JSON de saída
#' @import data.table jsonlite
write_mean_table <- function(data, path) {

  temp_dt <- data
  cols_notas <- c("NU_NOTA_LC", "NU_NOTA_CH", "NU_NOTA_CN", "NU_NOTA_MT", "NU_NOTA_REDACAO")

  # Verificação de integridade
  total_na_por_linha <- rowSums(is.na(temp_dt[, ..cols_notas]))
  if (any(total_na_por_linha == length(cols_notas))) {
    stop("Há participantes com NA em todas as áreas")
  }

  # Tratamento de NAs e Média
  cli::cli_alert_info("Tratando NAs e calculando médias...")
  setnafill(temp_dt, fill = 0, cols = cols_notas)
  temp_dt[, MEDIA_GERAL := rowMeans(.SD), .SDcols = cols_notas]

  # Ordenar e filtrar os top 2500
  top_dt <- head(temp_dt[order(-MEDIA_GERAL)], 2500)

  # --- ADICIONANDO A COLUNA DE RANKING ---
  # Como o DT já está ordenado, .I gera a sequência 1, 2, 3...
  top_dt[, RANKING := .I]

  areas <- c("LC", "CH", "CN", "MT")

  # 2. Loop de Processamento dos Scores com CLI
  for (a in areas) {
    res_col <- paste0("TX_RESPOSTAS_", a)
    gab_col <- paste0("TX_GABARITO_", a)
    score_col <- paste0("SCORE_", a)

    cli::cli_process_start("Processando scores da área: {.strong {a}}")

    top_dt[, (score_col) := mapply(function(r, g) {
      res_matriz <- process_score(r, g)
      res_limpo <- res_matriz[res_matriz != 9]
      res_limpo[res_limpo %in% c(7, 8)] <- 0
      return(paste0(res_limpo, collapse = ""))
    }, get(res_col), get(gab_col))]

    cli::cli_process_done()
  }

  # 3. Seleção de colunas específicas para o JSON (incluindo RANKING)
  cols_to_export <- c(
    "RANKING", # Coluna adicionada aqui
    grep("SCORE_", names(top_dt), value = TRUE),
    grep("CO_PROVA_", names(top_dt), value = TRUE),
    grep("NU_NOTA_", names(top_dt), value = TRUE),
    "TP_LINGUA",
    "MEDIA_GERAL"
  )

  # --- TRATAMENTO DO PATH E EXPORTAÇÃO ---
  final_file <- if(grepl("\\.json$", path)) path else file.path(path, "mean_table.json")
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  cli::cli_process_start("Gravando JSON em {.path {final_file}}")
  # Garante que as colunas exportadas sigam a ordem da lista cols_to_export
  jsonlite::write_json(top_dt[, ..cols_to_export], path = final_file, pretty = TRUE)
  cli::cli_process_done()

  cli::cli_alert_success("Processo concluído com sucesso!")

  return(invisible(top_dt))
}
