#' Exportar Estatísticas das Notas do ENEM
#' @param data Um data.table bruto
#' @param path Caminho do arquivo JSON ou diretório
#' @import data.table jsonlite psych cli
write_mean_describe <- function(data, path) {

  temp_dt <- copy(data)
  cols_notas <- c("NU_NOTA_LC", "NU_NOTA_CH", "NU_NOTA_CN", "NU_NOTA_MT", "NU_NOTA_REDACAO")

  # Verificação de integridade
  total_na_por_linha <- rowSums(is.na(temp_dt[, ..cols_notas]))
  if (any(total_na_por_linha == length(cols_notas))) {
    stop("Há participantes com NA em todas as áreas")
  }

  # Tratamento de NAs e Média
  setnafill(temp_dt, fill = 0, cols = cols_notas)
  temp_dt[, MEDIA_GERAL := rowMeans(.SD), .SDcols = cols_notas]

  # Gerar Estatísticas base
  stats_desc <- psych::describe(temp_dt$MEDIA_GERAL)
  notas_rank <- sort(temp_dt$MEDIA_GERAL, decreasing = TRUE)

  # --- CÁLCULO DA CURVA DE DENSIDADE LIMITADA ---
  # Usamos stats_desc$min e stats_desc$max para truncar a estimativa
  dens <- density(
    temp_dt$MEDIA_GERAL,
    na.rm = TRUE,
    from = stats_desc$min,
    to = stats_desc$max
  )

  output <- list(
    total_candidates = stats_desc$n,
    metrics = list(
      mean = stats_desc$mean,
      min = stats_desc$min, # Adicionado para conferência no JSON
      max = stats_desc$max,
      p99 = as.numeric(quantile(temp_dt$MEDIA_GERAL, 0.99)),
      nota_2000 = notas_rank[2000]
    ),
    density_curve = list(
      x = dens$x,
      y = dens$y
    )
  )

  # --- TRATAMENTO DO PATH ---
  cli::cli_process_start("Exportando arquivo JSON")

  final_file <- if(grepl("\\.json$", path)) path else file.path(path, "mean_describe.json")
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  jsonlite::write_json(output, path = final_file, pretty = TRUE, auto_unbox = TRUE)

  cli::cli_process_done()
  cli::cli_alert_success("Processo concluído: {.path {final_file}}")

  return(invisible(temp_dt))
}
