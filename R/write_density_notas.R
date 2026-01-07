#' Exportar coordenadas de densidade para Chart.js
#'
#' @param data Data.table com a coluna NU_NOTA_...
#' @param path_json Caminho do arquivo JSON.
#' @export
write_density_notas <- function(data, path_json) {
  cli::cli_h2("Processamento de Densidade (Chart.js)")

  col_nota <- grep("^NU_NOTA_", names(data), value = TRUE)
  notas <- data[[col_nota]]
  notas_filtrado <- notas[notas > 0 & !is.na(notas)]

  # Cálculo da densidade REAL
  dens <- density(notas_filtrado, n = 512,
                  from = min(notas_filtrado),
                  to = max(notas_filtrado))
  pontos_reais <- data.frame(x = dens$x, y = dens$y)

  # Cálculo da densidade NORMAL (Teórica)
  # Usamos a média e DP reais dos dados para criar a referência
  media_real <- mean(notas_filtrado)
  sd_real <- sd(notas_filtrado)

  # Geramos pontos da normal nos mesmos X da densidade
  y_normal <- dnorm(dens$x, mean = media_real, sd = sd_real)
  pontos_normal <- data.frame(x = dens$x, y = y_normal)

  # Monta a estrutura com DOIS datasets
  lista_completa <- list(
    datasets = list(
      list(
        id = "main-density",
        data = pontos_reais
      ),
      list(
        id = "normal-reference",
        data = pontos_normal
      )
    )
  )
  cli::cli_process_done()

  # --- TRATAMENTO DO PATH ---
  cli::cli_process_start("Preparando diretórios e exportação")
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "density.json")
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  # Exporta a lista completa
  jsonlite::write_json(lista_completa, path = final_file, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salvo com sucesso em: {.path {final_file}}")

  return(invisible(final_file))
}
