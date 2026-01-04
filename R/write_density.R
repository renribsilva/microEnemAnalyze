#' Exportar coordenadas de densidade para Chart.js
#'
#' @param data Data.table com a coluna NU_NOTA_...
#' @param path_json Caminho do arquivo JSON.
#' @export
write_density <- function(data, path_json) {

  # --- TÍTULO ---
  cli::cli_h2("Processamento de Densidade (Chart.js)")

  # Cálculo da densidade
  cli::cli_process_start("Calculando coordenadas de densidade")
  col_nota <- grep("^NU_NOTA_", names(data), value = TRUE)
  notas <- data[[col_nota]]
  notas_filtrado <- notas[notas > 0 & !is.na(notas)]

  dens <- density(notas_filtrado, n = 512)

  # 1. Cria o data.frame com os pontos (cada linha vira um objeto {x, y})
  pontos <- data.frame(x = dens$x, y = dens$y)

  # 2. Monta a estrutura exata pedida pelo Chart.js
  lista_completa <- list(
    datasets = list(
      list(
        data = pontos
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
