#' Exportar TCCs de todos os cadernos em um único JSON
#'
#' @param data Data.table com a coluna NU_NOTA
#' @param path_json String. Caminho completo do arquivo (ex: "caminho/describe.json").
#' @export
write_notas <- function(data, path_json) {

  # --- TÍTULO ---
  cli::cli_h2("Exportação de Vetor de Notas")

  # Processamento
  cli::cli_process_start("Filtrando e preparando notas")
  col_nota <- grep("^NU_NOTA_", names(data), value = TRUE)
  notas <- data[[col_nota]]
  notas_filtrado <- notas[notas > 0 & !is.na(notas)]
  cli::cli_process_done()

  # --- TRATAMENTO DO PATH ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "notas.json")

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  # Exportação
  cli::cli_process_start("Exportando arquivo JSON")
  jsonlite::write_json(notas_filtrado, path = final_file, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_process_done()

  cli::cli_alert_success("Processo concluído: {.path {final_file}}")

  return(invisible(final_file))
}
