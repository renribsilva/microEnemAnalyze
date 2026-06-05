#' @title Escrever JSON com os dados de contantes.rda
#'
#' @description Esta função carrega um arquivo constantes.rda
#' (constantes de transformação da escala do ENEM) e o
#' converte para JSON.
#'
#' @param path_json String com o nome do arquivo de saída
#'
#' @export
write_constantes <- function(path_json) {
  # --- TÍTULO ---
  cli::cli_h1("Exportacao das Constantes do ENEM")

  # Importa constantes.rda
  cli::cli_process_start("Recuperando dados do Global Env")
  tryCatch(
    {
      constantes_df <- get("constantes", envir = .GlobalEnv)
      cli::cli_process_done()
    },
    error = function(e) {
      cli::cli_alert_danger("Erro: Objeto nao encontrados no Global Env.")
      stop(e)
    }
  )

  # --- TRATAMENTO DO PATH ---
  cli::cli_process_start("Exportando arquivo JSON")
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, "constantes.json")
  }

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  # Exportação
  jsonlite::write_json(
    constantes_df,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    dataframe = "columns"
  )
  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salva em: {.path {final_file}}")

  invisible(final_file)
}
