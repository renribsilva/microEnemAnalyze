#' Exportar Dados para JSON
#'
#' Esta função carrega um arquivo RDA e o converte para JSON.
#'
#' @param path_json String com o nome do arquivo de saída
#' @export
write_constantes <- function (path_json) {

  # --- TÍTULO ---
  cli::cli_h2("Exportação das Constantes do ENEM")

  # 1. Recuperar objetos
  cli::cli_process_start("Recuperando dados do Global Env")
  tryCatch({
    constantes_df <- get("constantes", envir = .GlobalEnv)
    cli::cli_process_done()
  }, error = function(e) {
    cli::cli_alert_danger("Erro: Objeto não encontrados no Global Env.")
    stop(e)
  })

  # --- TRATAMENTO DO PATH ---
  cli::cli_process_start("Preparando diretórios")
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "constantes.json")

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)
  cli::cli_process_done()

  # Exportação
  cli::cli_process_start("Exportando arquivo JSON")
  jsonlite::write_json(constantes_df, path = final_file, pretty = TRUE, auto_unbox = TRUE, dataframe = 'columns')
  cli::cli_process_done()

  cli::cli_alert_success("Processo concluído: {.path {final_file}}")

  return(invisible(final_file))
}
