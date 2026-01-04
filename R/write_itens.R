#' Exportar Dados para JSON
#'
#' Esta função carrega um arquivo RDA e o converte para JSON.
#'
#' @param path_json String com o nome do arquivo de saída
#' @export
write_itens <- function (path_json, ano) {

  # 1. Recuperar objetos
  tryCatch({
    itens_df <- get(paste0("itens_", as.character(ano)), envir = .GlobalEnv)
  }, error = function(e) {
    cli::cli_alert_danger("Erro: Objeto não encontrados no Global Env.")
    stop(e)
  })

  # --- TRATAMENTO DO PATH ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, paste0("itens_", ano,".json"))

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  cli::cli_alert_info("Exportando arquivo JSON...")
  jsonlite::write_json(itens_df, path = final_file, pretty = TRUE, auto_unbox = TRUE, dataframe = 'columns')
  cli::cli_alert_success("Arquivo salvo em: {.path {final_file}}")

  return(invisible(final_file))
}
