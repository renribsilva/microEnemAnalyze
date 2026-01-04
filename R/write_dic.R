#' Exportar Dados para JSON
#'
#' Esta função carrega um arquivo RDA e o converte para JSON.
#'
#' @param path_json String com o nome do arquivo de saída
#' @param ano Ano do exame (por exemplo: 2019)
#' @export
write_dic <- function (path_json, ano) {

  # 1. Recuperar objetos
  tryCatch({
    dic_df <- get(paste0("dic_", as.character(ano)), envir = .GlobalEnv)
  }, error = function(e) {
    cli::cli_alert_danger("Erro: Objeto não encontrados no Global Env.")
    stop(e)
  })

  # --- TRATAMENTO DO PATH ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, paste0("dic_", ano,".json"))

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  cli::cli_alert_info("Exportando arquivo JSON...")
  jsonlite::write_json(dic_df, path = final_file, pretty = TRUE, auto_unbox = TRUE, dataframe = 'columns')
  cli::cli_alert_success("Arquivo salvo em: {.path {final_file}}")

  return(invisible(final_file))
}
