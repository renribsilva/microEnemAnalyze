#' Exportar Dados para JSON
#'
#' Esta função carrega um arquivo RDA e o converte para JSON.
#'
#' @param path_json String com o nome do arquivo de saída
#' @param ano Ano do exame (por exemplo: 2019)
#' @export
write_dic <- function(path_json, ano) {
  # --- TÍTULO ---
  cli::cli_h2("Exportacao de Dicionario de Cadernos - ENEM {ano}")

  # 1. Recuperar objetos
  cli::cli_process_start("Recuperando dados do Global Env")
  tryCatch(
    {
      dic_df <- get(paste0("dic_", as.character(ano)), envir = .GlobalEnv)
      cli::cli_process_done()
    },
    error = function(e) {
      cli::cli_alert_danger("Erro: Objeto nao encontrados no Global Env.")
      stop(e)
    }
  )

  # --- TRATAMENTO DO PATH ---
  cli::cli_process_start("Preparando diretorios")
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, paste0("dic_", ano, ".json"))
  }

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)
  cli::cli_process_done()

  # Exportação
  cli::cli_process_start("Exportando arquivo JSON")
  jsonlite::write_json(
    dic_df,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    dataframe = "columns"
  )
  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salvo com sucesso em: {.path {final_file}}")

  invisible(final_file)
}
