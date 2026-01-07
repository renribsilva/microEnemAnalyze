#' Exportar Dados para JSON (Garantindo UTF-8 e Tipos Originais)
#'
#' @param path_json String com o caminho da pasta ou nome do arquivo de saída
#' @param ano Inteiro ou string representando o ano (ex: 2019)
#' @export
write_itens <- function (path_json, ano) {

  cli::cli_h2("Exportação de Itens (Dicionário de Parâmetros) - ENEM {ano}")

  # 1. Recuperar objeto
  obj_name <- paste0("itens_", as.character(ano))
  if (!exists(obj_name, envir = .GlobalEnv)) {
    cli::cli_alert_danger("Erro: Objeto {.val {obj_name}} não encontrado.")
    stop("Objeto inexistente.")
  }

  itens_df <- as.data.frame(get(obj_name, envir = .GlobalEnv))

  # 2. Limpeza de Encoding (Apenas em colunas de texto)
  cli::cli_process_start("Limpando caracteres inválidos (UTF-8)")

  itens_df[] <- lapply(itens_df, function(x) {
    if (is.character(x) || is.factor(x)) {
      x <- as.character(x)
      # Corrige erros como "ergncia" convertendo de latin1 e limpando bytes órfãos
      x <- iconv(x, from = "latin1", to = "UTF-8", sub = "")
      x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
      return(x)
    }
    # Se for numérico, retorna intacto (mantém os NAs originais como NA)
    return(x)
  })
  cli::cli_process_done()

  # 3. Tratamento do Path
  final_file <- if(grepl("\\.json$", path_json, ignore.case = TRUE)) {
    path_json
  } else {
    file.path(path_json, paste0("itens_", ano, ".json"))
  }
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  # 4. Exportação
  cli::cli_process_start("Gravando JSON")

  # na = "null": garante que NAs numéricos virem null (sem aspas)
  # e NAs de texto virem null (sem aspas) no JSON.
  json_string <- jsonlite::toJSON(
    itens_df,
    pretty = TRUE,
    auto_unbox = TRUE,
    dataframe = 'columns',
    na = "null"
  )

  writeLines(json_string, final_file, useBytes = FALSE)

  cli::cli_process_done()
  cli::cli_alert_success("Sucesso! Arquivo pronto para o Next.js: {.path {final_file}}")

  return(invisible(final_file))
}
