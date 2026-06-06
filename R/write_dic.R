#' @title Exportar Dados para JSON
#'
#' @description Esta função carrega um arquivo RDA e o converte para JSON.
#'
#' @param path_json String com o nome do arquivo de saída
#' @param ano Ano do exame (por exemplo: 2019)
#'
#' @export
write_dic <- function(path_json, ano) {
  # --- TÍTULO ---
  cli::cli_h1("Exportacao de Dicionario de Cadernos - ENEM {ano}")

  cli::cli_process_start("Validando argumentos")

  if (missing(path_json)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg path_csv} e obrigatorio.",
      "i" = "Por favor, forneca o caminho onde o csv sera gravado."
    ))
  }

  if (missing(ano)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg ano} e obrigatorio.",
      "i" = "Informe o ano de referencia da aplicacao (ex: {.val 2019})."
    ))
  }

  if (!is.character(path_json)) {
    cli::cli_abort(
      "{.arg area} precisa ser do tipo {.cls character}."
    )
  }

  if (!(is.numeric(ano) || is.integer(ano))) {
    cli::cli_abort(
      "{.arg ano} precisa ser do tipo {.cls number} ou {.cls integer}."
    )
  }

  cli::cli_process_done()

  # Importa dicionário de provas do ano determinado
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
  cli::cli_process_start("Exportando arquivo JSON")
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, paste0("dic_", ano, ".json"))
  }

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  # Exportação
  jsonlite::write_json(
    dic_df,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    dataframe = "columns"
  )
  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salvo em: {.path {final_file}}")

  invisible(final_file)
}
