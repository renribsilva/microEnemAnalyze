#' @title Gera JSON sobre faixa etária dos participantes do ENEM
#'
#' @description Esta função agrupa os dados de faixa etária do
#' ENEM, calcula frequências relativas e exporta um JSON
#' formatado para uso no Chart.js.
#'
#' @param data Um data.table contendo a coluna TP_FAIXA_ETARIA.
#' @param path_json Caminho da pasta onde o arquivo JSON será salvo.
#'
#' @export
write_fx_etaria <- function(data, path_json) {
  # --- TÍTULO DO PROCESSO ---
  cli::cli_h1("Processamento de Dados: Faixa Etaria")

  cli::cli_process_start("Validando argumentos")

  if (missing(data)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg data} e obrigatorio.",
      "i" = "Por favor, forneca os microdados do ENEM."
    ))
  }

  if (missing(path_json)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg path_csv} e obrigatorio.",
      "i" = "Por favor, forneca o caminho onde o csv sera gravado."
    ))
  }

  if (!is.character(path_json)) {
    cli::cli_abort("{.arg path_csv} precisa ser do tipo character.")
  }

  # Normaliza os microdados
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  cli::cli_process_done()

  # --- PROCESSAMENTO ---
  cli::cli_process_start("Calculando frequencias e agrupando faixas")

  labels_etaria <- c(
    "Menor de 20 anos",
    "20-25 anos",
    "26-30 anos",
    "31-35 anos",
    "36-40 anos",
    "41-45 anos",
    "46-50 anos",
    "51-55 anos",
    "56-60 anos",
    "Maior de 60 anos"
  )

  fx_etaria_abs <- table(data$TP_FAIXA_ETARIA)
  fx_etaria_abs_grouped <- c(
    "1-4" = sum(fx_etaria_abs[1:4]),
    "5-10" = sum(fx_etaria_abs[5:10]),
    fx_etaria_abs[11:17],
    "18-20" = sum(fx_etaria_abs[18:20])
  )

  fx_etaria_rel_grouped <- prop.table(fx_etaria_abs_grouped) * 100

  objeto_etaria <- list(
    labels = labels_etaria,
    datasets = list(
      list(
        data = as.numeric(round(fx_etaria_rel_grouped, 2)),
        abs_values = as.numeric(fx_etaria_abs_grouped),
        n = as.numeric(nrow(data))
      )
    )
  )
  cli::cli_process_done()

  # --- EXPORTAÇÃO ---
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, "faixa_etaria.json")
  }
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  cli::cli_process_start("Exportando arquivo JSON para {.path {final_file}}")
  jsonlite::write_json(
    objeto_etaria,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  cli::cli_process_done()

  cli::cli_alert_success("Finalizado com sucesso!")

  invisible(final_file)
}
