#' @title Gera JSON sobre a distribuição das notas de Redação
#'
#' @param data Um data.table contendo a coluna TP_STATUS_REDACAO.
#' @param path_json Caminho da pasta ou arquivo onde o JSON será salvo.
#'
#' @export
write_status_redacao <- function(data, path_json) {
  cli::cli_h1("Processamento de Dados: Status")

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

  # --- VALIDAÇÃO ---
  if (!"NU_NOTA_REDACAO" %in% names(data)) {
    cli::cli_alert_danger("Coluna {.var NU_NOTA_REDACAO} nao encontrada.")
    stop("Execucao interrompida.")
  }

  # --- PROCESSAMENTO COM TABLE ---
  cli::cli_process_start("Calculando frequencias")

  # Gera a contagem de cada nota (0, 40, 80... 1000)
  tab_redacao <- table(data$TP_STATUS_REDACAO, useNA = "no")

  # Estrutura exata para o Chart.js
  objeto_redacao <- list(
    labels = names(tab_redacao),
    datasets = list(
      list(
        data = as.numeric(tab_redacao),
        n_total = sum(tab_redacao)
      )
    )
  )
  cli::cli_process_done()

  # --- EXPORTAÇÃO ---
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, "status_redacao.json")
  }
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  cli::cli_process_start("Exportando JSON para {.path {final_file}}")
  jsonlite::write_json(
    objeto_redacao,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  cli::cli_process_done()

  cli::cli_alert_success("Finalizado!")

  invisible(final_file)
}
