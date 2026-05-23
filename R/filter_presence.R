#' @title Filtrar inscritos com pelo menos uma presença
#'
#' @description Esta função processa uma data.table em batches para
#' filtrar candidatos que compareceram a pelo menos uma das quatro
#' provas do ENEM.
#'
#' @param data A data.table com os microdados.
#' @param path_csv Caminho para salvar o arquivo CSV final.
#' treineiros
#' @return Retorna a data.table filtrada invisivelmente.
#' @export
filter_presence <- function(data, path_csv) {
  # --- TÍTULO ---
  cli::cli_h2("Filtracao: Presenca Minima")

  # Validação básica
  cli::cli_process_start("Validando argumentos")

  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  if (!is.character(path_csv)) {
    cli::cli_abort("{.arg path_csv} precisa ser do tipo character.")
  }

  cli::cli_process_done()

  ano <- data[1, ]$NU_ANO
  dic_df <- get(paste0("dic_", ano))
  dic_df_p1 <- dic_df[dic_df$tipo == "1", ]
  cod_selected <- dic_df_p1$codigo

  cli::cli_process_start("Filtrando in-place (Otimizado)")

  col_presenca_lc <- "TP_PRESENCA_LC"
  col_presenca_ch <- "TP_PRESENCA_CH"
  col_presenca_cn <- "TP_PRESENCA_CN"
  col_presenca_mt <- "TP_PRESENCA_MT"

  col_prova_lc <- "CO_PROVA_LC"
  col_prova_ch <- "CO_PROVA_CH"
  col_prova_cn <- "CO_PROVA_CN"
  col_prova_mt <- "CO_PROVA_MT"

  at_least_one_presence <- data[
    (get(col_presenca_cn) == 1 & get(col_prova_cn) %in% cod_selected) |
      (get(col_presenca_ch) == 1 & get(col_prova_ch) %in% cod_selected) |
      (get(col_presenca_lc) == 1 & get(col_prova_lc) %in% cod_selected) |
      (get(col_presenca_mt) == 1 & get(col_prova_mt) %in% cod_selected)
  ]

  cli::cli_process_done()

  # Exportação
  cli::cli_process_start("Exportando arquivo CSV")

  final_file <- if (grepl("\\.csv$", path_csv)) {
    path_csv
  } else {
    file.path(path_csv, "at_least_one_presence.csv")
  }

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  utils::write.csv(at_least_one_presence, file = final_file, row.names = FALSE)

  cli::cli_process_done()

  cli::cli_alert_success("Processo concluido: {.path {final_file}}")

  invisible(at_least_one_presence)
}
