#' @title Filtrar inscritos com pelo menos uma presença
#'
#' @description Esta função processa uma data.table em batches para filtrar candidatos que
#' compareceram a pelo menos uma das quatro provas do ENEM.
#'
#' @param data A data.table com os microdados.
#' @param path_csv Caminho para salvar o arquivo CSV final.
#'
#' @return Retorna a data.table filtrada invisivelmente.
#' @export
#'
#' @import data.table
#' @importFrom cli cli_abort cli_h2 cli_alert_info cli_alert_success
filter_presence <- function(data,
                            path_csv) {

  # Validação básica
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  if (!is.character(path_csv)) {
    cli::cli_abort("{.arg path_csv} precisa ser do tipo character.")
  }

  cli::cli_h2("Início da filtração para ao menos uma presença")
  cp <- cli::cli_process_start("Filtração para ao menos uma presença")

  batch_size <- 50000
  total_rows <- nrow(data)
  num_batches <- ceiling((total_rows/batch_size))

  at_least_one_presence <- data.table()

  for (i in 1:num_batches) {
    start_row <- (i-1)*batch_size+1
    end_row <- min(i*batch_size, total_rows)
    dados_batch <- data[start_row:end_row]
    dados_batch_filtered <- dados_batch[
      (TP_PRESENCA_LC == 1 | TP_PRESENCA_CH == 1 | TP_PRESENCA_CN == 1 | TP_PRESENCA_MT == 1)
    ]
    at_least_one_presence <- rbindlist(list(at_least_one_presence, dados_batch_filtered))
    cli::cli_status_update(
      id = cp,
      msg = "Processando batch {i}/{num_batches} ({start_row} a {end_row})..."
    )
    rm(start_row, end_row, dados_batch, dados_batch_filtered)
    gc()
  }

  cli::cli_process_done(id = cp)

  # Define o caminho final do arquivo de forma segura
  # Se path_csv já for um arquivo .csv, ele usa como está. Se for pasta, adiciona o nome.
  final_file <- if(grepl("\\.csv$", path_csv)) path_csv else file.path(path_csv, "at_least_one_presence.csv")

  # Garantir que a pasta de destino existe
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  cli::cli_alert_info("Exportando arquivo JSON...")
  utils::write.csv(at_least_one_presence, file = final_file, row.names = FALSE)
  cli::cli_process_done()
  cli::cli_alert_success("Arquivo salvo em: {.path {final_file}}")

  return(invisible(at_least_one_presence))
}
