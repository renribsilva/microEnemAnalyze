#' @title Filtrar inscritos com pelo menos uma presença
#'
#' @description Esta função processa uma data.table em batches para
#' filtrar candidatos que compareceram a pelo menos uma das quatro
#' provas do ENEM.
#'
#' @param data A data.table com os microdados.
#' @param path_csv Caminho para salvar o arquivo CSV final.
#' @param nt Argumento booleano que acrescenta um filtro para não
#' treineiros
#'
#' @return Retorna a data.table filtrada invisivelmente.
#' @export
filter_presence <- function(data,
                            path_csv,
                            nt = FALSE) {

  # --- TÍTULO ---
  cli::cli_h2(if(nt) "Filtração: Presença Mínima (Não Treineiros)" else "Filtração: Presença Mínima")

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

  ano <- data[1,]$NU_ANO
  dic_df   <- get(paste0("dic_", ano),   envir = .GlobalEnv)
  cod_selected <- dic_df$codigo

  # Processamento de Batches
  batch_size <- 50000
  total_rows <- nrow(data)
  num_batches <- ceiling((total_rows/batch_size))

  cp <- cli::cli_process_start("Filtrando dados em {.val {num_batches}} batches")

  at_least_one_presence <- data.table()

  for (i in 1:num_batches) {
    start_row <- (i-1)*batch_size+1
    end_row <- min(i*batch_size, total_rows)

    dados_batch_filtered <- data[start_row:end_row][
      (TP_PRESENCA_CN == 1 & CO_PROVA_CN %in% cod_selected) |
        (TP_PRESENCA_CH == 1 & CO_PROVA_CH %in% cod_selected) |
        (TP_PRESENCA_LC == 1 & CO_PROVA_LC %in% cod_selected) |
        (TP_PRESENCA_MT == 1 & CO_PROVA_MT %in% cod_selected)
    ]

    if (nt) {
      dados_batch_filtered <- dados_batch_filtered[IN_TREINEIRO == 0]
    }

    at_least_one_presence <- rbindlist(list(at_least_one_presence, dados_batch_filtered))

    cli::cli_status_update(
      id = cp,
      msg = "Processando batch {i}/{num_batches} ({start_row} a {end_row})..."
    )

    # Removendo apenas o que realmente existe nesta iteração
    rm(dados_batch_filtered)

    # Dica: rode o gc() apenas a cada 10 ou 20 batches para não perder performance
    if (i %% 10 == 0) gc()
  }

  cli::cli_process_done(id = cp)

  # Exportação
  cli::cli_process_start("Exportando arquivo CSV")

  if (nt == TRUE) {
    final_file <- if(grepl("\\.csv$", path_csv)) path_csv else file.path(path_csv, "at_least_one_presence_nt.csv")
  } else {
    final_file <- if(grepl("\\.csv$", path_csv)) path_csv else file.path(path_csv, "at_least_one_presence.csv")
  }

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  utils::write.csv(at_least_one_presence, file = final_file, row.names = FALSE)

  cli::cli_process_done()

  cli::cli_alert_success("Processo concluído: {.path {final_file}}")

  return(invisible(at_least_one_presence))
}
