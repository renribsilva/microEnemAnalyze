#' @title Gerar JSON de Presença e Treineiros Filtrados
#'
#' @description Processa dados de presença, valida a integridade dos inscritos filtrados
#' e exporta os resultados para um arquivo JSON estruturado.
#'
#' @param data Uma data.table contendo os candidatos filtrados.
#' @param path_json O diretório ou caminho completo onde o arquivo JSON será salvo.
#'
#' @return Retorna o caminho do arquivo gerado (invisivelmente).
#' @import data.table
#' @export
write_presence <- function(data, path_json) {

  # data <- fread("exploration/2019/MICRODADOS/microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv", nrows = 100)

  # --- TÍTULO ---
  cli::cli_h2("Processamento de Presença e Treineiros Filtrados")

  # Validação básica
  cli::cli_process_start("Validando argumentos de entrada")
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  if (!is.character(path_json)) {
    cli::cli_alert_danger("Erro: {.var path_json} precisa ser character.")
    stop("Merda")
  }
  cli::cli_process_done()

  # Redução de dados
  cli::cli_process_start("Reduzindo dimensionalidade dos dados")
  batch_size <- 50000
  total_rows <- nrow(data)
  num_batches <- ceiling((total_rows/batch_size))

  data <- data.table::as.data.table(data)[, .(
    NU_INSCRICAO,
    IN_TREINEIRO,
    TP_PRESENCA_LC,
    TP_PRESENCA_CH,
    TP_PRESENCA_CN,
    TP_PRESENCA_MT
  )]

  cli::cli_process_done()

  presence_filtered <- data.table()

  # Filtração por Batches
  cp <- cli::cli_process_start("Filtrando presenças em {.val {num_batches}} batches")

  for (i in 1:num_batches) {
    start_row <- (i-1)*batch_size+1
    end_row <- min(i*batch_size, total_rows)
    dados_batch <- data[start_row:end_row]
    dados_batch_filtered <- dados_batch[
      (TP_PRESENCA_LC == 1 | TP_PRESENCA_CH == 1 | TP_PRESENCA_CN == 1 | TP_PRESENCA_MT == 1)
    ]
    presence_filtered <- rbindlist(list(presence_filtered, dados_batch_filtered))

    cli::cli_status_update(
      id = cp,
      msg = "Processando batch {i}/{num_batches} ({start_row} a {end_row})..."
    )

    rm(start_row, end_row, dados_batch, dados_batch_filtered)
    gc()
  }
  cli::cli_process_done(id = cp)

  # Cálculos e Proporções
  cli::cli_process_start("Calculando estatísticas e integridade")

  inscritos_filtered <- as.integer(0)

  if (!any(is.na(presence_filtered$NU_INSCRICAO))) {
    inscritos_filtered <- as.integer(length(presence_filtered$NU_INSCRICAO))
  } else {
    cli::cli_alert_danger("NA detectado em NU_INSCRICAO")
    stop("Merda")
  }

  treineiros_filtered <- table(presence_filtered$IN_TREINEIRO)
  treineiros_filtered_f <- prop.table(treineiros_filtered)

  tabela_treineiros_filtered <- t(as.data.table(rbind(treineiros_filtered, treineiros_filtered_f)))
  total_treineiros_filtered <- c(sum(tabela_treineiros_filtered[,1]), sum(tabela_treineiros_filtered[,2]))

  if (as.integer(inscritos_filtered) == as.integer(total_treineiros_filtered[1])) {
    tabela_treineiros_filtered <- rbind(tabela_treineiros_filtered, total_treineiros_filtered)
    tabela_treineiros_filtered[,2] <- round(tabela_treineiros_filtered[,2]*100, 2)
  } else {
    cli::cli_alert_danger("Inconsistência nos totais de presença")
    stop("Merda")
  }

  objeto_presenca <- list(
    list(
      grupo = "Presentes na prova*",
      total = as.numeric(tabela_treineiros_filtered[,1][3]),
      freq = as.numeric(tabela_treineiros_filtered[,2][3]),
      subRows = list(
        list(grupo = "Não treineiros*",
             total = as.numeric(tabela_treineiros_filtered[,1][1]),
             freq = as.numeric(tabela_treineiros_filtered[,2][1])
        ),
        list(grupo = "Treineiros",
             total = as.numeric(tabela_treineiros_filtered[,1][2]),
             freq = as.numeric(tabela_treineiros_filtered[,2][2]))
      )
    )
  )
  cli::cli_process_done()

  # Exportação
  cli::cli_process_start("Exportando arquivo JSON")

  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "presenca.json")

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  jsonlite::write_json(objeto_presenca, path = final_file, pretty = TRUE, auto_unbox = TRUE)

  cli::cli_process_done()
  cli::cli_alert_success("Processo concluído: {.path {final_file}}")

  return(invisible(final_file))
}
