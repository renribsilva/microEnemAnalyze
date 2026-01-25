#' @title Gerar JSON de Presença e Treineiros Filtrados por dia
#'
#' @description Processa dados de presença, valida a integridade dos inscritos filtrados
#' e exporta os resultados para um arquivo JSON estruturado.
#'
#' @param data Uma data.table contendo os candidatos filtrados.
#' @param path_json O diretório ou caminho completo onde o arquivo JSON será salvo.
#' @param day Dia de realização da prova: 1 ou 2 (numeric ou double)
#'
#' @return Retorna o caminho do arquivo gerado (invisivelmente).
#' @import data.table
#' @export
write_presence_day <- function(data, path_json, day) {

  # --- TÍTULO ---
  cli::cli_h2("Processamento de Presença: Dia {day}")

  # Validação básica
  cli::cli_process_start("Validando parâmetros e estrutura")
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  if (!is.character(path_json)) {
    cli::cli_abort("Erro: {.var path_json} precisa ser character.")
  }

  if (!typeof(day) %in% c("double", "integer") || !as.integer(day) %in% c(1, 2)) {
    cli::cli_abort("Erro: {.var day} precisa ser 1 ou 2 (numeric ou integer).")
  }
  cli::cli_process_done()

  cli::cli_process_start("Reduzindo dimensionalidade dos dados")
  data <- data.table::as.data.table(data)[, .(
    NU_ANO,
    NU_INSCRICAO,
    IN_TREINEIRO,
    TP_PRESENCA_LC,
    TP_PRESENCA_CH,
    TP_PRESENCA_CN,
    TP_PRESENCA_MT,
    CO_PROVA_LC,
    CO_PROVA_CH,
    CO_PROVA_CN,
    CO_PROVA_MT
  )]
  cli::cli_process_done()

  # Preparação dos dados
  cli::cli_process_start("Preparando batches")

  batch_size <- 50000
  total_rows <- nrow(data)
  num_batches <- ceiling((total_rows/batch_size))

  ano <- data[1,]$NU_ANO
  dic_df   <- get(paste0("dic_", ano),   envir = .GlobalEnv)
  cod_selected <- dic_df$codigo

  if (as.integer(day) == 1L) {
    data_filtered_by_cod <- data[CO_PROVA_LC %in% cod_selected | CO_PROVA_CH %in% cod_selected]
  } else {
    data_filtered_by_cod <- data[CO_PROVA_CN %in% cod_selected | CO_PROVA_MT %in% cod_selected]
  }

  presence_filtered <- data.table()

  cli::cli_process_done()

  # Filtração por Batches
  cp <- cli::cli_process_start("Filtrando presenças por dia")
  for (i in 1:num_batches) {
    start_row <- (i-1)*batch_size+1
    end_row <- min(i*batch_size, total_rows)

    if (as.integer(day) == 1L) {
      dados_batch_filtered <- data[start_row:end_row][
        (TP_PRESENCA_LC == 1 & CO_PROVA_LC %in% cod_selected) |
        (TP_PRESENCA_CH == 1 & CO_PROVA_CH %in% cod_selected)
      ]
    } else if (as.integer(day) == 2L) {
      dados_batch_filtered <- data[start_row:end_row][
        (TP_PRESENCA_CN == 1 & CO_PROVA_CN %in% cod_selected) |
        (TP_PRESENCA_MT == 1 & CO_PROVA_MT %in% cod_selected)
      ]
    }

    presence_filtered <- rbindlist(list(presence_filtered, dados_batch_filtered))

    cli::cli_status_update(
      id = cp,
      msg = "Processando batch {i}/{num_batches} ({start_row} a {end_row})..."
    )
    rm(dados_batch_filtered)

    # Dica: rode o gc() apenas a cada 10 ou 20 batches para não perder performance
    if (i %% 10 == 0) gc()
  }
  cli::cli_process_done(id = cp)

  # Validação e Frequências
  ap <- cli::cli_process_start("Calculando frequências e validação de integridade")

  if (any(is.na(data$NU_INSCRICAO))) {
    cli::cli_alert_danger("Valores ausentes detectados em {.var NU_INSCRICAO}")
    stop("Erro: Existem valores NA em NU_INSCRICAO.")
  }

  inscritos <- as.integer(length(data$NU_INSCRICAO))

  if (!any(is.na(presence_filtered$NU_INSCRICAO))) {
    inscritos_filtered <- as.integer(length(presence_filtered$NU_INSCRICAO))
  } else {
    stop("Merda")
  }

  objeto_presence_filtered <- list(
    list(
      grupo = "Presentes na prova",
      total = inscritos_filtered,
      abst = round(((inscritos-inscritos_filtered)/inscritos)*100, 2)
    )
  )
  cli::cli_process_done(id = ap)

  # Exportação
  cli::cli_process_start("Exportando arquivo JSON")
  if (as.integer(day) == 1L) {
    final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "presenca_dia1.json")
  } else if (as.integer(day) == 2L) {
    final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "presenca_dia2.json")
  }

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  jsonlite::write_json(objeto_presence_filtered, path = final_file, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_process_done()

  cli::cli_alert_success("Processo concluído: {.path {final_file}}")

  return(invisible(final_file))
}
