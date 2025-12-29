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
#' @export
#' @import data.table
#' @importFrom jsonlite write_json
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_process_start cli_process_done
write_presence_day <- function(data, path_json, day) {

  # Validação básica
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

  cli::cli_alert_info("Reduzinho o tamanho dos dados: aguarde...")

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

  presence_filtered <- data.table()

  cli::cli_h2("Início da filtração das presenças")
  cp <- cli::cli_process_start("Filtragem das presenças")

  for (i in 1:num_batches) {
    start_row <- (i-1)*batch_size+1
    end_row <- min(i*batch_size, total_rows)
    dados_batch <- data[start_row:end_row]
    if (as.integer(day) == 1L) {
      dados_batch_filtered <- dados_batch[TP_PRESENCA_LC == 1 | TP_PRESENCA_CH == 1]
    } else if (as.integer(day) == 2L) {
      dados_batch_filtered <- dados_batch[TP_PRESENCA_CN == 1 | TP_PRESENCA_MT == 1]
    }
    presence_filtered <- rbindlist(list(presence_filtered, dados_batch_filtered))
    cli::cli_status_update(
      id = cp,
      msg = "Processando batch {i}/{num_batches} ({start_row} a {end_row})..."
    )
    rm(start_row, end_row, dados_batch, dados_batch_filtered)
    # gc()
  }

  cli::cli_process_done(id = cp)

  # 1. Validação de inscritos
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

  cli::cli_h2("Inicia a criação de tabelas de frequências")
  ap <- cli::cli_process_start("Criação de tabelas de frequências")

  objeto_presence_filtered <- list(
    list(
      grupo = "Presentes na prova",
      total = inscritos_filtered,
      abst = round(((inscritos-inscritos_filtered)/inscritos)*100, 2)
    )
  )

  cli::cli_process_done(id = ap)

  # --- TRATAMENTO DO PATH ---
  if (as.integer(day) == 1L) {
    final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "presenca_dia1.json")
  } else if (as.integer(day) == 2L) {
    final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "presenca_dia2.json")
  }

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  cli::cli_alert_info("Exportando arquivo JSON...")
  jsonlite::write_json(objeto_presence_filtered, path = final_file, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_alert_success("Arquivo de presença salvo: {.path {final_file}}")

  return(invisible(final_file))
}
