#' @title Filtrar inscritos com pelo menos uma presença
#'
#' @description Esta função processa os microdados do ENEM em batches para
#' filtrar candidatos que compareceram a pelo menos uma das quatro
#' provas do ENEM.
#'
#' @param data Um data.table com os microdados do ENEM.
#' @param path_csv Um string indicando o caminho para salvar o
#' arquivo CSV final.
#'
#' @export
filter_presence <- function(data, path_csv) {
  cli::cli_h1("Filtracao: Presenca Minima")

  cli::cli_process_start("Validando argumentos")

  if (missing(data)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg data} e obrigatorio.",
      "i" = "Por favor, forneca os microdados do ENEM."
    ))
  }

  if (missing(path_csv)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg path_csv} e obrigatorio.",
      "i" = "Por favor, forneca o caminho onde o csv sera gravado."
    ))
  }

  if (!is.character(path_csv)) {
    cli::cli_abort("{.arg path_csv} precisa ser do tipo character.")
  }

  # Normaliza os microdados
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  cli::cli_process_done()

  # Extrai o ano do exame
  ano <- data[1, ]$NU_ANO

  # Importa o dataset dicionário e filtra para tipo de prova == 1
  # (aplicação regular)
  dic_df <- get(paste0("dic_", ano))
  dic_df_p1 <- dic_df[dic_df$tipo == "1", ]

  # Extrai os códigos após o filtro
  cod_selected <- dic_df_p1$codigo

  cli::cli_process_start("Filtrando in-place (Otimizado)")

  # Constrói os nomes da coluna TP_PRESENCA_
  col_presenca_lc <- "TP_PRESENCA_LC"
  col_presenca_ch <- "TP_PRESENCA_CH"
  col_presenca_cn <- "TP_PRESENCA_CN"
  col_presenca_mt <- "TP_PRESENCA_MT"

  # Constrói os nome das coluns CO_PROVA_
  col_prova_lc <- "CO_PROVA_LC"
  col_prova_ch <- "CO_PROVA_CH"
  col_prova_cn <- "CO_PROVA_CN"
  col_prova_mt <- "CO_PROVA_MT"

  # Filtra os microdados para presença em pelo menos uma prova
  # na aplicação regular
  at_least_one_presence <- data[
    (get(col_presenca_cn) == 1 & get(col_prova_cn) %in% cod_selected) |
      (get(col_presenca_ch) == 1 & get(col_prova_ch) %in% cod_selected) |
      (get(col_presenca_lc) == 1 & get(col_prova_lc) %in% cod_selected) |
      (get(col_presenca_mt) == 1 & get(col_prova_mt) %in% cod_selected)
  ]

  cli::cli_process_done()

  cli::cli_process_start("Exportando arquivo CSV")

  # Constrói o caminho onde o csv deve ser gravado
  final_file <- if (grepl("\\.csv$", path_csv)) {
    path_csv
  } else {
    file.path(path_csv, "at_least_one_presence.csv")
  }
  final_file <- normalizePath(final_file, mustWork = FALSE)

  # Cria um diretório recursivamente se não existir
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  # Exporta um csv com os dados filtraados
  utils::write.csv(at_least_one_presence, file = final_file, row.names = FALSE)

  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salva em: {.path {final_file}}")

  invisible(at_least_one_presence)
}
