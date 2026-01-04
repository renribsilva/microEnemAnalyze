#' @title Gera JSON sobre sexo dos participantes do ENEM
#'
#' @description Esta função calcula frequências da variável
#' sexo do microdados do ENEM e exporta um JSON formatado
#' para uso no Chart.js.
#'
#' @param data Um data.frame contendo a coluna TP_SEXO.
#' @param path_json Caminho da pasta onde o arquivo JSON será salvo.
#'
#' @return Salva um arquivo JSON no diretório especificado.
#' @export
write_sexo <- function(data, path_json) {

  # --- TÍTULO ---
  cli::cli_h2("Processamento de Dados: Sexo")

  # Validação básica
  cli::cli_process_start("Validando estrutura dos dados")
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  if (!is.character(path_json)) {
    cli::cli_alert_danger("Erro: {.var path_json} precisa ser character.")
    stop("Merda")
  }
  cli::cli_process_done()

  # Processamento
  cli::cli_process_start("Calculando frequências de sexo")

  labels_sexo <- c(
    "Feminino",
    "Masculino")

  # Gerar as frequências (Mantendo sua referência à variável filtered)
  sexo_abs <- table(data$TP_SEXO)
  sexo_rel <- prop.table(sexo_abs) * 100

  # Criar a estrutura para o Chart.js
  objeto_sexo <- list(
    labels = labels_sexo,
    datasets = list(
      list(
        data = as.numeric(round(sexo_rel, 2)),
        abs_values = as.numeric(sexo_abs),
        n = as.numeric(nrow(data))
      )
    )
  )
  cli::cli_process_done()

  # Exportação
  cli::cli_process_start("Exportando arquivo JSON")

  # --- TRATAMENTO DO PATH ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "sexo.json")

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  jsonlite::write_json(objeto_sexo, path = final_file, pretty = TRUE, auto_unbox = TRUE)

  cli::cli_process_done()
  cli::cli_alert_success("Processo concluído: {.path {final_file}}")

  return(invisible(final_file))
}
