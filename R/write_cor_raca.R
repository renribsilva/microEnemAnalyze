#' @title Gera JSON sobre cor ou raça dos participantes do ENEM
#'
#' @description Esta função calcula frequências da variável cor ou raça do microdados do EENM
#' e exporta um JSON formatado para uso no Chart.js.
#'
#' @param data Um data.frame contendo a coluna TP_COR_RACA.
#' @param path_json Caminho da pasta onde o arquivo JSON será salvo.
#'
#' @return Salva um arquivo JSON no diretório especificado.
#' @export
write_cor_raca <- function(data, path_json) {

  # --- TÍTULO ---
  cli::cli_h2("Processamento de Dados: Cor ou Raça")

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
  cli::cli_process_start("Calculando frequências e estruturando Treemap")

  labels_cor_raca <- c(
    "Não declarado",
    "Branca",
    "Preta",
    "Parda",
    "Amarela",
    "Indígena")

  # Gerar as frequências
  cor_raca_abs <- table(data$TP_COR_RACA)
  cor_raca_rel <- prop.table(cor_raca_abs) * 100

  # Criar a estrutura para o Treemap (Array de Objetos)
  df_treemap <- data.frame(
    label = labels_cor_raca,
    value = as.numeric(round(cor_raca_rel, 2)),
    abs = as.numeric(cor_raca_abs)
  )

  objeto_cor_raca <- list(
    datasets = list(
      list(
        tree = df_treemap,
        key = "value",
        groups = list("label"),
        n = as.numeric(nrow(data))
      )
    )
  )
  cli::cli_process_done()

  # Exportação
  cli::cli_process_start("Exportando arquivo JSON")

  # --- TRATAMENTO DO PATH ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "cor_raca.json")

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  jsonlite::write_json(objeto_cor_raca, path = final_file, pretty = TRUE, auto_unbox = TRUE)

  cli::cli_process_done()
  cli::cli_alert_success("Arquivo salvo em: {.path {final_file}}")

  return(invisible(final_file))
}
