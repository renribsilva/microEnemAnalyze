#' @title Gera JSON sobre faixa etária dos participantes do ENEM
#'
#' @description Esta função agrupa os dados de faixa etária do ENEM/INEP, calcula frequências
#' relativas e exporta um JSON formatado para uso no Chart.js.
#'
#' @param data Um data.frame contendo a coluna TP_FAIXA_ETARIA.
#' @param path_json Caminho da pasta onde o arquivo JSON será salvo.
#'
#' @return Salva um arquivo JSON no diretório especificado.
#' @export
#' @importFrom jsonlite write_json
#'
write_fx_etaria <- function(data, path_json) {

  # Validação básica
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  if (!is.character(path_json)) {
    cli::cli_alert_danger("Erro: {.var path_json} precisa ser character.")
    stop("Merda")
  }

  labels_etaria <- c(
    "Menor de 20 anos", # 1 a 4
    "20-25 anos",       # 5 a 10
    "26-30 anos",       # 11
    "31-35 anos",       # 12
    "36-40 anos",       # 13
    "41-45 anos",       # 14
    "46-50 anos",       # 15
    "51-55 anos",       # 16
    "56-60 anos",       # 17
    "Maior de 60 anos"  # 18 a 20
  )

  # Gerar as frequências
  fx_etaria_abs <- table(data$TP_FAIXA_ETARIA)
  fx_etaria_abs_grouped <- c(
    "1-4" = sum(fx_etaria_abs[1:4]),
    "5-10" = sum(fx_etaria_abs[5:10]),
    fx_etaria_abs[11:17],
    "18-20" = sum(fx_etaria_abs[18:20])
  )

  fx_etaria_rel_grouped <- prop.table(fx_etaria_abs_grouped) * 100 # Em porcentagem

  # Criar a estrutura para o Chart.js
  objeto_etaria <- list(
    # Garantimos que os nomes venham do nosso vetor de labels, ignorando os nomes do table()
    labels = labels_etaria,
    datasets = list(
      list(
        data = as.numeric(round(fx_etaria_rel_grouped, 2)),
        abs_values = as.numeric(fx_etaria_abs_grouped)
      )
    )
  )

  # --- TRATAMENTO DO PATH ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "faixa_etaria.json")

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  cli::cli_alert_info("Exportando arquivo JSON...")
  jsonlite::write_json(objeto_etaria, path = final_file, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_alert_success("Arquivo de presença salvo: {.path {final_file}}")

  return(invisible(final_file))
}
