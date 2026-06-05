#' @title Escrever JSON sobre cor ou raça dos participantes do ENEM
#'
#' @description Esta função calcula frequências da variável
#' cor ou raça do microdados do ENEM e exporta um JSON
#'
#' @param data Um data.table contendo os microdados do ENEM.
#' @param path_json Caminho da pasta onde o arquivo JSON será salvo.
#'
#' @export
write_cor_raca <- function(data, path_json) {
  cli::cli_h1("Processamento de Dados: Cor ou Raca")

  cli::cli_process_start("Validando argumentos")

  if (missing(data)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg data} e obrigatorio.",
      "i" = "Por favor, forneca os microdados do ENEM."
    ))
  }

  if (missing(path_json)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg path_csv} e obrigatorio.",
      "i" = "Por favor, forneca o caminho onde o csv sera gravado."
    ))
  }

  if (!is.character(path_json)) {
    cli::cli_abort("{.arg path_csv} precisa ser do tipo character.")
  }

  # Normaliza os microdados
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  cli::cli_process_done()

  cli::cli_process_start("Calculando frequencias e estruturando Treemap")

  # Mapeamento completo (Dicionário)
  mapa_cores <- c(
    "0" = "Nao declarado",
    "1" = "Branca",
    "2" = "Preta",
    "3" = "Parda",
    "4" = "Amarela",
    "5" = "Indigena",
    "6" = "Nao dispoe da informacao"
  )

  # Contagem bruta
  contagem <- table(as.character(data$TP_COR_RACA))

  # Cruza os dados existentes com os nomes do dicionário
  # Isso garante que apenas o que EXISTE no data.table seja mapeado
  df_treemap <- data.table::data.table(
    codigo = names(contagem),
    abs = as.numeric(contagem)
  )

  # Adiciona os labels baseados no código que veio da table
  df_treemap$label <- mapa_cores[df_treemap$codigo]

  # Calcula a porcentagem
  df_treemap$value <- round((df_treemap$abs / sum(df_treemap$abs)) * 100, 2)

  # Se por acaso houver um código fora de 0-6, o label fica NA.
  df_treemap <- df_treemap[!is.na(df_treemap$label), c("label", "value", "abs")]

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
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, "cor_raca.json")
  }
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  jsonlite::write_json(
    objeto_cor_raca,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salvo em: {.path {final_file}}")

  invisible(final_file)
}
