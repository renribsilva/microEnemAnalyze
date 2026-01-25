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

  cli::cli_h2("Processamento de Dados: Cor ou Raça")

  # Validação básica
  cli::cli_process_start("Validando estrutura dos dados")
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  cli::cli_process_done()

  cli::cli_process_start("Calculando frequências e estruturando Treemap")

  # 1. Mapeamento completo (Dicionário)
  mapa_cores <- c(
    "0" = "Não declarado",
    "1" = "Branca",
    "2" = "Preta",
    "3" = "Parda",
    "4" = "Amarela",
    "5" = "Indígena",
    "6" = "Não dispõe da informação"
  )

  # 2. Contagem bruta
  # Usamos table direto nos dados existentes
  contagem <- table(as.character(data$TP_COR_RACA))

  # 3. Cruzamos os dados existentes com os nomes do dicionário
  # Isso garante que apenas o que EXISTE no dado entre no data.frame
  df_treemap <- data.frame(
    codigo = names(contagem),
    abs = as.numeric(contagem)
  )

  # Adicionamos os labels baseados no código que veio da table
  df_treemap$label <- mapa_cores[df_treemap$codigo]

  # Calculamos a porcentagem baseada no total
  df_treemap$value <- round((df_treemap$abs / sum(df_treemap$abs)) * 100, 2)

  # 4. Limpeza: Se por acaso houver um código fora de 0-6, o label fica NA.
  # Vamos garantir que não exportamos lixo.
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
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "cor_raca.json")
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  jsonlite::write_json(objeto_cor_raca, path = final_file, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salvo em: {.path {final_file}}")
  return(invisible(final_file))
}
