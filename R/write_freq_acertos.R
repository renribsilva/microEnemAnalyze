#' Exportar frequência absoluta e relativa de acertos para Chart.js
#'
#' @param data Data.table contendo a coluna NU_SCORE.
#' @param path_json Caminho do arquivo ou diretório de destino.
#' @export
write_frequency_acertos <- function(data, path_json) {

  cli::cli_h2("Processamento de Frequência (Acertos)")

  # 1. Preparação dos dados
  cli::cli_process_start("Calculando frequências de NU_SCORE")

  # Garantir que NU_SCORE seja tratado como inteiro e remover NAs
  acertos <- data$NU_SCORE[!is.na(data$NU_SCORE)]

  # Criar tabela de frequência absoluta
  # Usamos factor para garantir que acertos com 0 alunos também apareçam (0 a 45)
  tab_abs <- table(factor(acertos, levels = 0:45))

  # Criar tabela de frequência relativa
  tab_rel <- prop.table(tab_abs) * 100

  # 2. Formatação para Chart.js {x: acerto, y: valor}
  df_frequencia <- data.frame(
    x = as.numeric(names(tab_abs)),
    frequencia_absoluta = as.integer(tab_abs),
    frequencia_relativa = as.numeric(tab_rel)
  )

  # Montagem da estrutura JSON
  # Exportamos ambas para que você escolha qual usar no gráfico (yAbs ou yRel)
  lista_completa <- list(
    datasets = list(
      list(
        label = "Frequência Absoluta",
        data = lapply(1:nrow(df_frequencia), function(i) {
          list(x = df_frequencia$x[i], y = df_frequencia$frequencia_absoluta[i])
        })
      ),
      list(
        label = "Frequência Relativa (%)",
        data = lapply(1:nrow(df_frequencia), function(i) {
          list(x = df_frequencia$x[i], y = df_frequencia$frequencia_relativa[i])
        })
      )
    )
  )
  cli::cli_process_done()

  # 3. Exportação
  cli::cli_process_start("Salvando JSON")
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "frequency_acertos.json")
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  jsonlite::write_json(lista_completa, path = final_file, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_process_done()

  cli::cli_alert_success("Frequências salvas em: {.path {final_file}}")
  return(invisible(final_file))
}
