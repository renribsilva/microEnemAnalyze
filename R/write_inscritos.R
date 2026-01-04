#' @title Gerar JSON de Inscritos e Treineiros
#'
#' @description Esta função processa os dados do ENEM, valida a integridade dos inscritos
#' e exporta um arquivo JSON estruturado para uso em dashboards.
#'
#' @param data Um data.table ou data.frame contendo as colunas NU_INSCRICAO e IN_TREINEIRO.
#' @param path_json O diretório onde o arquivo 'inscritos.json' será salvo.
#'
#' @return Retorna o caminho do arquivo gerado (invisivelmente).
#' @export
write_inscritos <- function(data, path_json) {

  # --- TÍTULO ---
  cli::cli_h2("Processamento de Inscritos e Treineiros")

  # 1. Validação dos argumentos
  cli::cli_process_start("Validando estrutura dos dados")

  if (typeof(path_json) != "character") {
    cli::cli_alert_danger("Erro no argumento {.var path_json}")
    stop("Erro: o argumentos path_json precisa ser do tipo character")
  }

  if (typeof(data) != "data.table") {
    # cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  if (any(is.na(data$NU_INSCRICAO))) {
    cli::cli_alert_danger("Valores ausentes detectados em {.var NU_INSCRICAO}")
    stop("Erro: Existem valores NA em NU_INSCRICAO.")
  }
  cli::cli_process_done()

  # 2. Criação de tabelas de frequências
  ap <- cli::cli_process_start("Calculando frequências e estatísticas")

  inscritos <- as.integer(length(data$NU_INSCRICAO))

  treineiros_counts <- table(data$IN_TREINEIRO)
  treineiros_prop <- prop.table(treineiros_counts)

  tabela_treineiros <- t(as.data.table(rbind(treineiros_counts, treineiros_prop)))
  total_counts <- sum(tabela_treineiros[, 1])
  total_props  <- sum(tabela_treineiros[, 2])

  # 3. Validação de integridade
  if (as.integer(inscritos) != as.integer(total_counts)) {
    cli::cli_alert_danger("Inconsistência nos totais calculados")
    stop("Erro: A soma de treineiros não bate com o total de inscritos.")
  }

  total_row <- c(total_counts, total_props)
  tabela_treineiros <- rbind(tabela_treineiros, total_row)
  tabela_treineiros[, 2] <- round(tabela_treineiros[, 2] * 100, 2)

  # 4. Objeto JSON
  objeto_inscritos <- list(
    list(
      grupo = "Inscritos",
      total = as.numeric(tabela_treineiros[3, 1]),
      freq  = as.numeric(tabela_treineiros[3, 2]),
      subRows = list(
        list(grupo = "Não treineiros",
             total = as.numeric(tabela_treineiros[1, 1]),
             freq  = as.numeric(tabela_treineiros[1, 2])),
        list(grupo = "Treineiros",
             total = as.numeric(tabela_treineiros[2, 1]),
             freq  = as.numeric(tabela_treineiros[2, 2]))
      )
    )
  )
  cli::cli_process_done(id = ap)

  # 5. Exportação
  cli::cli_process_start("Salvando arquivo final")

  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "inscritos.json")

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  jsonlite::write_json(objeto_inscritos, path = final_file, pretty = TRUE, auto_unbox = TRUE)

  cli::cli_process_done()

  cli::cli_alert_success("Processo concluído: {.file {final_file}}")

  return(invisible(final_file))
}
