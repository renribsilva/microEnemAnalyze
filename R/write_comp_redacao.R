#' @title Gera JSON de Estatísticas Completas da Redação
#'
#' @description Processa NU_NOTA_COMP1-5 e NU_NOTA_REDACAO com labels padronizados (by 20).
#'
#' @param data Um data.frame contendo as colunas de competências, nota e status.
#' @param path_json Caminho para salvar o arquivo JSON.
#'
#' @export
write_comp_redacao <- function(data, path_json) {

  cli::cli_h2("Processamento Integral: Competências + Nota Total")

  # --- CONFIGURAÇÃO DE COLUNAS ---
  cols_comp <- paste0("NU_NOTA_COMP", 1:5)
  cols_total <- "NU_NOTA_REDACAO"
  todas_as_notas <- c(cols_comp, cols_total)

  colunas_necessarias <- c(todas_as_notas, "TP_STATUS_REDACAO")

  if (!all(colunas_necessarias %in% names(data))) {
    cli::cli_alert_danger("Erro: Colunas necessárias ausentes.")
    stop("Execução interrompida.")
  }

  # --- PROCESSAMENTO ---
  cli::cli_process_start("Calculando métricas com labels fixos (0-200/1000 by 20)")

  resultados_final <- lapply(todas_as_notas, function(col) {

    # Filtro: Status 1 e remove NAs
    valores <- data[[col]][data$TP_STATUS_REDACAO == 1 & !is.na(data[[col]])]

    if(length(valores) == 0) return(NULL)

    # 1. FREQUÊNCIA COM LABELS FIXOS (by 20)
    limite_max <- if(col == "NU_NOTA_REDACAO") 1000 else 200
    labels_fixos <- seq(0, limite_max, by = 20)

    # Factor garante que todos os labels de 20 em 20 apareçam, mesmo com freq 0
    freq_tab <- base::table(factor(valores, levels = labels_fixos))

    # 2. ESTATÍSTICAS (Dados Crus)
    # psych::describe fornece skew e kurtosis
    desc <- psych::describe(valores)

    # Moda absoluta do dado bruto (valor exato mais frequente)
    raw_tab <- base::table(valores)
    moda_bruta <- as.numeric(names(raw_tab)[which.max(raw_tab)])

    # Percentis solicitados (Q1, Q3, P99)
    quants <- quantile(valores, probs = c(0.25, 0.75, 0.99), na.rm = TRUE, type = 1)

    # 3. DENSIDADE
    dens <- stats::density(valores, from = 0, to = limite_max)

    list(
      nome = col,
      frequencia = list(
        labels = as.numeric(names(freq_tab)),
        values = as.numeric(freq_tab)
      ),
      estatisticas = list(
        media    = round(desc$mean, 2),
        mediana  = desc$median,
        moda     = moda_bruta,
        sd       = round(desc$sd, 2),
        q1       = quants[[1]],
        q3       = quants[[2]],
        p99      = quants[[3]],
        skew     = round(desc$skew, 4),
        kurtosis = round(desc$kurtosis, 4),
        n        = desc$n
      ),
      densidade = list(
        x = round(dens$x, 2),
        y = round(dens$y, 6)
      )
    )
  })

  # Remove possíveis nulos e nomeia a lista
  names(resultados_final) <- todas_as_notas
  cli::cli_process_done()

  # --- EXPORTAÇÃO ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "estatisticas_redacao_completa.json")
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  cli::cli_process_start("Exportando JSON para {.path {final_file}}")
  jsonlite::write_json(resultados_final, path = final_file, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_process_done()

  cli::cli_alert_success("Sucesso! Estatísticas e frequências (step 20) exportadas corretamente.")

  return(invisible(final_file))
}
