#' @title Gera JSON de Estatísticas Completas da Redação
#'
#' @description Processa NU_NOTA_COMP1-5 e NU_NOTA_REDACAO com labels
#' padronizados (by 20).
#'
#' @param data Um data.table contendo as colunas de competências,
#' nota e status.
#' @param path_json Caminho para salvar o arquivo JSON.
#' @import cli
#'
#' @export
write_comp_redacao <- function(data, path_json) {
  cli::cli_h1("Processamento Integral: Competencias + Nota Total")

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

  # Constrói os nomes das colunas
  cols_comp <- paste0("NU_NOTA_COMP", 1:5)
  cols_total <- "NU_NOTA_REDACAO"
  todas_as_notas <- c(cols_comp, cols_total)
  colunas_necessarias <- c(todas_as_notas, "TP_STATUS_REDACAO")

  # Etapa de verificação
  if (!all(colunas_necessarias %in% names(data))) {
    colunas_faltantes <- setdiff(colunas_necessarias, names(data))
    cli::cli_abort(
      "Erro: Colunas necessarias ausentes: {.var {colunas_faltantes}}."
    )
  }

  # --- PROCESSAMENTO ---
  cli::cli_process_start(
    "Calculando metricas com labels fixos (0-200/1000 by 20)"
  )

  # Itera sobre todas as notas uma função, retornando uma lista
  resultados_final <- lapply(todas_as_notas, function(col) {
    # Filtro: Status 1 e remove NAs
    valores <- data[[col]][data$TP_STATUS_REDACAO %in% 1 & !is.na(data[[col]])]

    if (length(valores) == 0) {
      return(NULL)
    }

    # Frequência com lables fixos (by20)
    limite_max <- if (col == "NU_NOTA_REDACAO") 1000 else 200
    labels_fixos <- seq(0, limite_max, by = 20)

    # Factor garante que todos os labels de 20 em 20 apareçam, mesmo com freq 0
    freq_tab <- table(factor(valores, levels = labels_fixos))

    # psych::describe fornece skew e kurtosis
    desc <- psych::describe(valores)

    # Moda absoluta do dado bruto (valor exato mais frequente)
    raw_tab <- table(valores)
    moda_bruta <- as.numeric(names(raw_tab)[which.max(raw_tab)])

    # Percentis (Q1, Q3, P99)
    quants <- stats::quantile(
      valores,
      probs = c(0.25, 0.75, 0.99),
      na.rm = TRUE,
      type = 1
    )

    # Densidade
    dens <- stats::density(valores, from = 0, to = limite_max)

    list(
      nome = col,
      frequencia = list(
        labels = as.numeric(names(freq_tab)),
        values = as.numeric(freq_tab)
      ),
      estatisticas = list(
        media = round(desc$mean, 2),
        mediana = desc$median,
        moda = moda_bruta,
        sd = round(desc$sd, 2),
        q1 = quants[[1]],
        q3 = quants[[2]],
        p99 = quants[[3]],
        skew = round(desc$skew, 4),
        kurtosis = round(desc$kurtosis, 4),
        n = desc$n
      ),
      densidade = list(
        x = round(dens$x, 2),
        y = round(dens$y, 6)
      )
    )
  })

  names(resultados_final) <- todas_as_notas
  cli::cli_process_done()

  # --- EXPORTAÇÃO ---
  cli::cli_process_start("Exportando arquivo JSON")
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, "estatisticas_redacao_completa.json")
  }
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  jsonlite::write_json(
    resultados_final,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salvo em: {.path {final_file}}")

  invisible(final_file)
}
