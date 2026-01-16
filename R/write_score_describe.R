#' Exportar Estatísticas Descritivas e Densidade por Score
#' @param data Uma lista nomeada de data.tables.
#' @param path_json Caminho para o arquivo .json de saída.
write_score_describe <- function(data, path_json) {

  cli::cli_h1("Descrição estatística: Processamento por Score")
  cli::cli_alert_info("Iniciando análise de {length(data)} área(s)")

  lista_final_resultados <- list()

  for (i in seq_along(data)) {
    dt_area <- data[[i]]
    names_dt <- names(dt_area)

    idx_nota <- grep("NU_NOTA_", names_dt)
    if (length(idx_nota) == 0) next

    col_referencia <- names_dt[idx_nota[1]]
    nm <- gsub("NU_NOTA_", "", col_referencia)

    cli::cli_process_start("Processando área: {.strong {nm}}")

    if (!"NU_SCORE" %in% names_dt) {
      cli::cli_alert_danger("Coluna NU_SCORE não encontrada em {nm}")
      next
    }

    # 1. Filtro e Seleção em um passo único (Performance)
    dt_temp <- dt_area[!is.na(get(col_referencia)) & get(col_referencia) > 0,
                       .(nota = as.numeric(get(col_referencia)),
                         score = as.integer(NU_SCORE))]

    if (nrow(dt_temp) == 0) {
      cli::cli_alert_warning("Área {nm} sem dados válidos.")
      next
    }

    # 2. Agrupamento Estatístico
    res_agg <- dt_temp[, as.data.frame(psych::describe(nota)), keyby = .(score)]
    res_agg[, vars := NULL]

    # 3. Construção da lista de scores com Densidade
    lista_scores <- setNames(vector("list", 46), 0:45)

    for (s in 0:45) {
      # Dados do describe para o score s
      row_stats <- res_agg[score == s]
      # Dados brutos para cálculo da densidade
      notas_grupo <- dt_temp[score == s, nota]

      if (nrow(row_stats) > 0 && length(notas_grupo) > 1) {

        # --- Cálculo da Densidade ---
        x_min <- row_stats$min
        x_max <- row_stats$max
        # n é o número de pontos (de 1 em 1 ponto)
        n_points <- ((x_max - x_min) * 10) + 1

        dens <- density(notas_grupo, from = x_min, to = x_max, n = n_points)

        # Preparação do objeto de estatísticas
        stats_list <- as.list(row_stats)
        stats_list$score <- NULL

        # Adiciona o elemento de densidade solicitado
        stats_list$density <- list(
          x = seq(x_min, x_max, by = 0.1),
          y = dens$y
        )

        lista_scores[[as.character(s)]] <- stats_list
      } else if (nrow(row_stats) > 0) {
        # Caso tenha apenas 1 dado (não dá pra calcular densidade)
        stats_list <- as.list(row_stats)
        stats_list$score <- NULL
        stats_list$density <- NULL
        lista_scores[[as.character(s)]] <- stats_list
      }
    }

    lista_final_resultados[[nm]] <- lista_scores
    cli::cli_process_done()
  }

  # --- EXPORTAÇÃO ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "score_describe.json")
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  cli::cli_process_start("Salvando JSON: {.path {basename(final_file)}}")

  jsonlite::write_json(lista_final_resultados,
                       path = final_file,
                       pretty = TRUE,
                       auto_unbox = TRUE,
                       na = "null")

  cli::cli_process_done()
  cli::cli_alert_success("Processamento finalizado: {.path {final_file}}")

  return(invisible(lista_final_resultados))
}
