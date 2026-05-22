#' Exportar Estatísticas Descritivas e Densidade por Score
#' @param data Uma lista nomeada de data.tables.
#' @param path_json Caminho para o arquivo .json de saída.
#' @param ano Número que indica o ano do exame
#' @export
write_score_describe <- function(data, path_json, ano) {
  cli::cli_h1(
    "Descricao estatistica: Processamento por Score (Digital vs Regular)"
  )
  lista_final_resultados <- list()

  for (i in seq_along(data)) {
    dt_area <- data[[i]]

    # --- Identificação da Área ---
    names_dt <- names(dt_area)
    idx_nota <- grep("NU_NOTA_", names_dt)
    if (length(idx_nota) == 0) {
      next
    }

    col_referencia <- names_dt[idx_nota[1]]
    nm <- gsub("NU_NOTA_", "", col_referencia)
    col_prova <- paste0("CO_PROVA_", nm)

    cli::cli_process_start("Processando area: {.strong {nm}}")

    # --- Função interna para evitar repetição de código ---
    processar_grupo <- function(codigos_selecionados) {
      nota <- NULL
      score <- NULL
      vars <- NULL
      new_col_score <- "NU_SCORE"
      dt_temp <- dt_area[
        get(col_prova) %in%
          codigos_selecionados &
          !is.na(get(col_referencia)) &
          get(col_referencia) > 0,
        list(
          nota = as.numeric(get(col_referencia)),
          score = as.integer(get(new_col_score))
        )
      ]

      if (nrow(dt_temp) == 0) {
        return(NULL)
      }

      res_agg <- dt_temp[,
        data.table::as.data.table(psych::describe(nota)),
        keyby = list(score)
      ]

      res_agg[, vars := NULL] # nolint: objecti_usage_linter

      lista_scores <- stats::setNames(vector("list", 46), 0:45)
      for (s in 0:45) {
        row_stats <- res_agg[score == s]
        notas_grupo <- dt_temp[score == s, nota]
        if (nrow(row_stats) > 0) {
          stats_list <- as.list(row_stats)
          stats_list$score <- NULL
          if (length(notas_grupo) > 1) {
            dens <- stats::density(
              notas_grupo,
              from = row_stats$min,
              to = row_stats$max,
              n = ((row_stats$max - row_stats$min) * 10) + 1
            )
            stats_list$density <- list(
              x = seq(row_stats$min, row_stats$max, by = 0.1),
              y = dens$y
            )
          }
          lista_scores[[as.character(s)]] <- stats_list
        }
      }
      lista_scores
    }

    ano_dt <- dt_area[1, ]$NU_ANO
    dic_df <- get(paste0("dic_", ano_dt), envir = .GlobalEnv)
    dic_df_p1 <- dic_df[dic_df$tipo == "1", ]

    col_cor <- "cor"
    col_codigo <- "codigo"

    # --- Separação e Atribuição mantendo a chave original [[nm]] ---
    cod_digital <- dic_df_p1[
      grepl("Digital", get(col_cor), ignore.case = TRUE),
      get(col_codigo)
    ]
    cod_regular <- dic_df_p1[
      !grepl("Digital", get(col_cor), ignore.case = TRUE),
      get(col_codigo)
    ]

    lista_final_resultados[[nm]] <- list(
      digital = processar_grupo(cod_digital),
      regular = processar_grupo(cod_regular)
    )

    cli::cli_process_done()
  }

  # --- Exportação (Igual ao original) ---
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, "score_describe.json")
  }
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(
    lista_final_resultados,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  invisible(lista_final_resultados)
}
