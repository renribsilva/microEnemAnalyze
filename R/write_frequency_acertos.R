#' Exportar frequência absoluta e relativa de acertos para Chart.js
#'
#' @param data Data.table contendo a coluna NU_SCORE.
#' @param path_json Caminho do arquivo ou diretório de destino.
#' @export
write_frequency_acertos <- function(data, path_json) {
  cli::cli_h1("Processamento de Frequencia (Acertos)")

  # Validação básica
  cli::cli_process_start("Validando estrutura dos dados")

  # verificando data
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  if (!is.character(path_json)) {
    cli::cli_abort("{.arg path_csv} precisa ser do tipo character.")
  }

  cli::cli_process_done()

  col_prova <- grep("^CO_PROVA_", names(data), value = TRUE)
  col_score <- "NU_SCORE"

  # --- Função de Cálculo Interna ---
  calc_freq <- function(codigos_pool) {
    df_pool <- data[get(col_prova) %in% codigos_pool & !is.na(get(col_score))]
    if (nrow(df_pool) == 0) {
      return(NULL)
    }

    acertos <- df_pool[[col_score]]
    tab_abs <- table(factor(acertos, levels = 0:45))
    tab_rel <- prop.table(tab_abs) * 100

    df_f <- data.table::data.table(
      x = as.numeric(names(tab_abs)),
      abs = as.integer(tab_abs),
      rel = as.numeric(tab_rel)
    )

    list(
      datasets = list(
        list(
          label = "Frequencia Absoluta",
          data = lapply(seq_len(nrow(df_f)), function(i) {
            list(x = df_f$x[i], y = df_f$abs[i])
          })
        ),
        list(
          label = "Frequencia Relativa (%)",
          data = lapply(seq_len(nrow(df_f)), function(i) {
            list(x = df_f$x[i], y = df_f$rel[i])
          })
        )
      )
    )
  }

  ano <- data[1, ]$NU_ANO
  dic_df <- get(paste0("dic_", ano), envir = .GlobalEnv)
  dic_df_p1 <- dic_df[dic_df$tipo == "1", ]

  # --- Definição dos Pools ---
  cod_digital <- dic_df_p1$codigo[grepl(
    "Digital",
    dic_df$cor,
    ignore.case = TRUE
  )]
  cod_regular <- dic_df_p1$codigo[
    !grepl("Digital", dic_df$cor, ignore.case = TRUE)
  ]

  cli::cli_process_start("Calculando Frequencias (Digital vs Regular)")

  lista_completa <- list(
    digital = calc_freq(cod_digital),
    regular = calc_freq(cod_regular)
  )

  cli::cli_process_done()

  # --- Exportação ---
  cli::cli_process_start("Exportando arquivo JSON")
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, "frequency_acertos.json")
  }
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  jsonlite::write_json(
    lista_completa,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salvo em: {.path {final_file}}")

  invisible(final_file)
}
