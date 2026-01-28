#' Exportar TCCs de todos os cadernos em um único JSON
#'
#' @param data Data.table com a coluna NU_NOTA e NU_SCORE
#' @param path_json String. Caminho completo do arquivo.
#' @export
write_describe_notas <- function(data, path_json) {

  cli::cli_h2("Descrição Estatística")
  cli::cli_process_start("Calculando estatísticas descritivas")

  col_nota <- grep("^NU_NOTA_", names(data), value = TRUE)
  col_prova <- grep("^CO_PROVA_", names(data), value = TRUE)
  col_score <- "NU_SCORE"

  ano <- data[1,]$NU_ANO
  dic_df   <- get(paste0("dic_", ano),   envir = .GlobalEnv)
  cod_selected <- dic_df$codigo

  data_filtrado <- data[get(col_prova) %in% cod_selected & get(col_nota) > 0 & !is.na(get(col_nota))]

  get_mode <- function(x) {
    ux <- unique(na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
  }

  # Cálculos para NOTAS
  v_notas <- data_filtrado[[col_nota]]
  desc_nota <- as.list(psych::describe(v_notas)[1, ])
  desc_nota$mode <- microEnemAnalize::get_grouped_mode(v_notas, bin_width = 25)
  desc_nota$q1 <- quantile(v_notas, 0.25, na.rm = TRUE)[[1]]
  desc_nota$q3 <- quantile(v_notas, 0.75, na.rm = TRUE)[[1]]
  desc_nota$p99 <- quantile(v_notas, probs = 0.99, na.rm = TRUE, type = 1)[[1]] # Mínima do Top 1%

  # Cálculos para ACERTOS
  v_acertos <- data_filtrado[[col_score]]
  desc_acertos <- as.list(psych::describe(v_acertos)[1, ])
  desc_acertos$mode <- get_mode(v_acertos)
  desc_acertos$q1 <- quantile(v_acertos, 0.25, na.rm = TRUE)[[1]]
  desc_acertos$q3 <- quantile(v_acertos, 0.75, na.rm = TRUE)[[1]]
  desc_acertos$p99 <- quantile(v_acertos, probs = 0.99, na.rm = TRUE, type = 1)[[1]]

  lista_completa <- list(
    notas = desc_nota,
    acertos = desc_acertos
  )

  cli::cli_process_done()

  # --- EXPORTAÇÃO ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "describe.json")
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(lista_completa, path = final_file, pretty = TRUE, auto_unbox = TRUE)

  cli::cli_alert_success("Processo concluído: {.path {final_file}}")
  return(invisible(final_file))
}
