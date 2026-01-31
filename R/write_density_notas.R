#' Exportar coordenadas de densidade para Chart.js
#'
#' @param data Data.table com a coluna NU_NOTA_...
#' @param path_json Caminho do arquivo JSON.
#' @export
write_density_notas <- function(data, path_json) {

  cli::cli_h2("Processamento de Densidade (Chart.js)")

  col_prova <- grep("^CO_PROVA_", names(data), value = TRUE)
  col_nota <- grep("^NU_NOTA_", names(data), value = TRUE)

  ano <- data[1,]$NU_ANO
  dic_df <- get(paste0("dic_", ano), envir = .GlobalEnv)

  # --- Função de Cálculo Interna ---
  calc_dens <- function(codigos_pool) {
    df_pool <- data[get(col_prova) %in% codigos_pool & get(col_nota) > 0 & !is.na(get(col_nota))]
    notas_v <- df_pool[[col_nota]]

    if (length(notas_v) < 2) return(NULL)

    dens <- density(notas_v, n = 512, from = min(notas_v), to = max(notas_v))
    y_normal <- dnorm(dens$x, mean = mean(notas_v), sd = sd(notas_v))

    list(
      datasets = list(
        list(id = "main-density", data = data.frame(x = dens$x, y = dens$y)),
        list(id = "normal-reference", data = data.frame(x = dens$x, y = y_normal))
      )
    )
  }

  # --- Definição dos Pools ---
  cod_digital <- dic_df$codigo[grepl("Digital", dic_df$cor, ignore.case = TRUE)]
  cod_regular <- dic_df$codigo[!grepl("Digital", dic_df$cor, ignore.case = TRUE)]

  cli::cli_process_start("Calculando Densidades (Digital vs Regular)")

  lista_completa <- list(
    digital = calc_dens(cod_digital),
    regular = calc_dens(cod_regular)
  )

  cli::cli_process_done()

  # --- TRATAMENTO DO PATH E EXPORTAÇÃO ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "density.json")
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  jsonlite::write_json(lista_completa, path = final_file, pretty = TRUE, auto_unbox = TRUE)

  cli::cli_alert_success("Arquivo salvo com sucesso em: {.path {final_file}}")
  return(invisible(final_file))
}
