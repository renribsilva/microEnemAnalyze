#' Exportar Frequências Gerais e por Faixas de Nota para JSON
#'
#' Esta função processa listas de microdados do ENEM, calculando a frequência
#' total de respostas por item e a distribuição dessas respostas em faixas
#' de 50 pontos (bins).
#'
#' @param data Uma lista nomeada de \code{data.table}s.
#' @param path_json Caminho completo do arquivo .json ou diretório de destino.
#'
#' @return Retorna invisivelmente a lista processada. Estrutura do JSON:
#' \code{id_item -> { counts: {...}, bins: { labels: [], "0": [], "1": [] } }}.
#'
#' @importFrom data.table is.data.table
#' @importFrom jsonlite write_json
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_process_start cli_process_done
#' @importFrom utils head tail
#' @export
write_score_table <- function(data, path_json) {

  prefixos_ignore <- c("NU_ANO", "NU_INSCRICAO", "TP_LINGUA", "NU_SCORE", "TP_PRESENCA",
                       "CO_PROVA", "NU_NOTA", "TX_RESPOSTAS", "TX_GABARITO")

  cli::cli_h1("Processamento de Frequências (ENEM)")
  cli::cli_alert_info("Iniciando processamento de {length(data)} área(s)")

  lista_final_resultados <- list()

  for (i in seq_along(data)) {

    dt_area <- data[[i]]

    ano <- dt_area[1,]$NU_ANO
    dic_df   <- get(paste0("dic_", ano),   envir = .GlobalEnv)
    cod_selected <- dic_df$codigo

    names_dt <- names(dt_area)

    idx_nota <- grep("NU_NOTA_", names_dt)
    if (length(idx_nota) == 0) next

    col_referencia <- names_dt[idx_nota[1]]
    nm <- gsub("NU_NOTA_", "", col_referencia)

    # Identifica a coluna de prova dinâmica (ex: CO_PROVA_CN)
    col_prova <- paste0("CO_PROVA_", nm)

    # --- FILTRAGEM DO DT (APENAS NOTAS > 0) ---
    dt <- dt_area[get(col_prova) %in% cod_selected & get(col_referencia) > 0]

    cli::cli_alert_info("Processando: {.strong {nm}}...")

    # --- LÓGICA DE FAIXAS ---
    nota_min <- min(dt[[col_referencia]], na.rm = TRUE)
    nota_max <- max(dt[[col_referencia]], na.rm = TRUE)
    quebras  <- seq(floor(nota_min/100)*100, ceiling(nota_max/100)*100, by = 50)
    labels_faixas <- paste0(head(quebras, -1), "-", tail(quebras, -1))

    faixas <- cut(dt[[col_referencia]], breaks = quebras, labels = labels_faixas, include.lowest = TRUE)

    regex_ignore <- paste0("^(", paste(prefixos_ignore, collapse = "|"), ")")
    cols_para_calcular <- names_dt[!grepl(regex_ignore, names_dt)]

    if (length(cols_para_calcular) > 0) {
      res_area <- lapply(dt[, ..cols_para_calcular], function(x) {

        # 1. Contagens Totais
        total_counts <- as.list(table(x, useNA = "no"))

        # 2. Bins com chave 'labels' explícita
        # Criamos um dataframe para garantir que os status (0, 1, etc)
        # sejam as colunas e as faixas as linhas
        tab_bins <- table(faixas, x, useNA = "no")
        df_bins <- as.data.frame.matrix(tab_bins)

        # Transformamos o df em lista e injetamos os labels
        bin_data <- as.list(df_bins)
        bin_data$labels <- labels_faixas # <--- A CHAVE QUE FALTAVA

        list(
          counts = total_counts,
          bins = bin_data
        )
      })
      lista_final_resultados <- c(lista_final_resultados, res_area)
    }
    cli::cli_alert_success("Área {.strong {nm}} concluída.")
  }

  is_file <- grepl("\\.json$", path_json, ignore.case = TRUE)
  final_file <- if(is_file) path_json else file.path(path_json, "score_table.json")
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  cli::cli_process_start("Salvando arquivo JSON")
  jsonlite::write_json(lista_final_resultados, path = final_file, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_process_done()

  return(invisible(lista_final_resultados))
}
