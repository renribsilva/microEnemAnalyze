#' Exportar Curva Empírica por Item (CCI)
#'
#' Esta função processa microdados do ENEM para calcular a probabilidade observada
#' de acerto (curva empírica) por item, utilizando uma escala imutável de 1 em 1 ponto.
#' O cálculo da proporção considera apenas respostas válidas (0, 1, 7, 8) e notas > 0.
#'
#' @param data Uma lista nomeada de \code{data.table}s (ex: list(MT = dt_mt)).
#' @param path_json Caminho completo para o arquivo .json de saída.
#'
#' @return Retorna a lista processada invisivelmente. O JSON gerado segue a estrutura:
#' \code{codigo_item -> { x: [notas], y: [proporcoes] }}.
#'
#' @importFrom data.table is.data.table .N
#' @importFrom jsonlite write_json
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_process_start cli_process_done cli_alert_warning
#' @export
write_score_graph <- function(data, path_json) {

  prefixos_ignore <- c("NU_INSCRICAO", "TP_LINGUA", "NU_SCORE", "TP_PRESENCA",
                       "CO_PROVA", "NU_NOTA", "TX_RESPOSTAS", "TX_GABARITO")

  cli::cli_h1("CCI Empírica: Processamento por Item")
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

    # --- FILTRAGEM DO DT (APENAS NOTAS > 0) ---
    dt <- dt_area[get(col_referencia) > 0]

    if (nrow(dt) == 0) {
      cli::cli_process_done()
      cli::cli_alert_warning("Pulei {.val {nm}}: nenhum registro com nota > 0.")
      next
    }

    # --- ESCALA IMUTÁVEL DE 1 EM 1 PONTO ---
    nota_min <- min(dt[[col_referencia]], na.rm = TRUE)
    nota_max <- max(dt[[col_referencia]], na.rm = TRUE)
    escala_x <- as.numeric(seq(floor(nota_min), ceiling(nota_max), by = 1))

    # Filtra colunas de itens
    regex_ignore <- paste0("^(", paste(prefixos_ignore, collapse = "|"), ")")
    cols_para_calcular <- names_dt[!grepl(regex_ignore, names_dt)]

    # Processamento por Item
    res_area <- lapply(cols_para_calcular, function(code) {

      # Lógica Empírica: Frequência 1 / (0+1+7+8)
      tabela_real <- dt[, .(
        p = sum(get(code) == "1", na.rm = TRUE) /
          sum(get(code) %in% c("0", "1", "7", "8"), na.rm = TRUE)
      ), keyby = .(x = as.integer(round(get(col_referencia), 0)))]

      # Merge para escala completa
      df_merge <- merge(data.frame(x = escala_x), tabela_real, by = "x", all.x = TRUE)

      list(
        x = escala_x,
        y = round(df_merge$p, 3)
      )
    })

    names(res_area) <- cols_para_calcular
    lista_final_resultados <- c(lista_final_resultados, res_area)

    cli::cli_process_done()
  }

  # --- EXPORTAÇÃO ---
  is_file <- grepl("\\.json$", path_json, ignore.case = TRUE)
  final_file <- if(is_file) path_json else file.path(path_json, "score_graph.json")

  cli::cli_process_start("Salvando JSON: {.path {basename(final_file)}}")
  jsonlite::write_json(lista_final_resultados, path = final_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
  cli::cli_process_done()

  cli::cli_alert_success("Processamento finalizado com sucesso.")
  return(invisible(lista_final_resultados))
}
