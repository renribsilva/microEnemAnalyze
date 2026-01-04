#' @title Gerar Score de todas as provas do ENEM, para um determinado ano.
#'
#' @description Esta função escreve arquivos csv com os scores de todos os
#' participantes do ENEM, para fins estatísticos.
#'
#' @param data Um data frame com as colunas CO_PROVA_, TX_RESPOSTAS_, TX_GABARITO_
#' @param ano Inteiro ou caractere indicando o ano (ex: 2019) para buscar os objetos itens_ano e dic_ano.
#' @param path_csv Opcional. String com o caminho do diretório ou nome do arquivo .json final.
#' @param area Opcional. String com a area a ser processada (ex: LC).
#'
#' @export
write_score <- function(data, path_csv = NULL, ano, area = NULL) {

  if (is.null(path_csv)) {
    cli::cli_abort("{.arg path_csv} não pode ser NULL.")
  }

  # --- TÍTULO PRINCIPAL ---
  cli::cli_h1("Processamento de Scores ENEM {ano}")

  # 1. Recuperar os objetos da memória
  cli::cli_process_start("Recuperando dicionários e itens do ambiente")
  tryCatch({
    itens_df <- get(paste0("itens_", as.character(ano)), envir = .GlobalEnv)
    dic_df   <- get(paste0("dic_", as.character(ano)), envir = .GlobalEnv)
    cli::cli_process_done()
  }, error = function(e) {
    cli::cli_process_failed()
    cli::cli_alert_danger("Objetos {.var itens_{ano}} ou {.var dic_{ano}} não encontrados no {.code .GlobalEnv}.")
    stop(e)
  })

  n <- nrow(data)
  dic_df_P1 <- dic_df[dic_df$aplicacao == "P1", ]
  areas_to_process <- if (!is.null(area)) area else c("LC", "CH", "CN", "MT")

  for (area_loop in areas_to_process) {

    # --- SUBTÍTULO POR ÁREA ---
    cli::cli_h2("Área: {.field {area_loop}}")

    # Filtra o dicionário de provas e dados dos itens
    dic_df_P1_area <- dic_df_P1[as.character(dic_df_P1$area) == area_loop, ]
    itens_df_filtered <- itens_df[itens_df$CO_PROVA %in% dic_df_P1_area$codigo, ]
    cod_itens <- unique(itens_df_filtered$CO_ITEM)

    score_df <- matrix(
      NA_integer_,
      nrow = n,
      ncol = length(cod_itens),
      dimnames = list(NULL, as.character(cod_itens))
    )
    score_nu <- matrix(NA, ncol = 1, nrow = n)

    # Variáveis
    col_co_prova <- paste0("CO_PROVA_", area_loop)
    col_tx_gabarito <- paste0("TX_GABARITO_", area_loop)
    col_tx_resposta <- paste0("TX_RESPOSTAS_", area_loop)

    vetor_respostas <- data[[col_tx_resposta]]
    vetor_gabaritos <- data[[col_tx_gabarito]]
    vetor_codigos <- as.numeric(data[[col_co_prova]])

    # Interface de progresso dinâmica
    cp <- cli::cli_process_start("Corrigindo provas de {.val {n}} participantes")

    for (i in seq_len(n)) {

      cod_prova_origem <- as.numeric(vetor_codigos[i])

      if (is.na(cod_prova_origem)) {
        score_df[i, ] <- NA
        score_nu[i, ] <- NA
        next
      }

      if (cod_prova_origem %in% as.numeric(dic_df_P1_area$codigo)) {

        itens_prova_origem <- itens_df[itens_df$CO_PROVA == cod_prova_origem, ]
        itens_prova_origem <- itens_prova_origem[order(itens_prova_origem$TP_LINGUA, itens_prova_origem$CO_POSICAO), ]

        if (is.null(itens_prova_origem)) stop(sprintf("Erro: itens da prova %d não está mapeada", cod_prova_origem))

        resp_orig_string <- vetor_respostas[i]
        resp_orig_vetor <- strsplit(gsub(" ", "", resp_orig_string), "")[[1]]

        if (length(resp_orig_vetor) != (if(area_loop == "LC") 50 else 45)) {
          stop(sprintf("Vetor RESP tem tamanho errado (%d) na linha %d.", length(resp_orig_vetor), i))
        }

        gab_orig_string <- vetor_gabaritos[i]
        gab_orig_vetor <- strsplit(gsub(" ", "", gab_orig_string), "")[[1]]

        if (length(gab_orig_vetor) != (if(area_loop == "LC") 50 else 45)) {
          stop(sprintf("Vetor gab_orig_vetor tem tamanho errado (%d) na linha %d.", length(gab_orig_vetor), i))
        }

        if (nrow(itens_prova_origem) != length(resp_orig_vetor)) {
          stop(sprintf("O tamanho do vetor resp_orig_vetor (%d) diverge do comprimento de seus itens (%d) na linha %d.", length(resp_orig_vetor), nrow(itens_prova_origem), i))
        }

        col_names  <- as.character(itens_prova_origem$CO_ITEM)
        gab_vetor  <- as.character(itens_prova_origem$TX_GABARITO)
        resp_vetor <- as.character(resp_orig_vetor)

        res_vetor <- ifelse(resp_vetor == gab_vetor, 1L, 0L)
        res_vetor[resp_vetor == "9"] <- 9L
        res_vetor[resp_vetor == "."] <- 8L
        res_vetor[resp_vetor == "*"] <- 7L

        score_df[i, col_names] <- res_vetor

        score_orig <- process_score(resp_orig_string, gab_orig_string)

        if (sum(score_df[i, ] == 1, na.rm = TRUE) == sum(score_orig == 1)) {
          score_nu[i, ] <- sum(score_orig == 1)
        } else {
          stop(sprintf("Erro integridade na linha %d: Original %d != Novo %d", i, sum(score_orig == 1), sum(score_df[i, ] == 1, na.rm = TRUE)))
        }

      }

      if (i %% 5000 == 0) {
        cli::cli_status_update(
          id = cp,
          msg = "Processando {.field {area_loop}}: {i} / {n} ({round(i/n*100)}%)"
        )
      }
    }

    cli::cli_process_done(id = cp)

    # Preparação para exportação
    cli::cli_process_start("Consolidando matriz de scores")
    cols_mantidas <- c("NU_INSCRICAO",
                       paste0("TP_PRESENCA_", area_loop),
                       paste0("CO_PROVA_", area_loop),
                       paste0("NU_NOTA_", area_loop),
                       paste0("TX_RESPOSTAS_", area_loop),
                       paste0("TX_GABARITO_", area_loop))
    if ("TP_LINGUA" %in% names(data)) cols_mantidas <- c(cols_mantidas, "TP_LINGUA")

    score <- data[, ..cols_mantidas]
    score$NU_SCORE <- score_nu
    score <- cbind(score, as.data.table(score_df))
    cli::cli_process_done()

    # --- TRATAMENTO DO PATH E ESCRITA ---
    final_file <- if(grepl("\\.csv$", path_csv)) path_csv else file.path(path_csv, paste0("score_", area_loop, ".csv"))
    dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
    final_file <- normalizePath(final_file, mustWork = FALSE)

    cli::cli_process_start("Gravando {.path {basename(final_file)}}")
    data.table::fwrite(score, file = final_file)
    cli::cli_process_done()

    cli::cli_alert_success("Área {.field {area_loop}} finalizada.")
    gc()
  }

  cli::cli_alert_success("Processamento do ano {.val {ano}} concluído!")
  return(invisible(final_file))
}
