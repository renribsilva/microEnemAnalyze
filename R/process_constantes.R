#' Calcular Constantes de Transformação por Área
#'
#' Esta função processa as respostas, calcula a verossimilhança via TRI (EAP)
#' e estima as constantes de transformação (k e d) para equalização de escalas.
#'
#' @param sample Um data.frame ou data.table contendo os microdados (respostas, gabaritos e notas).
#' @param area Uma string indicando a área do conhecimento (ex: "LC", "MT", "CH", "CN").
#' @param itens_db Um data.frame contendo os parâmetros dos itens (A, B, C) e códigos de prova.
#'
#' @importFrom stats dnorm sd
#' @import cli
#'
#' @return Uma lista contendo a constante de escala (k), a constante de deslocamento (d) e a área processada.
#' @export
process_constantes <- function(sample, area, itens_db) {

  # Seleciona nomes das colunas dinamicamente
  col_resp <- paste0("TX_RESPOSTAS_", area)
  col_gaba <- paste0("TX_GABARITO_", area)
  col_prov <- paste0("CO_PROVA_", area)
  col_nota <- paste0("NU_NOTA_", area)

  # 1. Processamento de Scores
  score_list <- list(); co_prova_list <- character(); keep_idx <- c()
  cli::cli_progress_bar(paste("Processando Scores", area), total = nrow(sample))

  for (i in 1:nrow(sample)) {
    cli::cli_progress_update()
    res <- tryCatch({
      list(
        sc = process_score(sample[[col_resp]][i], sample[[col_gaba]][i]),
        co = sample[[col_prov]][i]
      )
    }, error = function(e) return(NULL))

    if (!is.null(res)) {
      score_list[[length(score_list) + 1]] <- res$sc
      co_prova_list[length(co_prova_list) + 1] <- res$co
      keep_idx <- c(keep_idx, i)
    }
  }
  score <- do.call(rbind, score_list)
  co_prova <- co_prova_list
  sample_f <- sample[keep_idx, ]
  cli::cli_progress_done()

  # 2. Traceline
  theta <- seq(-4, 4, length.out = 40)
  cci_3pl <- function(theta, a, b, c) c + ((1 - c) / (1 + exp(-a * (theta - b))))
  ls_traceline <- list()

  for (k in unique(co_prova)) {
    pars <- itens_db[itens_db$CO_PROVA == k, ]
    pars <- pars[order(pars$CO_POSICAO), ]
    if (nrow(pars) == 0) next # Evita crash se caderno não existir
    ls_traceline[[as.character(k)]] <- lapply(1:nrow(pars), function(idx) {
      p_vector <- cci_3pl(theta, pars$NU_PARAM_A[idx], pars$NU_PARAM_B[idx], pars$NU_PARAM_C[idx])
      list(p1 = p_vector, p0 = 1 - p_vector)
    })
  }

  # 3. Verossimilhança Vetorizada (Para evitar o erro de NA e índice)
  prod_prob <- list()
  cli::cli_progress_bar(paste("Calculando Likelihood", area), total = nrow(sample_f))
  for (m in 1:nrow(sample_f)) {
    cli::cli_progress_update()
    traceline_prova <- ls_traceline[[as.character(co_prova[m])]]
    list_probs <- lapply(1:length(traceline_prova), function(q) {
      res <- score[m, q]
      it <- traceline_prova[[q]]
      if (is.na(res) || any(is.na(it$p1))) return(rep(1, length(theta)))
      if (res == 1) return(it$p1) else return(it$p0)
    })
    prod_prob[[m]] <- Reduce(`*`, list_probs)
  }

  cli::cli_progress_done()

  # 4. EAP e Constantes
  p_theta <- stats::dnorm(theta, mean = 0, sd = 1)
  theta_EAP <- sapply(prod_prob, function(L_theta) {
    posterior <- L_theta * p_theta
    sum(theta * posterior) / sum(posterior)
  })

  media_x <- mean(theta_EAP, na.rm = TRUE); dp_x <- stats::sd(theta_EAP, na.rm = TRUE)
  media_y <- mean(sample_f[[col_nota]], na.rm = TRUE); dp_y <- stats::sd(sample_f[[col_nota]], na.rm = TRUE)

  k_const <- dp_y / dp_x
  d_const <- media_y - (k_const * media_x)

  return(list(k = k_const, d = d_const, area = area))
}
