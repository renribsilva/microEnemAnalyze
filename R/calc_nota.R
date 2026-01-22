#' Calcular Nota Final (EAP Transformado)
#' @export
calc_nota <- function(sample, area, ano) {

  nome_itens <- paste0("itens_", ano)
  if (!exists(nome_itens)) stop(paste("Objeto", nome_itens, "nao encontrado."))
  itens_db_total <- get(nome_itens)

  col_resp <- paste0("TX_RESPOSTAS_", area)
  col_gaba <- paste0("TX_GABARITO_", area)
  col_prov <- paste0("CO_PROVA_", area)

  theta <- seq(-4, 4, length.out = 40)
  cci_3pl <- function(theta, a, b, c) c + ((1 - c) / (1 + exp(-a * (theta - b))))
  p_theta <- stats::dnorm(theta, mean = 0, sd = 1)

  prod_prob <- list()
  cli::cli_progress_bar(paste("Calculando Notas", area), total = nrow(sample))

  for (i in 1:nrow(sample)) {
    cli::cli_progress_update()

    resp <- sample[[col_resp]][i]
    gaba <- sample[[col_gaba]][i]
    cod_prova <- sample[[col_prov]][i]

    # 1. PEGA O BANCO DO CADERNO E ORDENA
    pars <- itens_db_total[itens_db_total$CO_PROVA == cod_prova, ]
    if (nrow(pars) == 0) { prod_prob[[i]] <- NULL; next }

    # Ordenação inicial para garantir Inglês/Espanhol
    if (ano > 2009) {
      pars <- pars[base::order(pars$TP_LINGUA, pars$CO_POSICAO), ]
    } else {
      pars <- pars[base::order(pars$CO_POSICAO), ]
    }

    # 2. TRATAMENTO LÍNGUA (FILTRA STRING E BANCO SIMULTANEAMENTE)
    if (area == "LC" && nchar(resp) == 50) {
      lg <- sample$TP_LINGUA[i]
      if (lg == 1) { # ESPANHOL
        resp <- substr(resp, 6, 50)
        gaba <- substr(gaba, 6, 50)
        pars <- pars[!(pars$TP_LINGUA == 0 & pars$CO_POSICAO %in% 1:5), ]
      } else { # INGLÊS
        resp <- paste0(substr(resp, 1, 5), substr(resp, 11, 50))
        gaba <- paste0(substr(gaba, 1, 5), substr(gaba, 11, 50))
        pars <- pars[!(pars$TP_LINGUA == 1 & pars$CO_POSICAO %in% 6:10), ]
      }
    }

    # Garante ordenação final por posição para bater com a string
    pars <- pars[base::order(pars$CO_POSICAO), ]

    # 3. REMOÇÃO DE ITENS ANULADOS (IN_ITEM_ABAN == 1)
    # Identificamos quais posições da string/score devem sumir
    idx_anulados <- which(pars$IN_ITEM_ABAN == 1)

    score_i <- process_score(resp, gaba) # Gera vetor de 0 e 1 (45 ou 50 itens)

    if (length(idx_anulados) > 0) {
      score_i <- score_i[-idx_anulados] # Remove do score
      pars <- pars[-idx_anulados, ]     # Remove do banco
    }

    # 4. CÁLCULO DA LIKELIHOOD (LINHA POR LINHA)
    n_itens <- min(length(score_i), nrow(pars))
    list_probs <- lapply(1:n_itens, function(q) {
      res <- score_i[q]
      p_item <- pars[q, ]

      # Se o item não tem parâmetro ou a resposta é inválida, probabilidade neutra (1)
      if (is.na(res) || is.na(p_item$NU_PARAM_A)) return(rep(1, length(theta)))

      p1 <- cci_3pl(theta, p_item$NU_PARAM_A, p_item$NU_PARAM_B, p_item$NU_PARAM_C)
      return(if (res == 1) p1 else (1 - p1))
    })

    prod_prob[[i]] <- Reduce(`*`, list_probs)
  }
  cli::cli_progress_done()

  # 5. EAP E TRANSFORMAÇÃO
  # Remove nulos (casos onde o caderno não foi encontrado)
  prod_prob <- prod_prob[!sapply(prod_prob, is.null)]

  posterior <- L_theta * p_theta
  theta_EAP <- sapply(prod_prob, function(L_theta) {
    sum(theta * posterior) / sum(posterior)
  })

  k_val <- constantes[constantes$area == area, 'k']
  d_val <- constantes[constantes$area == area, 'd']

  return(round(theta_EAP * k_val + d_val, 1))
}
