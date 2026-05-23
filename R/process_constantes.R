#' @title Calcular Constantes de Transformação por Área
#'
#' @description Esta função processa as respostas,
#' calcula a verossimilhança via TRI (EAP)
#' e estima as constantes de transformação (k e d) para
#' equalização de escalas, tendo como referencia o exame
#' aplicado no ano de 2009.
#'
#' @param sample Um data.table ou data.table contendo os
#' microdados (respostas, gabaritos e notas).
#' @param area Uma string indicando a área do conhecimento
#' (ex: "LC", "MT", "CH", "CN").
#' @param itens_db Um data.table contendo os parâmetros
#' dos itens (A, B, C) e códigos de prova.
#'
#' @return Uma lista contendo a constante de escala (k),
#' a constante de deslocamento (d) e a área processada.
#' @export
process_constantes <- function(sample, area) {
  cli::cli_h1("Contanstes para a transformacao das escalas do ENEM")

  # Inicia a validação dos argumentos
  cli::cli_process_start("Validando argumentos")

  if (missing(sample)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg sample} e obrigatorio.",
      "i" = "Por favor, forneca os microdados do ENEM de 2009."
    ))
  }

  if (missing(area)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg area} e obrigatorio.",
      "i" = "Exemplos de areas validas:
      {.val LC}, {.val MT}, {.val CH} ou {.val CN}."
    ))
  }

  # Normaliza os argumentos
  if (!data.table::is.data.table(sample)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    sample <- data.table::as.data.table(sample)
  }
  area <- toupper(as.character(area))

  cli::cli_process_done()

  # Constrói nomes das colunas dinamicamente
  col_resp <- paste0("TX_RESPOSTAS_", area)
  col_gaba <- paste0("TX_GABARITO_", area)
  col_prov <- paste0("CO_PROVA_", area)
  col_nota <- paste0("NU_NOTA_", area)

  # Prepara algumas variáveis
  score_list <- list()
  co_prova_list <- character()
  keep_idx <- c()

  cli::cli_progress_bar(paste("Processando Scores", area), total = nrow(sample))

  # Itera sobre as linhas de sample
  for (i in seq_len(nrow(sample))) {
    cli::cli_progress_update()

    # Cria uma lista contendo dois itens: sc (score) e co (código da prova)
    res <- tryCatch(
      {
        list(
          sc = process_score(sample[[col_resp]][i], sample[[col_gaba]][i]), # nolint: object_usage_linter
          co = sample[[col_prov]][i]
        )
      },
      error = function(e) {
        NULL
      }
    )

    # Se res não for nulo, incrementa as variáveis com novos valores
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

  # Prepara variáveis e funções para calcular a verossimilhança
  theta <- seq(-4, 4, length.out = 40)

  cci_3pl <- function(theta, a, b, c) {
    c + ((1 - c) / (1 + exp(-a * (theta - b))))
  }

  ls_traceline <- list()

  # Importa dataset itens_2009
  itens_db <- get("itens_2009")

  # Inicia a iteração para cada código de prova
  for (k in unique(co_prova)) {
    # Filtra dataset itens_2009 e ordena
    pars <- itens_db[itens_db$CO_PROVA == k, ]
    pars <- pars[order(pars$CO_POSICAO), ]

    # Se não houver itens para o código k, segue para o próximo código
    if (nrow(pars) == 0) {
      next
    }

    # Para cada código (prova distinta), retorna um vetor com
    # 40 probabilidades tanto de acertos (p1) quanto de erros (p0)
    # para todos os itens da prova
    ls_traceline[[as.character(k)]] <- lapply(
      seq_len(nrow(pars)),
      function(idx) {
        p_vector <- cci_3pl(
          theta,
          pars$NU_PARAM_A[idx],
          pars$NU_PARAM_B[idx],
          pars$NU_PARAM_C[idx]
        )
        list(p1 = p_vector, p0 = 1 - p_vector)
      }
    )
  }

  # 3. Verossimilhança Vetorizada
  prod_prob <- list()

  cli::cli_progress_bar(
    paste("Calculando Likelihood", area),
    total = nrow(sample_f)
  )

  # Itera sobre sample com observações válidas
  for (m in seq_len(nrow(sample_f))) {
    cli::cli_progress_update()

    # Filtra ls_traceline de acordo com o código da prova sob análise
    traceline_prova <- ls_traceline[[as.character(co_prova[m])]]

    # Para cada item válido da prova, retorna um vetor com
    # 40 probabilidades, sendo que cada uma é associada a uma
    # possível proficiência (-4 < theta < 4)
    list_probs <- lapply(seq_along(traceline_prova), function(q) {
      res <- score[m, q]
      it <- traceline_prova[[q]]

      # Se o item não tem parâmetro ou a resposta
      # é inválida, probabilidade neutra (1)
      if (is.na(res) || any(is.na(it$p1))) {
        return(rep(1, length(theta)))
      }

      if (res == 1) {
        it$p1
      } else {
        it$p0
      }
    })

    # Faz o produtório dasa probabilidades de erros e acertos
    # para cada uma das 40 proficiências pré-estabelecidas (theta),
    # guardando o resultado um vetor com 40 valores de verossimilhança.
    prod_prob[[m]] <- Reduce(`*`, list_probs)
  }

  cli::cli_progress_done()

  # Calula EAP e Constantes
  p_theta <- stats::dnorm(theta, mean = 0, sd = 1)
  theta_eap <- sapply(prod_prob, function(l_theta) {
    posterior <- l_theta * p_theta
    sum(theta * posterior) / sum(posterior)
  })

  media_x <- mean(theta_eap, na.rm = TRUE)
  dp_x <- stats::sd(theta_eap, na.rm = TRUE)
  media_y <- mean(sample_f[[col_nota]], na.rm = TRUE)
  dp_y <- stats::sd(sample_f[[col_nota]], na.rm = TRUE)

  k_const <- dp_y / dp_x
  d_const <- media_y - (k_const * media_x)

  list(k = k_const, d = d_const, area = area)
}
