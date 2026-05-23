#' Calcular Nota Final (EAP Transformado)
#' @param sample Um data.table contendo linhas dos microdados
#' do ENEM
#' @param area String que indica a área do conhecimento: "LC", "CH", "CH", "MT"
#' @param ano Número que indica o ano do exame
#' @export
calc_nota <- function(sample, area, ano) {
  # --- TÍTULO ---
  cli::cli_h1("Calculando nota: metrica TRI")

  # Validação básica
  cli::cli_process_start("Validando argumentos")

  # verifica se sample é data.table
  if (!data.table::is.data.table(sample)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    sample <- data.table::as.data.table(sample)
  }

  # verifica se ano e área foram definidos
  if (is.null(area)) {
    stop(
      "Area nao definida. E necessario definir uma das seguintes
      areas: 'LC', 'CH', 'CN', 'MT'"
    )
  }

  # verifica se ano e área foram definidos
  if (is.null(ano)) {
    stop(
      "Ano nao definido. E necessario definir um ano igual
      ou maior que 2019"
    )
  }

  cli::cli_process_done()

  cli::cli_process_start("Verificando variaveis e arquivo .rda")

  # constrói o nome do rda dinamicamente
  nome_itens <- paste0("itens_", ano)

  # verifica se o arquivo rda exite
  if (!exists(nome_itens)) {
    stop(paste("Objeto", nome_itens, "nao encontrado."))
  }

  # importa o caderno de itens .rda
  itens_db_total <- get(nome_itens)

  # constrói os nomes das colunas dinamicamente
  col_resp <- paste0("TX_RESPOSTAS_", area)
  col_gaba <- paste0("TX_GABARITO_", area)
  col_prov <- paste0("CO_PROVA_", area)

  # prepara a função que calcula a proficiência
  theta <- seq(-4, 4, length.out = 40)
  p_theta <- stats::dnorm(theta, mean = 0, sd = 1)
  cci_3pl <- function(theta, a, b, c) {
    c + ((1 - c) / (1 + exp(-a * (theta - b))))
  }
  prod_prob <- list()

  cli::cli_process_done()

  cli::cli_progress_bar(
    paste("Calculando a nota na area: ", area),
    total = nrow(sample)
  )

  # inicia a iteração sobre o sample
  for (i in seq_len(nrow(sample))) {
    cli::cli_progress_update()

    resp <- sample[[col_resp]][i]
    gaba <- sample[[col_gaba]][i]
    cod_prova <- sample[[col_prov]][i]

    # 1. PEGA O BANCO DO CADERNO E ORDENA
    pars <- itens_db_total[itens_db_total$CO_PROVA == cod_prova, ]
    if (nrow(pars) == 0) {
      prod_prob[[i]] <- NULL
      stop(
        "Comprimento do caderno de itens e zero.
        Verifique se o codigo da prova e valido."
      )
    }

    # Ordenação inicial para garantir Inglês/Espanhol
    if (ano > 2009) {
      pars <- pars[order(pars$TP_LINGUA, pars$CO_POSICAO), ]
    } else {
      pars <- pars[order(pars$CO_POSICAO), ]
    }

    # 2. TRATAMENTO LÍNGUA (FILTRA STRING E BANCO SIMULTANEAMENTE)
    if (area == "LC" && nchar(resp) == 50) {
      lg <- sample$TP_LINGUA[i]
      if (lg == 1) {
        # ESPANHOL
        resp <- substr(resp, 6, 50)
        gaba <- substr(gaba, 6, 50)
        pars <- pars[!(pars$TP_LINGUA == 0 & pars$CO_POSICAO %in% 1:5), ]
      } else {
        # INGLÊS
        resp <- paste0(substr(resp, 1, 5), substr(resp, 11, 50))
        gaba <- paste0(substr(gaba, 1, 5), substr(gaba, 11, 50))
        pars <- pars[!(pars$TP_LINGUA == 1 & pars$CO_POSICAO %in% 6:10), ]
      }
    }

    # Garante ordenação final por posição para bater com a string
    pars <- pars[order(pars$CO_POSICAO), ]

    # 3. REMOÇÃO DE ITENS ANULADOS (IN_ITEM_ABAN == 1)
    # Identificamos quais posições da string/score devem sumir
    idx_anulados <- which(pars$IN_ITEM_ABAN == 1)

    if (nchar(resp) != nchar(gaba)) {
      stop(sprintf(
        "Tamanho invalido: O vetor resp tem %d caracteres e vetor
        gaba tem %d. Deveriam ter 45 ou 50.",
        resp,
        gaba
      ))
    }

    score_i <- process_score(resp, gaba) # nolint: object_usage_linter
    if (length(idx_anulados) > 0) {
      score_i <- score_i[-idx_anulados] # Remove do score
      pars <- pars[-idx_anulados, ] # Remove do banco
    }

    if (length(score_i) != nrow(pars)) {
      stop(
        "Numero de itens do cardeno e diferente do numero
        de itens do score."
      )
    }

    list_probs <- lapply(
      seq_along(score_i),
      function(q) {
        res <- score_i[q]
        p_item <- pars[q, ]

        # Se o item não tem parâmetro ou a resposta
        # é inválida, probabilidade neutra (1)
        if (is.na(res) || is.na(p_item$NU_PARAM_A)) {
          return(rep(1, length(theta)))
        }

        p1 <- cci_3pl(
          theta,
          p_item$NU_PARAM_A,
          p_item$NU_PARAM_B,
          p_item$NU_PARAM_C
        )
        if (res == 1) p1 else (1 - p1)
      }
    )

    prod_prob[[i]] <- Reduce(`*`, list_probs)
  }

  cli::cli_progress_done()

  # 5. EAP E TRANSFORMAÇÃO
  # Remove nulos (casos onde o caderno não foi encontrado)
  prod_prob <- prod_prob[!sapply(prod_prob, is.null)]

  theta_eap <- sapply(prod_prob, function(l_theta) {
    posterior <- l_theta * p_theta
    sum(theta * posterior) / sum(posterior)
  })

  constantes_dt <- get("constantes")

  k_val <- constantes_dt[constantes_dt$area == area, "k"]
  d_val <- constantes_dt[constantes_dt$area == area, "d"]
  nota_final <- round(theta_eap * k_val + d_val, 1)

  cli::cli_alert_success("Nota: {nota_final}")

  invisible(nota_final)
}
