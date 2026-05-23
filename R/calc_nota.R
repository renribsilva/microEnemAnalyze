#' @title Calcular Nota do Enem
#' @description Calcula a proficiência dos participantes do ENEM para
#' um determinado ano e uma determinada área do conhecimento.
#' O método divulgado pelo INEP e seus pesquisadore é o 'Expected
#' a posteriori' (EAP), uma forma de obter a média das proficiências
#' relacionadas a uma determinada sequência de acertos e erros.
#' @param sample Um subconjunto dos microdados do ENEM, o qual pode conter
#' uma linha ou mais.
#' @param area String que indica a área do conhecimento: "LC", "CH", "CH", "MT"
#' @param ano Número que indica o ano do exame, que deve ser maior ou
#' igual a 2019.
#' @export
calc_nota <- function(sample, area, ano) {
  # --- TÍTULO ---
  cli::cli_h1("Calculando nota: metrica TRI")

  cli::cli_process_start("Validando argumentos")

  if (missing(sample)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg sample} e obrigatorio.",
      "i" = "Por favor, forneca um subconjunto dos microdados
      do ENEM, que tenha ao menos uma linha."
    ))
  }

  if (missing(area)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg area} e obrigatorio.",
      "i" = "Exemplos de areas validas:
      {.val LC}, {.val MT}, {.val CH} ou {.val CN}."
    ))
  }

  if (missing(ano)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg ano} e obrigatorio.",
      "i" = "Informe o ano de referencia da aplicacao (ex: {.val 2019})."
    ))
  }

  if (!is.character(area)) {
    cli::cli_abort(
      "{.arg area} precisa ser do tipo {.cls character}."
    )
  }

  if (!is.numeric(ano) || !is.integer(ano)) {
    cli::cli_abort(
      "{.arg ano} precisa ser do tipo {.cls number} ou {.cls integer}."
    )
  }

  # normaliza os argumentos
  if (!data.table::is.data.table(sample)) {
    cli::cli_alert_info(
      "Convertendo objeto para {.cls data.table}"
    )
    sample <- data.table::as.data.table(sample)
  }
  area <- toupper(as.character(area))
  ano <- as.integer(ano)

  if (ano < 2019) {
    cli::cli_abort(
      "x" = "{.arg ano} invalido",
      "i" = "Informe um ano maior ou igual a {.val 2019}."
    )
  }

  cli::cli_process_done()

  cli::cli_process_start("Preparando variaveis e arquivo .rda")

  # constrói o nome do rda dinamicamente
  nome_itens <- paste0("itens_", ano)

  # verifica se o arquivo rda exite
  if (!exists(nome_itens)) {
    cli::cli_abort(c(
      "x" = "O objeto {.var {nome_itens}} nao foi encontrado na memoria.",
      "i" = "Certifique-se de que o arquivo {.file
      {paste0(nome_itens, '.rda')}} exite em {.path ./data}."
    ))
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
      cli::cli_abort(c(
        "x" = "Selecao dos itens do caderno {. var nome_itens} falhou.",
        "i" = "Verifique se no caderno {.var nome_itens} ha itens
          que pertencem a prova {.var cod_prova}."
      ))
    }

    # Ordenação inicial para garantir Inglês/Espanhol
    pars <- pars[order(pars$TP_LINGUA, pars$CO_POSICAO), ]

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

    tamanho_resp <- nchar(resp)
    tamanho_gaba <- nchar(gaba)

    if (tamanho_resp != tamanho_gaba) {
      cli::cli_abort(c(
        "x" = "Vetor de resposta e vetor de gabarito tem tamanhos
        diferentes: {.var tamanho_resp} vs. {.var tamanho_gaba},
        respectivamente.",
        "i" = "Verifique se os microdados do ENEM do ano {.agr ano}
        apresentam um padrao diferente em relacao as coluna TX_RESPOSTA_
        e TX_GABARITO_"
      ))
    }

    score_i <- process_score(resp, gaba) # nolint: object_usage_linter
    if (length(idx_anulados) > 0) {
      score_i <- score_i[-idx_anulados] # Remove do score
      pars <- pars[-idx_anulados, ] # Remove do banco
    }

    tamanho_score_i <- length(score_i)
    tamanho_pars <- nrow(pars)

    if (tamanho_score_i != tamanho_pars) {
      cli::cli_abort(c(
        "x" = "Matriz de score e caderno de itens apresentaram tamanhos
        diferentes apos o tratamento de itens anulados:
        {.var tamanho_score_i} vs. {.var tamanho_pars},
        respectivamente.",
        "i" = "Verifique as funcoes {.fun process_score} e o caderno do ano
        {.arg ano}."
      ))
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

  cli::cli_process_start(
    "Calculando a nota media por EAP"
  )
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

  cli::cli_process_done()

  cli::cli_alert_success("Nota: {nota_final}")

  invisible(nota_final)
}
