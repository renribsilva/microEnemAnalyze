#' @title Gerar Informação do Item para Chart.js
#'
#' @description Esta função processa modelos MIRT para cadernos do ENEM, extrai as curvas de informação
#' do item e exporta os dados em formato JSON compatível com a estrutura do ApexCharts ou Chart.js.
#'
#' @param ano Inteiro ou caractere indicando o ano para buscar os objetos itens_ano e dic_ano.
#' @param path_json Opcional. String com o caminho do diretório ou nome do arquivo .json final.
#' @param co_prova Opcional. Código do caderno específico (CO_PROVA) para processamento individual.
#'
#' @export
write_iteminfo <- function(path_json = NULL, co_prova = NULL, ano) {

  # --- TÍTULO ---
  cli::cli_h2("Processamento de Curvas de Informação (Item Info) - ENEM {ano}")

  # 1. Recuperar os objetos da memória
  cli::cli_process_start("Buscando objetos no Global Environment")
  tryCatch({
    itens_df <- get(paste0("itens_", as.character(ano)), envir = .GlobalEnv)
    dic_df   <- get(paste0("dic_", as.character(ano)), envir = .GlobalEnv)
    cli::cli_process_done()
  }, error = function(e) {
    cli::cli_alert_danger("Objetos itens_{ano} ou dic_{ano} não encontrados.")
    stop(e)
  })

  if (!is.null(co_prova)) {
    co_prova_char <- as.character(co_prova)
    dic_df <- dic_df[dic_df$codigo == co_prova_char, ]
    if (nrow(dic_df) == 0) {
      cli::cli_alert_danger("Código {.val {co_prova}} não encontrado no dicionário.")
      return(NULL)
    }
  }

  # 2. Preparação Inicial
  cli::cli_process_start("Preparando estrutura MIRT")
  lista_modelos <- list()
  lista_nomes_itens <- list()
  lista_mask_na <- list()
  theta_vetor <- seq(-6, 6, by=0.1)
  Theta <- matrix(theta_vetor)
  cli::cli_process_done()

  # 3. Geração dos Modelos
  cp_mod <- cli::cli_process_start("Gerando modelos para os cadernos")
  for (codigo in dic_df$codigo) {
    dados_prova <- itens_df[itens_df$CO_PROVA == codigo, ]
    dados_prova <- dados_prova[order(dados_prova$TP_LINGUA, dados_prova$CO_POSICAO), ]

    if (nrow(dados_prova) > 0) {
      lista_nomes_itens[[as.character(codigo)]] <- as.character(dados_prova$CO_ITEM)
      lista_mask_na[[as.character(codigo)]] <- is.na(dados_prova$NU_PARAM_A) | is.na(dados_prova$NU_PARAM_B)

      a_vals <- as.numeric(dados_prova$NU_PARAM_A)
      b_vals <- as.numeric(dados_prova$NU_PARAM_B)
      g_vals <- as.numeric(dados_prova$NU_PARAM_C)

      a_vals[is.na(a_vals)] <- 1
      b_vals[is.na(b_vals)] <- 0
      g_vals[is.na(g_vals)] <- 0

      df_mirt <- data.frame(
        a1 = a_vals,
        d  = a_vals * -b_vals,
        g  = g_vals
      )

      tryCatch({
        lista_modelos[[as.character(codigo)]] <- mirtCAT::generate.mirt_object(df_mirt, '3PL')
        cli::cli_status_update(id = cp_mod, msg = "Modelo gerado: {codigo} ({nrow(df_mirt)} itens)")
      }, error = function(e) {
        cli::cli_alert_danger("Erro no modelo {.val {codigo}}: {e$message}")
      })
    }
  }
  cli::cli_process_done(id = cp_mod)

  # 4. Extração de Informação e Formatação
  output_final <- list(
    theta_labels = as.vector(theta_vetor),
    datasets = list()
  )

  retorno_ambiente <- NULL
  cp_ext <- cli::cli_process_start("Extraindo curvas de informação")

  for (codigo in names(lista_modelos)) {
    tryCatch({
      mod <- lista_modelos[[codigo]]
      n_itens <- length(lista_nomes_itens[[codigo]])

      # mirt::iteminfo calcula a info para cada item individualmente
      # Criamos uma matriz para armazenar: linhas = theta, colunas = itens
      matriz_info <- matrix(0, nrow = nrow(Theta), ncol = n_itens)

      for(j in 1:n_itens) {
        # Extraímos a info do item j do modelo
        matriz_info[, j] <- mirt::iteminfo(mirt::extract.item(mod, j), Theta)
      }

      if (!is.null(co_prova) && codigo == as.character(co_prova)) {
        retorno_ambiente <- matriz_info
        nome_obj <- paste0("iteminfo_", codigo)
        assign(nome_obj, matriz_info, envir = .GlobalEnv)
        cli::cli_alert_success("Objeto {.var {nome_obj}} criado no Global Env.")
      }

      ids_reais <- lista_nomes_itens[[codigo]]
      mask_na <- lista_mask_na[[codigo]]
      itens_list <- list()

      for (i in 1:n_itens) {
        if (mask_na[i]) {
          itens_list[[ids_reais[i]]] <- rep(NA, nrow(Theta))
        } else {
          itens_list[[ids_reais[i]]] <- as.vector(matriz_info[, i])
        }
      }

      output_final$datasets[[codigo]] <- itens_list
      cli::cli_status_update(id = cp_ext, msg = "Caderno {codigo} processado")
    }, error = function(e) {
      cli::cli_alert_danger("Erro ao extrair dados do caderno {.val {codigo}}: {e$message}")
    })
  }
  cli::cli_process_done(id = cp_ext)

  # 5. Exportação
  if (!is.null(path_json)) {
    cli::cli_process_start("Exportando arquivo JSON")
    final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, paste0("iteminfo_", ano, ".json"))
    final_path_clean <- path.expand(final_file)
    dir.create(dirname(final_path_clean), showWarnings = FALSE, recursive = TRUE)

    jsonlite::write_json(output_final, path = final_path_clean, pretty = TRUE, auto_unbox = TRUE, digits = 4, na = "null")
    cli::cli_process_done()
    cli::cli_alert_success("Processo concluído: {.path {final_path_clean}}")
  }

  if (!is.null(retorno_ambiente)) {
    return(invisible(retorno_ambiente))
  } else if (!is.null(path_json)) {
    return(invisible(final_path_clean))
  } else {
    return(invisible(output_final))
  }
}
