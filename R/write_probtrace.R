#' @title Gerar Probtrace para Chart.js
#'
#' @description Esta função processa modelos MIRT para cadernos do ENEM, extrai as curvas de probabilidade
#' de acerto e exporta os dados em formato JSON compatível com a estrutura do Chart.js.
#'
#' @param ano Inteiro ou caractere indicando o ano (ex: 2019) para buscar os objetos itens_ano e dic_ano.
#' @param path_json Opcional. String com o caminho do diretório ou nome do arquivo .json final.
#' Se for NULL, o arquivo não será escrito.
#' @param co_prova Opcional. Código do caderno específico (CO_PROVA) para processamento individual.
#'
#' @return Se co_prova for informado, cria o objeto no GlobalEnv e retorna a matriz. Caso contrário, retorna o caminho do JSON (invisível).
#'
#' @import mirtCAT
#' @import cli
#' @import jsonlite
#' @export
write_probtrace <- function(path_json = NULL, co_prova = NULL, ano) {

  # 1. Recuperar os objetos da memória
  tryCatch({
    itens_df <- get(paste0("itens_", as.character(ano)), envir = .GlobalEnv)
    dic_df   <- get(paste0("dic_", as.character(ano)), envir = .GlobalEnv)
  }, error = function(e) {
    cli::cli_alert_danger("Objetos itens_{ano} ou dic_{ano} não encontrados no Global Environment.")
    stop(e)
  })

  # Filtrar dic_df se um co_prova específico for solicitado
  if (!is.null(co_prova)) {
    co_prova_char <- as.character(co_prova)
    dic_df <- dic_df[dic_df$codigo == co_prova_char, ]
    if (nrow(dic_df) == 0) {
      cli::cli_alert_danger("Código {.val {co_prova}} não encontrado no dicionário.")
      return(NULL)
    }
  }

  # 2. Preparação Inicial
  cli::cli_h1("Iniciando Processamento MIRT - ENEM {ano}")
  lista_modelos <- list()
  theta_vetor <- seq(-4, 4, length.out = 40)
  Theta <- matrix(theta_vetor)

  # 3. Geração dos Modelos
  for (codigo in dic_df$codigo) {
    dados_prova <- itens_df[itens_df$CO_PROVA == codigo, ]
    dados_prova <- dados_prova[order(dados_prova$TP_LINGUA, dados_prova$CO_POSICAO), ]

    if (nrow(dados_prova) > 0) {
      df_mirt <- data.frame(
        a1 = as.numeric(dados_prova$NU_PARAM_A),
        d  = as.numeric(dados_prova$NU_PARAM_A) * -as.numeric(dados_prova$NU_PARAM_B),
        g  = as.numeric(dados_prova$NU_PARAM_C)
      )

      tryCatch({
        lista_modelos[[as.character(codigo)]] <- mirtCAT::generate.mirt_object(df_mirt, '3PL')
        cli::cli_alert_success("Modelo gerado: {.val {codigo}} ({nrow(df_mirt)} itens)")
      }, error = function(e) {
        cli::cli_alert_danger("Erro no modelo {.val {codigo}}: {e$message}")
      })
    }
  }

  # 4. Extração de Probabilidades e Formatação
  output_final <- list(
    theta_labels = as.vector(theta_vetor),
    datasets = list()
  )

  retorno_ambiente <- NULL

  cli::cli_h2("Extraindo Probtrace")

  for (codigo in names(lista_modelos)) {
    tryCatch({
      tracos <- mirt::probtrace(lista_modelos[[codigo]], Theta)

      # Se for o caderno específico, prepara para salvar no Environment
      if (!is.null(co_prova) && codigo == as.character(co_prova)) {
        retorno_ambiente <- tracos
        nome_obj <- paste0("probtrace_", codigo)
        assign(nome_obj, tracos, envir = .GlobalEnv)
        cli::cli_alert_success("Objeto {.var {nome_obj}} criado no Global Environment.")
      }

      colunas_acerto <- seq(2, ncol(tracos), by = 2)
      prob_acertos <- tracos[, colunas_acerto]

      itens_list <- list()
      for (i in 1:ncol(prob_acertos)) {
        itens_list[[paste0("Item_", i)]] <- prob_acertos[, i]
      }

      output_final$datasets[[codigo]] <- itens_list
      cli::cli_alert_info("Caderno {.val {codigo}} processado.")
    }, error = function(e) {
      cli::cli_alert_danger("Erro ao extrair dados do caderno {.val {codigo}}")
    })
  }

  # 5. Exportação Condicional
  if (!is.null(path_json)) {
    final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, paste0("probtrace_", ano, ".json"))
    final_path_clean <- path.expand(final_file)
    dir.create(dirname(final_path_clean), showWarnings = FALSE, recursive = TRUE)

    jsonlite::write_json(output_final, path = final_path_clean, pretty = TRUE, auto_unbox = TRUE, digits = 4)
    cli::cli_alert_success("JSON exportado para: {.path {final_path_clean}}")
  }

  # Lógica de Retorno
  if (!is.null(retorno_ambiente)) {
    return(invisible(retorno_ambiente))
  } else if (!is.null(path_json)) {
    return(invisible(final_path_clean))
  } else {
    return(invisible(output_final))
  }
}
