#' Exportar TCCs de todos os cadernos em um único JSON
#'
#' @param path_json String. Caminho completo do arquivo (ex: "caminho/tcc_completo.json").
#' @param ano Ano do exame (ex: 2019).
#' @export
write_tcc <- function(path_json, ano) {

  # --- TÍTULO ---
  cli::cli_h2("Processamento de Curvas de Score Esperado (TCC)")

  # 1. Recuperar objetos
  cli::cli_process_start("Recuperando objetos do Global Env")
  tryCatch({
    itens_df <- get(paste0("itens_", as.character(ano)), envir = .GlobalEnv)
    dic_df   <- get(paste0("dic_", as.character(ano)), envir = .GlobalEnv)
    consts   <- get("constantes", envir = .GlobalEnv)
    cli::cli_process_done()
  }, error = function(e) {
    cli::cli_alert_danger("Erro: Objetos não encontrados no Global Env.")
    stop(e)
  })

  # Filtros iniciais
  dic_df_P1 <- dic_df[dic_df$aplicacao == "P1", ]
  codigos <- unique(dic_df_P1$codigo)

  # 2. Definição da Estrutura Global
  Theta <- matrix(seq(-4, 4, length.out = 40))

  # Inicializamos a lista com labels de 0 a 1000
  lista_completa <- list(
    labels = seq(0, 1000, length.out = 40),
    datasets = list()
  )

  cp <- cli::cli_process_start("Consolidando datasets no objeto único")

  for (codigo in codigos) {
    area_nome <- dic_df$area[dic_df$codigo == codigo][1]
    cor_name <- dic_df$cor[dic_df$codigo == codigo][1]
    const_row <- consts[consts$area == area_nome, ]
    linguas <- if(area_nome == "LC") c(0, 1) else list(NULL)

    for (lingua in linguas) {
      itens_caderno <- itens_df[itens_df$CO_PROVA == codigo, ]
      key_name <- as.character(codigo)

      if (area_nome == "LC") {
        itens_caderno <- itens_caderno[order(itens_caderno$TP_LINGUA, itens_caderno$CO_POSICAO), ]
        condicao <- is.na(itens_caderno$TP_LINGUA) |
          itens_caderno$TP_LINGUA == "" |
          itens_caderno$TP_LINGUA == lingua
        itens_caderno <- itens_caderno[condicao, ]
        key_name <- paste0(codigo, "_", lingua)
      }

      # 3. Cálculos TRI
      itens_mirt <- data.frame(
        a1 = as.numeric(itens_caderno$NU_PARAM_A),
        d  = as.numeric(itens_caderno$NU_PARAM_A) * -as.numeric(itens_caderno$NU_PARAM_B),
        g  = as.numeric(itens_caderno$NU_PARAM_C)
      )

      mod_test <- mirtCAT::generate.mirt_object(itens_mirt, '3PL')
      escore_esperado <- mirt::expected.test(mod_test, Theta)

      # Cálculo do B médio na escala ENEM para o metadata
      b_medio_enem <- mean(as.numeric(itens_caderno$NU_PARAM_B), na.rm = TRUE) * const_row$k + const_row$d

      # 4. APPEND NO DATASETS
      novo_dataset <- list(
        label = key_name,
        metadata = list(
          codigo = as.numeric(codigo),
          area = area_nome,
          cor = cor_name,
          lingua = if(area_nome == "LC") as.numeric(lingua) else "N/A",
          b_medio_enem = round(b_medio_enem, 2)
        ),
        data = round(as.vector(escore_esperado), 2)
      )

      lista_completa$datasets[[length(lista_completa$datasets) + 1]] <- novo_dataset

      cli::cli_status_update(id = cp, msg = "Adicionado: {key_name} ({area_nome})")
    }
  }
  cli::cli_process_done(id = cp)

  # --- TRATAMENTO DO PATH ---
  final_file <- if(grepl("\\.json$", path_json)) path_json else file.path(path_json, "tcc.json")

  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  final_file <- normalizePath(final_file, mustWork = FALSE)

  cli::cli_process_start("Exportando arquivo JSON")
  jsonlite::write_json(lista_completa, path = final_file, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_process_done()

  cli::cli_alert_success("Processo concluído: {.path {final_file}}")

  return(invisible(final_file))
}
