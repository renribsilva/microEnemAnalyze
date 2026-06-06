#' @title Exportar TCCs de todos os cadernos em um único JSON
#'
#' @description Essa função escreve um JSON com dados sobre a
#' curva característica do exame, por área.
#'
#' @param data Data.table com os microdados do ENEM.
#' @param path_json String com o caminho completo do arquivo.
#'
#' @export
write_describe_notas <- function(data, path_json) {
  cli::cli_h1("Descricao Estatistica")

  cli::cli_process_start("Validando argumentos")

  if (missing(data)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg data} e obrigatorio.",
      "i" = "Por favor, forneca os microdados do ENEM."
    ))
  }

  if (missing(path_json)) {
    cli::cli_abort(c(
      "x" = "O argumento {.arg path_csv} e obrigatorio.",
      "i" = "Por favor, forneca o caminho onde o csv sera gravado."
    ))
  }

  if (!is.character(path_json)) {
    cli::cli_abort("{.arg path_csv} precisa ser do tipo character.")
  }

  # Normaliza os microdados
  if (!data.table::is.data.table(data)) {
    cli::cli_alert_info("Convertendo objeto para {.cls data.table}")
    data <- data.table::as.data.table(data)
  }

  cli::cli_process_done()

  col_nota <- grep("^NU_NOTA_", names(data), value = TRUE)
  col_prova <- grep("^CO_PROVA_", names(data), value = TRUE)
  col_score <- "NU_SCORE"

  # --- Helpers Mínimos ---
  get_mode <- function(x) {
    ux <- unique(stats::na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
  }

  get_cor_from_dic <- function(codigo_procurado, dicionario) {
    cor_encontrada <- dicionario$cor[dicionario$codigo == codigo_procurado]
    cor_encontrada[1]
  }

  get_area_from_dic <- function(codigo_procurado, dicionario) {
    area_encontrada <- dicionario$area[dicionario$codigo == codigo_procurado]
    area_encontrada[1]
  }

  # --- Função interna para rodar os dois pools ---
  calc_stats <- function(codigos_pool) {
    df_pool <- data[
      get(col_prova) %in%
        codigos_pool &
        get(col_nota) > 0 &
        !is.na(get(col_nota))
    ]
    if (nrow(df_pool) == 0) {
      return(NULL)
    }

    idx_min <- which.min(df_pool[[col_nota]])
    idx_max <- which.max(df_pool[[col_nota]])

    c_min <- df_pool[[col_prova]][idx_min]
    c_max <- df_pool[[col_prova]][idx_max]

    area_prova <- get_area_from_dic(c_min, dic_df)

    lang_min <- ""
    lang_max <- ""

    if ("TP_LINGUA" %in% names(df_pool) && identical(area_prova, "LC")) {
      tp_min <- df_pool$TP_LINGUA[idx_min]
      tp_max <- df_pool$TP_LINGUA[idx_max]

      if (!is.na(tp_min)) {
        lang_min <- paste0(" (", ifelse(tp_min == 0, "Ingles", "Espanhol"), ")")
      }

      if (!is.na(tp_max)) {
        lang_max <- paste0(" (", ifelse(tp_max == 0, "Ingles", "Espanhol"), ")")
      }
    }

    v_n <- df_pool[[col_nota]]

    d_n <- as.list(psych::describe(v_n)[1, ])
    d_n$mode <- microEnemAnalize::get_grouped_mode(v_n, bin_width = 25)
    d_n$q1 <- stats::quantile(v_n, 0.25, na.rm = TRUE)[[1]]
    d_n$q3 <- stats::quantile(v_n, 0.75, na.rm = TRUE)[[1]]
    d_n$p99 <- stats::quantile(v_n, probs = 0.99, na.rm = TRUE, type = 1)[[1]]
    d_n$cor_min <- paste0(get_cor_from_dic(c_min, dic_df), lang_min)
    d_n$cor_max <- paste0(get_cor_from_dic(c_max, dic_df), lang_max)
    d_n$cod_min <- c_min
    d_n$cod_max <- c_max

    # Acertos
    v_a <- df_pool[[col_score]]
    d_a <- as.list(psych::describe(v_a)[1, ])
    d_a$mode <- get_mode(v_a)
    d_a$q1 <- stats::quantile(v_a, 0.25, na.rm = TRUE)[[1]]
    d_a$q3 <- stats::quantile(v_a, 0.75, na.rm = TRUE)[[1]]
    d_a$p99 <- stats::quantile(v_a, probs = 0.99, na.rm = TRUE, type = 1)[[1]]

    list(notas = d_n, acertos = d_a)
  }

  ano <- data[1, ]$NU_ANO
  dic_df <- get(paste0("dic_", ano), envir = .GlobalEnv)
  dic_df_p1 <- dic_df[dic_df$tipo == "1", ]

  # --- Definição dos Pools ---
  cod_digital <- dic_df_p1$codigo[grepl(
    "Digital",
    dic_df$cor,
    ignore.case = TRUE
  )]
  cod_regular <- dic_df_p1$codigo[
    !grepl("Digital", dic_df$cor, ignore.case = TRUE)
  ]

  cli::cli_process_start("Processando Pools")

  lista_completa <- list(
    digital = calc_stats(cod_digital),
    regular = calc_stats(cod_regular)
  )

  cli::cli_process_done()

  # --- Exportação ---
  cli::cli_process_start("Exportando arquivo JSON")
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, "describe.json")
  }
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(
    lista_completa,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salva em: {.path {final_file}}")

  invisible(final_file)
}
