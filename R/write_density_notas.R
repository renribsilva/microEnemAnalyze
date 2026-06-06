#' @title Escrever JSON com as coordenadas de densidade
#'
#' @description Essa função escreve um JSON com a distribuição
#' da densidade das notas de cada área do exame.
#'
#' @param data Data.table com os microdados do ENEM
#' @param path_json Caminho do arquivo JSON.
#'
#' @export
write_density_notas <- function(data, path_json) {
  cli::cli_h1("Processamento de Densidade")

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

  cli::cli_process_start("Preparando variaveis e funcoes auxiliares")

  # Contrói os nomes das colunas
  col_prova <- grep("^CO_PROVA_", names(data), value = TRUE)
  col_nota <- grep("^NU_NOTA_", names(data), value = TRUE)

  # --- Função auxiliar ---
  calc_dens <- function(codigos_pool) {
    df_pool <- data[
      get(col_prova) %in%
        codigos_pool &
        get(col_nota) > 0 &
        !is.na(get(col_nota))
    ]
    notas_v <- df_pool[[col_nota]]

    if (length(notas_v) < 2) {
      return(NULL)
    }

    dens <- stats::density(
      notas_v,
      n = 512,
      from = min(notas_v),
      to = max(notas_v)
    )
    y_normal <- stats::dnorm(
      dens$x,
      mean = mean(notas_v),
      sd = stats::sd(notas_v)
    )

    list(
      datasets = list(
        list(
          id = "main-density",
          data = data.table::data.table(x = dens$x, y = dens$y)
        ),
        list(
          id = "normal-reference",
          data = data.table::data.table(x = dens$x, y = y_normal)
        )
      )
    )
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

  cli::cli_process_done()

  cli::cli_process_start("Calculando Densidades (Digital vs Regular)")

  lista_completa <- list(
    digital = calc_dens(cod_digital),
    regular = calc_dens(cod_regular)
  )

  cli::cli_process_done()

  # --- TRATAMENTO DO PATH E EXPORTAÇÃO ---
  cli::cli_process_start("Exportando arquivo JSON")
  final_file <- if (grepl("\\.json$", path_json)) {
    path_json
  } else {
    file.path(path_json, "density.json")
  }
  dir.create(dirname(final_file), showWarnings = FALSE, recursive = TRUE)

  jsonlite::write_json(
    lista_completa,
    path = final_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  cli::cli_process_done()

  cli::cli_alert_success("Arquivo salvo em: {.path {final_file}}")

  invisible(final_file)
}
