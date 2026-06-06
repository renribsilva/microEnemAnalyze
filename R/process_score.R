#' @title Processar scores
#'
#' @description Compara uma string de respostas contra uma de gabarito,
#' preservando caracteres especiais (incluindo "9", "." e "*").
#'
#' @param res String com as respostas (ex: "ABC9*") com 45 ou 50 caracteres.
#' @param gab String com o gabarito (ex: "ABCDE") com 45 ou 50 caracteres.
#'
#' @export
process_score <- function(res, gab) {
  # Validações de Entrada
  tamanho_res <- nchar(res)
  tamanho_gab <- nchar(gab)

  # Etapa de segurança
  if (tamanho_res != tamanho_gab) {
    cli::cli_abort(
      "Comprimentos diferentes: Resposta ({.val {tamanho_res}}) vs
      Gabarito ({.val {tamanho_gab}})."
    )
  }

  if (!(tamanho_res %in% c(45, 50))) {
    cli::cli_abort(
      "Tamanho invalido: A resposta tem {.val {tamanho_res}} caracteres.
      Deve ter 45 ou 50."
    )
  }

  # Vetorização
  r_vec <- strsplit(res, "")[[1]]
  g_vec <- strsplit(gab, "")[[1]]

  # Resultado
  resultado <- ifelse(r_vec == g_vec, 1L, 0L)

  # Reconstrução dos valores especiais
  resultado[r_vec == "9"] <- 9L
  resultado[r_vec == "."] <- 8L
  resultado[r_vec == "*"] <- 7L

  # Transformação em Matriz de Inteiros
  mat_resultado <- matrix(as.integer(resultado), nrow = 1)

  # Etapa de segurança
  if (tamanho_res != ncol(mat_resultado)) {
    cli::cli_abort(
      "Tamanho invalido de {.var mat_resultado}:
      tem {.val {ncol(mat_resultado)}} colunas, mas deveria
      ter {.val {tamanho_res}}."
    )
  }

  invisible(mat_resultado)
}
