#' Processar scores mantendo códigos originais
#'
#' Compara uma string de respostas contra um gabarito, preservando o comprimento
#' original (incluindo "9", "." e "*").
#'
#' @param res String com as respostas (ex: "ABC9*").
#' @param gab String com o gabarito (ex: "ABCDE").
#' @return Uma matriz de uma linha com o processamento (invisível).
#' @export
process_score <- function(res, gab) {
  # Validações de Entrada
  tamanho_res <- nchar(res)
  tamanho_gab <- nchar(gab)

  # Etapa de segurança
  if (tamanho_res != tamanho_gab) {
    stop(sprintf(
      "Comprimentos diferentes: Resposta (%d) vs Gabarito (%d).",
      tamanho_res,
      tamanho_gab
    ))
  }

  if (!(tamanho_res %in% c(45, 50))) {
    stop(sprintf(
      "Tamanho invalido: A resposta tem %d caracteres. Deve ter 45 ou 50.",
      tamanho_res
    ))
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
    stop(sprintf(
      "Tamanho invalido de mat_resultado: tem %d colunas, mas deveria ter %d",
      ncol(mat_resultado),
      tamanho_res
    ))
  }

  invisible(mat_resultado)
}
