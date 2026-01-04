#' Processar escores mantendo códigos originais
#'
#' Compara uma string de respostas contra um gabarito, preservando o comprimento
#' original (incluindo "9", "." e "*"). Atribui o resultado a 'score' no GlobalEnv.
#'
#' @param res String com as respostas (ex: "ABC9*").
#' @param gab String com o gabarito (ex: "ABCDE").
#' @return Uma matriz de uma linha com o processamento (invisível).
#' @export
process_score <- function(res, gab) {

  # 1. Validações de Entrada
  if (nchar(res) != nchar(gab)) {
    stop(sprintf("Comprimentos diferentes: Resposta (%d) vs Gabarito (%d).",
                 nchar(res), nchar(gab)))
  }

  tamanho_res <- nchar(res)
  tamanho_gab <- nchar(gab)

  if (!(tamanho_res %in% c(45, 50))) {
    stop(sprintf("Tamanho inválido: A resposta tem %d caracteres. Deve ter 45 ou 50.", tamanho_res))
  }

  # 2. Vetorização
  r_vec <- strsplit(res, "")[[1]]
  g_vec <- strsplit(gab, "")[[1]]

  # 3. Lógica de Correção Manual (para manter caracteres não-numéricos)
  # Comparamos resposta com gabarito: 1 se igual, 0 se diferente
  resultado <- ifelse(r_vec == g_vec, 1L, 0L)

  # 4. Codificação de Caracteres Especiais (conforme solicitado)
  # Nota: usamos 'L' para garantir o tipo Integer no R
  resultado[r_vec == "9"] <- 9L
  resultado[r_vec == "."] <- 8L
  resultado[r_vec == "*"] <- 7L

  # 5. Transformação em Matriz de Inteiros
  mat_resultado <- matrix(as.integer(resultado), nrow = 1)

  if (tamanho_res != ncol(mat_resultado)) {
    stop(sprintf("Tamanho inválido de mat_resultado: tem %d colunas, mas deveria ter %d", ncol(mat_resultado), tamanho_res))
  }

  # 6. Atribuição no Environment
  # assign("score", mat_resultado, envir = .GlobalEnv)

  return(invisible(mat_resultado))
}
