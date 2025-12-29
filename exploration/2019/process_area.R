# Função para processar cada área
process_area <- function(respostas, gabarito, keepna = FALSE) {
  if (is.na(respostas)) {
    stop("Erro: respostas é NA.")
  }
  if (is.na(gabarito)) {
    stop("Erro: gabarito é NA.")
  }
  # transforma em lista de vetores e coloca NA em "." ou "*"
  r <- lapply(strsplit(respostas, ""), function(x) {
    x[x %in% c(".", "*", "9")] <- NA
    x
  })
  # transforma gabarito em lista de vetores
  s <- strsplit(gabarito, "")
  # compara e transforma TRUE/FALSE em 1/0 mantendo NA
  comp_list <- mapply(function(r, s) {
    if (length(r) != length(s)) {
      stop("Erro: respostas e gabarito com comprimentos diferentes.")
    }
    res <- r == s
    as.integer(res)
  }, r, s, SIMPLIFY = FALSE)
  # junta em matriz
  mat <- do.call(rbind, comp_list)
  if (keepna == FALSE) {
    # substitui todos os NA por 0
    mat[is.na(mat)] <- 0
  } else {
    # não substitui o NA
    mat[is.na(mat)] <- NA
  }
  return(mat)
}