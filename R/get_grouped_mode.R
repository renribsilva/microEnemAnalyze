#' @title Calcular a Moda para Dados Agrupados (Fórmula de Czuber)
#'
#' @description Esta função calcula a estimativa da moda para variáveis contínuas utilizando o método
#' de agrupamento em classes. É especialmente útil para distribuições de notas (como as do ENEM),
#' onde a moda empírica simples pode ser ruidosa. A estimativa tende a coincidir com
#' o pico da curva de densidade kernel.
#'
#' @param x Vetor numérico contendo os dados brutos (ex: notas).
#' @param bin_width Numérico. A largura do intervalo (classe) para o agrupamento.
#'   O padrão é 10 (recomendado para escalas de 0 a 1000).
#'
#' @return Um valor numérico representando a moda estimada.
#'
#' @details
#' A fórmula de Czuber considera as frequências das classes adjacentes à classe modal:
#' \deqn{Mo = Li + \left( \frac{\Delta_1}{\Delta_1 + \Delta_2} \right) \cdot h}
#' Onde \eqn{\Delta_1 = f_{mo} - f_{ant}} e \eqn{\Delta_2 = f_{mo} - f_{post}}.
#'
#' @export
#'
#' @examples
#' notas <- c(500, 505, 505, 508, 510, 512, 540, 550)
#' get_grouped_mode(notas, bin_width = 10)
get_grouped_mode <- function(x, bin_width = 10) {

  # Remover valores ausentes
  x <- x[!is.na(x)]

  if (length(x) == 0) return(NA_real_)

  # 1. Cria os intervalos (bins)
  # Garantimos que o limite superior cubra o valor máximo
  breaks <- seq(floor(min(x)), ceiling(max(x)) + bin_width, by = bin_width)
  intervals <- cut(x, breaks = breaks, right = FALSE)

  # 2. Calcula a tabela de frequências
  tab <- as.data.frame(table(intervals))

  # 3. Identifica a classe modal (a que tem maior frequência)
  idx_mo <- which.max(tab$Freq)
  f_mo   <- tab$Freq[idx_mo]

  # Frequências vizinhas (trata bordas com 0)
  f_ant  <- if(idx_mo > 1) tab$Freq[idx_mo - 1] else 0
  f_post <- if(idx_mo < nrow(tab)) tab$Freq[idx_mo + 1] else 0

  # Limite inferior da classe modal
  Li <- breaks[idx_mo]

  # Cálculo dos deltas (Fórmula de Czuber)
  d1 <- f_mo - f_ant
  d2 <- f_mo - f_post

  # Caso especial: se d1 e d2 forem 0 (raro em grandes volumes de dados)
  if ((d1 + d2) == 0) return(Li + (bin_width / 2))

  # 4. Aplica a Fórmula de Czuber
  mode_grouped <- Li + (d1 / (d1 + d2)) * bin_width

  return(as.numeric(mode_grouped))
}
