#' Parâmetros dos Itens do ENEM
#'
#' Dataset contendo os parâmetros A, B e C dos itens processados.
#'
#' @format Um data.frame (ou data.table) com as colunas:
#' \describe{
#'   \item{a1}{Parâmetro de discriminação}
#'   \item{d}{Intercepto calculado como -a*b}
#'   \item{g}{Parâmetro de acerto ao acaso (chute)}
#' }
#' @source Microdados do INEP 2019.
"itens_2019"

#' Código das provas do ENEM
#'
#' Dataset contendo o código das provas do ENEM de 2019.
#'
#' @format Um data.frame (ou data.table) com as colunas:
#' \describe{
#'   \item{ano}{Ano da prova}
#'   \item{codigo}{Código do caderno}
#'   \item{cor}{Cor do caderno}
#' }
#' @source Microdados do INEP 2019.
"dic_2019"
