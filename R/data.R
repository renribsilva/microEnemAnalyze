#' @title Constantes de transformação
#' @description Constantes de transformação da escala dos itens
#' divulgados nos microdados para a escala oficial do Enem
#'
#' @format Um objeto do tipo `data.table` com as constantes de cada
#' área.
#' \describe{
#'  \item{area}{Área do conhecimento: 'LC', 'CH', 'CN', 'MT'}
#'  \item{d}{Nota média referente ao exame de 2009}
#'  \item{k}{Desvio padrão referente ao exame de 2009}
#' }
#'
#' @details Temos duas escalas: a escala oficial do Enem, cuja
#' referência são os concluintes regulares de escolas públicas do
#' Enem 2009 (média 500, desvio padrão 100); e a escala dos
#' itens divulgados nos microdados (escala dos microdados),
#' cuja referência é a amostra de
#' calibração do Enem 2009 (média 0, desvio padrão 1).
#'
#' Para calcular as constantes de transformação de uma escala
#' para a outra, aplicamos a equalização linear
#' (Hambleton et al., 1991). Inicialmente estimamos
#' a nota dos primeiros 300.000 sujeitos do banco dos
#' microdados em cada área, de acordo com
#' as especificações em `calc.nota`. Em seguida, estabelecemos
#' a igualdade entre a padronização das notas desses sujeitos:
#' \deqn{\frac{Y_{i}-\overline{Y}}{DP_{y}}=\frac{X_{i}-\overline{X}}{DP_{x}}}
#' onde \eqn{Y_{i}} representa a nota do sujeito \eqn{i} na escala
#' oficial do Enem e \eqn{\overline{Y}} e \eqn{DP_{y}} representam a
#' média e o
#' desvio padrão das notas dessa amostra de 300.000 sujeitos nessa mesma
#' escala. \eqn{X_{i}} representa a nota do sujeito \eqn{i} na escala
#' dos microdados. \eqn{\overline{X}} e \eqn{DP_{x}}
#' representam a média e o desvio padrão das notas da amostra nessa
#' escala.
#'
#' Nessa
#' equação, consideramos que as notas padronizadas das duas escalas
#' são iguais, pois provêm da mesma amostra. Se isolarmos
#' \eqn{Y_{i}}, teremos (Muñiz, 1997):
#'
#' \deqn{Y_{i}=\frac{DP_{y}}{DP_{x}}X_{i}+
#' \overline{Y}-\frac{DP_{y}}{DP_{x}}\overline{X}}
#'
#' Dessa equação, extraímos as constantes \eqn{k} e \eqn{d}:
#'
#' \deqn{k=\frac{DP_{y}}{DP_{x}}}
#'
#' \deqn{d=\overline{Y}-\frac{DP_{y}}{DP_{x}}\overline{X}}
#' \deqn{d=\overline{Y}-k\overline{X}}
#'
#' Portanto, para transformar a nota da escala dos microdados para a
#' escala oficial do Enem, utilizamos a seguinte equação:
#' \deqn{Y_{i}=kX_{i}+d}
#'
#'
"constantes"

#' @title Parâmetros dos Itens do ENEM
#'
#' @description Dataset contendo os parâmetros A, B e C dos
#' itens processados.
#'
#' @section Names:
#' \describe{
#'   \item{NU_PARAM_A}{Parâmetro de discriminação}
#'   \item{NU_PARAM_B}{Parâmetro de dificuldade}
#'   \item{NU_PARAM_C}{Parâmetro de acerto ao acaso (chute)}
#' }
#' @source Microdados do INEP.
"itens_2009"

#' @inherit itens_2009 title description source
#' @inheritSection itens_2009 Names
"itens_2019"

#' @inherit itens_2009 title description source
#' @inheritSection itens_2009 Names
"itens_2020"

#' @inherit itens_2009 title description source
#' @inheritSection itens_2009 Names
"itens_2021"

#' @inherit itens_2009 title description source
#' @inheritSection itens_2009 Names
"itens_2022"

#' @inherit itens_2009 title description source
#' @inheritSection itens_2009 Names
"itens_2023"

#' @inherit itens_2009 title description source
#' @inheritSection itens_2009 Names
"itens_2024"

#' @title Código das provas do ENEM
#'
#' @description Dataset contendo o código das provas do ENEM.
#'
#' @section Names:
#' \describe{
#'   \item{ano}{Ano da prova}
#'   \item{codigo}{Código do caderno}
#'   \item{cor}{Cor do caderno}
#' }
#' @source Microdados do INEP.
"dic_2009"

#' @inherit dic_2009 title description source
#' @inheritSection dic_2009 Names
"dic_2019"

#' @inherit dic_2009 title description source
#' @inheritSection dic_2009 Names
"dic_2020"

#' @inherit dic_2009 title description source
#' @inheritSection dic_2009 Names
"dic_2021"

#' @inherit dic_2009 title description source
#' @inheritSection dic_2009 Names
"dic_2022"

#' @inherit dic_2009 title description source
#' @inheritSection dic_2009 Names
"dic_2023"

#' @inherit dic_2009 title description source
#' @inheritSection dic_2009 Names
"dic_2024"
