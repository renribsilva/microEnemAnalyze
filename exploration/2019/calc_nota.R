install.packages("mirt")
install.packages("mirtCAT")
# install.packages("bit64")
# install.packages("stringr")

library(mirt)
library(mirtCAT)
library(dplyr)

# library(data.table)
# library(bit64)
# library(dplyr)
# library(stringr)

filtered <- fread("~/Área\ de\ trabalho/DEV/R/microEnemAnalyze/exploration/2019/MICRODADOS/at_least_one_presence.csv")

# Encontra meus dados
obs <- filtered[1, ]

vector <- obs$TX_RESPOSTAS_LC
codigo <- obs$CO_PROVA_LC
lingua <- obs$TP_LINGUA

itens <- fread(input='exploration/2019/MICRODADOS/microdados_enem_2019/DADOS/ITENS_PROVA_2019.csv')
# itens_2019_filtered <- itens_2019 %>%
#   dplyr::filter(CO_PROVA == 511 | CO_PROVA == 512 | CO_PROVA == 513 | CO_PROVA == 514 | CO_PROVA == 521 | CO_PROVA == 525 |
#                 CO_PROVA == 507 | CO_PROVA == 508 | CO_PROVA == 509 | CO_PROVA == 510 | CO_PROVA == 520 | CO_PROVA == 524 |
#                 CO_PROVA == 503 | CO_PROVA == 504 | CO_PROVA == 505 | CO_PROVA == 506 | CO_PROVA == 519 | CO_PROVA == 523 |
#                 CO_PROVA == 515 | CO_PROVA == 516 | CO_PROVA == 517 | CO_PROVA == 518 | CO_PROVA == 522 | CO_PROVA == 526)

itens <- subset(itens, CO_PROVA == codigo)
itens <- dplyr::arrange(itens, TP_LINGUA, CO_POSICAO)
itens_mirt <- itens %>%
  dplyr::select("NU_PARAM_A", "NU_PARAM_B", "NU_PARAM_C")
names(itens_mirt) <- c('a1', 'd', 'g')

itens_mirt$a1 <- as.numeric(itens_mirt$a1)
itens_mirt$d <- as.numeric(itens_mirt$d)
itens_mirt$g <- as.numeric(itens_mirt$g)
itens_mirt$d <- itens_mirt$a1*-itens_mirt$d

mod <- mirtCAT::generate.mirt_object(
  itens_mirt,
  '3PL'
)

# modelo mirt do caderno
# mod <- mod.caderno(codigo = codigo)

# gabarito do caderno
key <- subset(itens, CO_PROVA == codigo)
key <- dplyr::arrange(key, TP_LINGUA, CO_POSICAO)

abre.resp <- function (unico) {
  resp. <- strsplit(as.character(as.matrix(unico)), NULL)
  resp <- do.call(rbind, resp.)
  return(resp)
}

# abrir o vetor de respostas
resp <- abre.resp(resp$TX_RESPOSTAS_MT)

# verificar se algum item foi anulado
anulado <- which(key$IN_ITEM_ABAN == 1)
key <- subset(key, IN_ITEM_ABAN == 0)

# área e ano do caderno
area <- "MT"
ano <- 2019

# transformar 9 em NA nos itens de língua estrangeira
if (area == 'LC' & ano != 2009)
{
  resp <- apply(resp, 2, \(x)ifelse(x == '9', NA, x))

  if (ano == 2022)
    if(ncol(resp) != 45)
    {stop(paste0('Para o ano de ', ano, ', o vetor de resposta deve ter 45 elementos.'))} else {resp <- insereNA(data = resp, lingua = lingua)}
}

# retirar o item anulado da resposta
if (length(anulado) > 0)
  if(length(resp) > 1)
  {resp <- resp[,-anulado]} else {resp <- t(data.frame(resp[-anulado]))}


# # verificar se o tamanho da prova é igual ao tamanho do vetor de respostas
# if(ncol(resp) != length(key$TX_GABARITO))
#   stop(paste0('O vetor de resposta deve ser do mesmo tamanho da prova. O vetor de respostas possui ',
#               ncol(resp), ' caracteres e a prova possui ',
#               length(key$TX_GABARITO),
#               ' itens.'))

# corrigir as respostas
resp <- mirt::key2binary(resp, key$TX_GABARITO)

# calcular a nota
nota <- data.frame(mirt::fscores(mod, response.pattern = resp, quadpts = 40, theta_lim = c(-4,4)))$F1

# transformação da escala
nota <- round(nota*constantes[constantes$area == area, 'k'] + constantes[constantes$area == area, 'd'], 1)

# quem entregou a prova em branco recebe 0
branco <- stringr::str_count(resps, "\\.")
nota[(branco == 45)] <- 0
