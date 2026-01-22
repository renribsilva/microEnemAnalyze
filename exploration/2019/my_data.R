library(data.table)
library(dplyr)

#-----------2019--------------

data <- fread("exploration/2019/MICRODADOS/microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv")
my_data <- data %>%
  dplyr::filter(NU_NOTA_MT == 795.4 & NO_MUNICIPIO_PROVA == "Votuporanga")

a <- substr(my_data$TX_RESPOSTAS_LC, 0, 5)
b <- substr(my_data$TX_RESPOSTAS_LC, 11, 99999)
c <- substr(my_data$TX_GABARITO_LC, 0, 5)
d <- substr(my_data$TX_GABARITO_LC, 11, 99999)

res_LC <- paste0(a, b)
res_CH <- my_data$TX_RESPOSTAS_CH
res_CN <- my_data$TX_RESPOSTAS_CN
res_MT <- my_data$TX_RESPOSTAS_MT

gab_LC <- paste0(c, d)
gab_CH <- my_data$TX_GABARITO_CH
gab_CN <- my_data$TX_GABARITO_CN
gab_MT <- my_data$TX_GABARITO_MT

score_LC <- sum(process_score(res_LC, gab_LC)[process_score(res_LC, gab_LC) == 1])
score_CH <- sum(process_score(res_CH, gab_CH)[process_score(res_CH, gab_CH) == 1])
score_CN <- sum(process_score(res_CN, gab_CN)[process_score(res_CN, gab_CN) == 1])
score_MT <- sum(process_score(res_MT, gab_MT)[process_score(res_MT, gab_MT) == 1])

my_data <- fread("exploration/2019/MICRODADOS/at_least_one_presence.csv", nrows = 1)
calc_nota(my_data, area = "MT", ano = 2019)

sample <- "000000000000000000000000000000000000000000000"

# Converte a string em vetor numérico
score_i <- as.numeric(strsplit(sample, "")[[1]])

# Transforma em matriz 1 linha x 45 colunas
score_i <- matrix(score_i, nrow = 1)

# Verifica dimensões
dim(score_i)
# [1] 1 45

# Mostra a matriz
score_i

