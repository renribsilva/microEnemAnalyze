library(data.table)
library(dplyr)

#-----------2021--------------

data <- fread("exploration/2021/MICRODADOS/microdados_enem_2021/DADOS/MICRODADOS_ENEM_2021.csv")
my_data <- data %>%
  dplyr::filter(NU_NOTA_MT == 745.9 & NO_MUNICIPIO_PROVA == "Votuporanga")

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

score_LC <- sum(process_score(res_LC, gab_LC))
score_CH <- sum(process_score(res_CH, gab_CH))
score_CN <- sum(process_score(res_CN, gab_CN))
score_MT <- sum(process_score(res_MT, gab_MT))
