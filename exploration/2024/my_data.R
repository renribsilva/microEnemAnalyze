library(data.table)
library(dplyr)

#-----------2024--------------

data <- fread("exploration/2024/MICRODADOS/microdados_enem_2024/DADOS/RESULTADOS_2024.csv")
my_data <- data %>%
  dplyr::filter(NU_NOTA_MT == 823.6 & NO_MUNICIPIO_PROVA == "Votuporanga")

c <- substr(my_data$TX_GABARITO_LC, 0, 5)
d <- substr(my_data$TX_GABARITO_LC, 11, 99999)

res_LC <- my_data$TX_RESPOSTAS_LC
res_CH <- my_data$TX_RESPOSTAS_CH
res_CN <- my_data$TX_RESPOSTAS_CN
res_MT <- my_data$TX_RESPOSTAS_MT

gab_LC <- paste0(c, d)
gab_CH <- my_data$TX_GABARITO_CH
gab_CN <- my_data$TX_GABARITO_CN
gab_MT <- my_data$TX_GABARITO_MT
f <- process_score(res_CH, gab_CH)

score_LC <- sum(process_score(res_LC, gab_LC)[process_score(res_LC, gab_LC) == 1])
score_CH <- sum(process_score(res_CH, gab_CH)[process_score(res_CH, gab_CH) == 1])
score_CN <- sum(process_score(res_CN, gab_CN)[process_score(res_CN, gab_CN) == 1])
score_MT <- sum(process_score(res_MT, gab_MT)[process_score(res_MT, gab_MT) == 1])
