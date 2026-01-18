library(data.table)
library(dplyr)

#-----------2023--------------

data <- fread("exploration/2023/MICRODADOS/microdados_enem_2023/DADOS/MICRODADOS_ENEM_2023.csv")
my_data <- data %>%
  dplyr::filter(NU_NOTA_MT == 833.2 & NO_MUNICIPIO_PROVA == "Votuporanga")

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

score_LC <- sum(process_score(res_LC, gab_LC))
score_CH <- sum(process_score(res_CH, gab_CH))
score_CN <- sum(process_score(res_CN, gab_CN))
score_MT <- sum(process_score(res_MT, gab_MT))
