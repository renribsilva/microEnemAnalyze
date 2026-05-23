#-----------2020--------------

data <- data.table::fread(
  input = file.path(
    "~/Downloads",
    "microdados_enem_2020/DADOS/MICRODADOS_ENEM_2020.csv"
  ),
  encoding = "UTF-8"
)

my_data <- data |>
  dplyr::filter(NU_NOTA_MT == 804.5 & NO_MUNICIPIO_PROVA == "Votuporanga")

a <- substr(my_data$TX_RESPOSTAS_LC, 0, 5)
b <- substr(my_data$TX_RESPOSTAS_LC, 11, 99999)
c <- substr(my_data$TX_GABARITO_LC, 0, 5)
d <- substr(my_data$TX_GABARITO_LC, 11, 99999)

res_lc <- paste0(a, b)
res_ch <- my_data$TX_RESPOSTAS_CH
res_cn <- my_data$TX_RESPOSTAS_CN
res_mt <- my_data$TX_RESPOSTAS_MT

gab_lc <- paste0(c, d)
gab_ch <- my_data$TX_GABARITO_CH
gab_cn <- my_data$TX_GABARITO_CN
gab_mt <- my_data$TX_GABARITO_MT

score_lc <- sum(process_score(res_lc, gab_lc))
score_ch <- sum(process_score(res_ch, gab_ch))
score_cn <- sum(process_score(res_cn, gab_cn))
score_mt <- sum(process_score(res_mt, gab_mt))
