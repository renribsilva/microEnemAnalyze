#-----------2025--------------

data <- data.table::fread(
  input = file.path(
    "~/Downloads",
    "microdados_enem_2025/DADOS/RESULTADOS_2025.csv"
  ),
  encoding = "UTF-8"
)

my_data <- data |>
  dplyr::filter(NU_NOTA_MT == 812.1 & NO_MUNICIPIO_PROVA == "Votuporanga")

c <- substr(my_data$TX_GABARITO_LC, 0, 5)
d <- substr(my_data$TX_GABARITO_LC, 11, 99999)

res_lc <- my_data$TX_RESPOSTAS_LC
res_ch <- my_data$TX_RESPOSTAS_CH
res_cn <- my_data$TX_RESPOSTAS_CN
res_mt <- my_data$TX_RESPOSTAS_MT

gab_lc <- paste0(c, d)
gab_ch <- my_data$TX_GABARITO_CH
gab_cn <- my_data$TX_GABARITO_CN
gab_mt <- my_data$TX_GABARITO_MT
f <- process_score(res_ch, gab_ch)

score_lc <- sum(process_score(res_lc, gab_lc)[
  process_score(res_lc, gab_lc) == 1
])
score_ch <- sum(process_score(res_ch, gab_ch)[
  process_score(res_ch, gab_ch) == 1
])
score_cn <- sum(process_score(res_cn, gab_cn)[
  process_score(res_cn, gab_cn) == 1
])
score_mt <- sum(process_score(res_mt, gab_mt)[
  process_score(res_mt, gab_mt) == 1
])
