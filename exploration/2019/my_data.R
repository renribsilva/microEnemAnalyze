library(data.table)
library(dplyr)

#-----------2019--------------

# CADERNO 516
data <- data.table::fread(
  input = file.path(
    "~/Downloads",
    "microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv"
  ),
  encoding = "UTF-8"
)

my_data <- data |>
  dplyr::filter(NU_NOTA_MT == 795.4 & NO_MUNICIPIO_PROVA == "Votuporanga")

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

paste0(process_score(res_mt, gab_mt), collapse = "")
my_data$CO_PROVA_MT

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

my_data <- data.table::fread(
  "exploration/2019/MICRODADOS/at_least_one_presence.csv",
  nrows = 1
)
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
