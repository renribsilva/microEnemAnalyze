library(data.table)
load("data/itens_2009")

sample_LC <- fread("exploration/2009/MICRODADOS/sample_constantes_LC.csv")
sample_CH <- fread("exploration/2009/MICRODADOS/sample_constantes_CH.csv")
sample_CN <- fread("exploration/2009/MICRODADOS/sample_constantes_CN.csv")
sample_MT <- fread("exploration/2009/MICRODADOS/sample_constantes_MT.csv")

res_lc <- process_constantes(sample_LC, "LC", itens_2009)
res_ch <- process_constantes(sample_CH, "CH", itens_2009)
res_cn <- process_constantes(sample_CN, "CN", itens_2009)
res_mt <- process_constantes(sample_MT, "MT", itens_2009)

constantes <- data.frame(
  area = c("CH", "CN", "LC", "MT"),
  d    = c(res_ch$d, res_cn$d, res_lc$d, res_mt$d),
  k    = c(res_ch$k, res_cn$k, res_lc$k, res_mt$k)
)

# 4. Ordenar alfabeticamente pela área (opcional, para bater com seu exemplo)
constantes <- constantes[order(constantes$area), ]

usethis::use_data(constantes, overwrite = TRUE)

# Visualizar resultado
print(constantes)
