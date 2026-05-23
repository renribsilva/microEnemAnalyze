load("data/itens_2009")

sample_lc <- data.table::fread(
  "exploration/2009/MICRODADOS/sample_constantes_LC.csv"
)
sample_ch <- data.table::fread(
  "exploration/2009/MICRODADOS/sample_constantes_CH.csv"
)
sample_cn <- data.table::fread(
  "exploration/2009/MICRODADOS/sample_constantes_CN.csv"
)
sample_mt <- data.table::fread(
  "exploration/2009/MICRODADOS/sample_constantes_MT.csv"
)

res_lc <- process_constantes(sample_lc, "LC", itens_2009)
res_ch <- process_constantes(sample_ch, "CH", itens_2009)
res_cn <- process_constantes(sample_cn, "CN", itens_2009)
res_mt <- process_constantes(sample_mt, "MT", itens_2009)

constantes <- data.table::data.table(
  area = c("CH", "CN", "LC", "MT"),
  d = c(res_ch$d, res_cn$d, res_lc$d, res_mt$d),
  k = c(res_ch$k, res_cn$k, res_lc$k, res_mt$k)
)

# 4. Ordenar alfabeticamente pela área (opcional, para bater com seu exemplo)
constantes <- constantes[order(constantes$area), ]

usethis::use_data(constantes, overwrite = TRUE)

# Visualizar resultado
print(constantes)
