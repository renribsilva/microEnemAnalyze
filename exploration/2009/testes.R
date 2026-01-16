library(dplyr)

sample <- fread("exploration/2019/MICRODADOS/microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv", nrows = 100)

data <- sample %>%
  dplyr::filter(NU_NOTA_MT > 0) %>%
  slice(6)

data$NU_NOTA_MT
calc_nota(data, area = "MT", ano = 2019)
# A divergência das notas da área de MT no ano de 2019
# continua mesmo reproduzindo manualmente todos os cálculos.
