# ------
# 2019 -
# ------

# Importa os itens
itens_2019 <- fread("exploration/2019/MICRODADOS/microdados_enem_2019/DADOS/ITENS_PROVA_2019.csv")

# Salvando no pacote
usethis::use_data(itens_2019, overwrite = TRUE, compress = "xz")

# ------
# 2009 -
# ------

# Importa os itens
itens_2009 <- fread("exploration/2009/MICRODADOS/microdados_enem_2009/DADOS/ITENS_PROVA_2009.csv")

# Salvando no pacote
usethis::use_data(itens_2009, overwrite = TRUE, compress = "xz")
