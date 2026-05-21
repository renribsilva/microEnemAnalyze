# ------
# 2009 -
# ------

library(data.table)

# Importa os itens
itens_2009 <- fread(
  "~/Downloads/
  microdados_enem_2009/DADOS/ITENS_PROVA_2009.csv",
  encoding = "UTF-8"
)

# Salvando no pacote
usethis::use_data(itens_2009, overwrite = TRUE, compress = "xz")

# ------
# 2019 -
# ------

library(data.table)

# Importa os itens
itens_2019 <- fread(
  "~/Downloads/
  microdados_enem_2019/DADOS/ITENS_PROVA_2019.csv",
  encoding = "UTF-8"
)

# Salvando no pacote
usethis::use_data(itens_2019, overwrite = TRUE, compress = "xz")

# ------
# 2020 -
# ------

library(data.table)

# Importa os itens
itens_2020 <- fread(
  "~/Downloads/
  microdados_enem_2020/DADOS/ITENS_PROVA_2020.csv",
  encoding = "UTF-8"
)

# Salvando no pacote
usethis::use_data(itens_2020, overwrite = TRUE, compress = "xz")

# ------
# 2021 -
# ------

library(data.table)

# Importa os itens
itens_2021 <- fread(
  "~/Downloads/
  microdados_enem_2021/DADOS/ITENS_PROVA_2021.csv",
  encoding = "UTF-8"
)

# Salvando no pacote
usethis::use_data(itens_2021, overwrite = TRUE, compress = "xz")

# ------
# 2022 -
# ------

library(data.table)

# Importa os itens
itens_2022 <- fread(
  "~/Downloads/
  microdados_enem_2022/DADOS/ITENS_PROVA_2022.csv",
  encoding = "UTF-8"
)

# Salvando no pacote
usethis::use_data(itens_2022, overwrite = TRUE, compress = "xz")

# ------
# 2023 -
# ------

library(data.table)

# Importa os itens
itens_2023 <- fread(
  "~/Downloads/
  microdados_enem_2023/DADOS/ITENS_PROVA_2023.csv",
  encoding = "UTF-8"
)

# Salvando no pacote
usethis::use_data(itens_2023, overwrite = TRUE, compress = "xz")

# ------
# 2024 -
# ------

library(data.table)

# Importa os itens
itens_2024 <- fread(
  "~/Downloads/
  microdados_enem_2024/DADOS/ITENS_PROVA_2024.csv",
  encoding = "UTF-8"
)

# Salvando no pacote
usethis::use_data(itens_2024, overwrite = TRUE, compress = "xz")
