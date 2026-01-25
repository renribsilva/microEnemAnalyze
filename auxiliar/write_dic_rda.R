# ------
# 2009 -
# ------

library(data.table)

itens_2009 <- fread(input='exploration/2009/MICRODADOS/microdados_enem_2009/DADOS/ITENS_PROVA_2009.csv')

# --- 1. Linguagens e Códigos (LC) ---
lc_2009 <- data.table(
  area      = "LC",
  ano       = 2009,
  codigo    = as.numeric(c(57, 58, 59, 60, 83, 73, 74, 75, 76)),
  cor       = c("Amarela", "Cinza", "Azul", "Rosa", "Cinza - Adaptada Ledor",
                "Amarela", "Cinza", "Azul", "Rosa"),
  aplicacao = c(rep("P1", 5), rep("P2", 4))
)

# --- 2. Ciências Humanas (CH) ---
ch_2009 <- data.table(
  area      = "CH",
  ano       = 2009,
  codigo    = as.numeric(c(53, 54, 55, 56, 82, 69, 70, 71, 72)),
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Branca - Adaptada Ledor",
                "Azul", "Amarela", "Branca", "Rosa"),
  aplicacao = c(rep("P1", 5), rep("P2", 4))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2009 <- data.table(
  area      = "CN",
  ano       = 2009,
  codigo    = as.numeric(c(49, 50, 51, 52, 81, 65, 66, 67, 68)),
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Branca - Adaptada Ledor",
                "Azul", "Amarela", "Branca", "Rosa"),
  aplicacao = c(rep("P1", 5), rep("P2", 4))
)

# --- 4. Matemática (MT) ---
mt_2009 <- data.table(
  area      = "MT",
  ano       = 2009,
  codigo    = as.numeric(c(61, 62, 63, 64, 84, 77, 78, 79, 80)),
  cor       = c("Amarela", "Cinza", "Azul", "Rosa",
                "Cinza - Adaptada Ledor",
                "Amarela", "Cinza", "Azul", "Rosa"),
  aplicacao = c(rep("P1", 5), rep("P2", 4))
)

# --- UNINDO TUDO ---
dic_2009 <- rbind(lc_2009, ch_2009, cn_2009, mt_2009)

# Salvando no pacote
usethis::use_data(dic_2009, overwrite = TRUE)

# ------
# 2019 -
# ------

library(data.table)

itens_2019 <- fread(input='exploration/2019/MICRODADOS/microdados_enem_2019/DADOS/ITENS_PROVA_2019.csv')

# --- 1. Linguagens e Códigos (LC) ---
lc_2019 <- data.table(
  area      = "LC",
  ano       = 2019,
  codigo    = as.numeric(c(511, 512, 513, 514, 521, 525)),
  cor       = c("Azul", "Amarela", "Rosa", "Branca",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras"),
  aplicacao = c(rep("P1", 6))
)

# --- 2. Ciências Humanas (CH) ---
ch_2019 <- data.table(
  area      = "CH",
  ano       = 2019,
  codigo    = as.numeric(c(507, 508, 509, 510, 520, 524)),
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras"),
  aplicacao = c(rep("P1", 6))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2019 <- data.table(
  area      = "CN",
  ano       = 2019,
  codigo    = as.numeric(c(503, 504, 505, 506, 519, 523)),
  cor       = c("Azul", "Amarela", "Cinza", "Rosa",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras"),
  aplicacao = c(rep("P1", 6))
)

# --- 4. Matemática (MT) ---
mt_2019 <- data.table(
  area      = "MT",
  ano       = 2019,
  codigo    = as.numeric(c(515, 516, 517, 518, 522, 526)),
  cor       = c("Azul", "Amarela", "Rosa", "Cinza",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras"),
  aplicacao = c(rep("P1", 6))
)

# --- UNINDO TUDO ---
dic_2019 <- rbind(lc_2019, ch_2019, cn_2019, mt_2019)

# Salvando no pacote
usethis::use_data(dic_2019, overwrite = TRUE)

# ------
# 2020 -
# ------

library(data.table)

itens_2020 <- fread(input='exploration/2020/MICRODADOS/microdados_enem_2020/DADOS/ITENS_PROVA_2020.csv')

# --- 1. Linguagens e Códigos (LC) ---
lc_2020 <- data.table(
  area      = "LC",
  ano       = 2020,
  codigo    = as.numeric(c(577, 578, 579, 580, 584, 585, 691, 692, 693, 694)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Branca",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras",
                "Azul (Digital)", "Amarela (Digital)", "Branca (Digital)", "Rosa (Digital)"),
  aplicacao = c(rep("P1", 10))
)

# --- 2. Ciências Humanas (CH) ---
ch_2020 <- data.table(
  area      = "CH",
  ano       = 2020,
  codigo    = as.numeric(c(567, 568, 569, 570, 574, 575, 687, 688, 689, 690)),  # P1
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras",
                "Azul (Digital)", "Amarela (Digital)", "Branca (Digital)", "Rosa (Digital)"),
  aplicacao = c(rep("P1", 10))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2020 <- data.table(
  area      = "CN",
  ano       = 2020,
  codigo    = as.numeric(c(597, 598, 599, 600, 604, 605, 699, 700, 701, 702)), # P1
  cor       = c("Azul", "Amarela", "Cinza", "Rosa",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras",
                "Azul (Digital)", "Amarela (Digital)", "Rosa (Digital)", "Cinza (Digital)"),
  aplicacao = c(rep("P1", 10))
)

# --- 4. Matemática (MT) ---
mt_2020 <- data.table(
  area      = "MT",
  ano       = 2020,
  codigo    = as.numeric(c(587, 588, 589, 590, 594, 595, 695, 696, 697, 698)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Cinza",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras",
                "Azul (Digital)", "Amarela (Digital)", "Rosa (Digital)", "Cinza (Digital)"),
  aplicacao = c(rep("P1", 10))
)

# --- UNINDO TUDO ---
dic_2020 <- rbind(lc_2020, ch_2020, cn_2020, mt_2020)

# Salvando no pacote
usethis::use_data(dic_2020, overwrite = TRUE)

# ------
# 2021 -
# ------

library(data.table)

itens_2021 <- fread(input='exploration/2021/MICRODADOS/microdados_enem_2021/DADOS/ITENS_PROVA_2021.csv')

# --- 1. Linguagens e Códigos (LC) ---
lc_2021 <- data.table(
  area      = "LC",
  ano       = 2021,
  codigo    = as.numeric(c(889, 890, 891, 892, 896, 897, 1003, 1004, 1005, 1006)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Branca",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras",
                "Azul (Digital)", "Amarela (Digital)", "Branca (Digital)", "Rosa (Digital)"),
  aplicacao = c(rep("P1", 10))
)

# --- 2. Ciências Humanas (CH) ---
ch_2021 <- data.table(
  area      = "CH",
  ano       = 2021,
    codigo    = as.numeric(c(879, 880, 881, 882, 886, 887, 999, 1000, 1001, 1002)), # P1
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras",
                "Azul (Digital)", "Amarela (Digital)", "Branca (Digital)", "Rosa (Digital)"),
  aplicacao = c(rep("P1", 10))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2021 <- data.table(
  area      = "CN",
  ano       = 2021,
  codigo    = as.numeric(c(909, 910, 911, 912, 916, 917, 1011, 1012, 1013, 1014)), # P1
  cor       = c("Azul", "Amarela", "Cinza", "Rosa",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras",
                "Azul (Digital)", "Amarela (Digital)", "Rosa (Digital)", "Cinza (Digital)"),
  aplicacao = c(rep("P1", 10))
)

# --- 4. Matemática (MT) ---
mt_2021 <- data.table(
  area      = "MT",
  ano       = 2021,
  codigo    = as.numeric(c(899, 900, 901, 902, 906, 907, 1007, 1008, 1009, 1010)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Cinza",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras",
                "Azul (Digital)", "Amarela (Digital)", "Rosa (Digital)", "Cinza (Digital)"),
  aplicacao = c(rep("P1", 10))
)

# --- UNINDO TUDO ---
dic_2021 <- rbind(lc_2021, ch_2021, cn_2021, mt_2021)

# Salvando no pacote
usethis::use_data(dic_2021, overwrite = TRUE)
