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
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 2. Ciências Humanas (CH) ---
ch_2019 <- data.table(
  area      = "CH",
  ano       = 2019,
  codigo    = as.numeric(c(507, 508, 509, 510, 520, 524)),
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2019 <- data.table(
  area      = "CN",
  ano       = 2019,
  codigo    = as.numeric(c(503, 504, 505, 506, 519, 523)),
  cor       = c("Azul", "Amarela", "Cinza", "Rosa",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 4. Matemática (MT) ---
mt_2019 <- data.table(
  area      = "MT",
  ano       = 2019,
  codigo    = as.numeric(c(515, 516, 517, 518, 522, 526)),
  cor       = c("Azul", "Amarela", "Rosa", "Cinza",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
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
  codigo    = as.numeric(c(577, 578, 579, 580, 584, 585)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Branca",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 2. Ciências Humanas (CH) ---
ch_2020 <- data.table(
  area      = "CH",
  ano       = 2020,
  codigo    = as.numeric(c(567, 568, 569, 570, 574, 575)),  # P1
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2020 <- data.table(
  area      = "CN",
  ano       = 2020,
  codigo    = as.numeric(c(597, 598, 599, 600, 604, 605)), # P1
  cor       = c("Azul", "Amarela", "Cinza", "Rosa",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 4. Matemática (MT) ---
mt_2020 <- data.table(
  area      = "MT",
  ano       = 2020,
  codigo    = as.numeric(c(587, 588, 589, 590, 594, 595)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Cinza",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
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
  codigo    = as.numeric(c(889, 890, 891, 892, 896, 897)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Branca",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 2. Ciências Humanas (CH) ---
ch_2021 <- data.table(
  area      = "CH",
  ano       = 2021,
    codigo    = as.numeric(c(879, 880, 881, 882, 886, 887)), # P1
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2021 <- data.table(
  area      = "CN",
  ano       = 2021,
  codigo    = as.numeric(c(909, 910, 911, 912, 916, 917)), # P1
  cor       = c("Azul", "Amarela", "Cinza", "Rosa",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 4. Matemática (MT) ---
mt_2021 <- data.table(
  area      = "MT",
  ano       = 2021,
  codigo    = as.numeric(c(899, 900, 901, 902, 906, 907)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Cinza",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- UNINDO TUDO ---
dic_2021 <- rbind(lc_2021, ch_2021, cn_2021, mt_2021)

# Salvando no pacote
usethis::use_data(dic_2021, overwrite = TRUE)

# ------
# 2022 -
# ------

library(data.table)

itens_2022 <- fread(input='exploration/2022/MICRODADOS/microdados_enem_2022/DADOS/ITENS_PROVA_2022.csv')

# --- 1. Linguagens e Códigos (LC) ---
lc_2022 <- data.table(
  area      = "LC",
  ano       = 2022,
  codigo    = as.numeric(c(1065, 1066, 1067, 1068, 1072, 1073)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Branca",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 2. Ciências Humanas (CH) ---
ch_2022 <- data.table(
  area      = "CH",
  ano       = 2022,
  codigo    = as.numeric(c(1055, 1056, 1057, 1058, 1062, 1063)), # P1
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2022 <- data.table(
  area      = "CN",
  ano       = 2022,
  codigo    = as.numeric(c(1085, 1086, 1087, 1088, 1092, 1093)), # P1
  cor       = c("Azul", "Amarela", "Cinza", "Rosa",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- 4. Matemática (MT) ---
mt_2022 <- data.table(
  area      = "MT",
  ano       = 2022,
  codigo    = as.numeric(c(1075, 1076, 1077, 1078, 1082, 1083)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Cinza",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras"),
  tipo = c(rep("1", 4), rep("2", 1), rep("3", 1))
)

# --- UNINDO TUDO ---
dic_2022 <- rbind(lc_2022, ch_2022, cn_2022, mt_2022)

# Salvando no pacote
usethis::use_data(dic_2022, overwrite = TRUE)

# ------
# 2023 -
# ------

library(data.table)

itens_2023 <- fread(input='exploration/2023/MICRODADOS/microdados_enem_2023/DADOS/ITENS_PROVA_2023.csv')

# --- 1. Linguagens e Códigos (LC) ---
lc_2023 <- data.table(
  area      = "LC",
  ano       = 2023,
  codigo    = as.numeric(c(1201, 1202, 1203, 1204,
                           1205, 1206,
                           1207, 1208, 1209)),
  cor       = c("Azul", "Amarela", "Rosa", "Branca",
                "Rosa (Ampliada)", "Rosa (Superampliada)",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras", "Laranja - Braile"),
  tipo = c(rep("1", 6), rep("2", 1), rep("3", 1), rep("4", 1))
)

# --- 2. Ciências Humanas (CH) ---
ch_2023 <- data.table(
  area      = "CH",
  ano       = 2023,
  codigo    = as.numeric(c(1191, 1192, 1193, 1194,
                           1195, 1196,
                           1197, 1198, 1199)), # P1
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Rosa (Ampliada)", "Rosa (Superampliada)",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras", "Laranja - Braile"),
  tipo = c(rep("1", 6), rep("2", 1), rep("3", 1), rep("4", 1))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2023 <- data.table(
  area      = "CN",
  ano       = 2023,
  codigo    = as.numeric(c(1221, 1222, 1223, 1224,
                           1225, 1226,
                           1227, 1228, 1229)), # P1
  cor       = c("Azul", "Amarela", "Rosa", "Cinza",
                "Rosa (Ampliada)", "Rosa (Superampliada)",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras", "Laranja - Braile"),
  tipo = c(rep("1", 6), rep("2", 1), rep("3", 1), rep("4", 1))
)

# --- 4. Matemática (MT) ---
mt_2023 <- data.table(
  area      = "MT",
  ano       = 2023,
  codigo    = as.numeric(c(1211, 1212, 1213, 1214,
                           1215, 1216,
                           1217, 1218, 1219)),  # P1
  cor       = c("Azul", "Amarela", "Rosa", "Cinza",
                "Rosa (Ampliada)", "Rosa (Superampliada)",
                "Laranja - Adaptada Ledor", "Verde - Videoprova - Libras", "Laranja - Braile"),
  tipo = c(rep("1", 6), rep("2", 1), rep("3", 1), rep("4", 1))
)

# --- UNINDO TUDO ---
dic_2023 <- rbind(lc_2023, ch_2023, cn_2023, mt_2023)

# Salvando no pacote
usethis::use_data(dic_2023, overwrite = TRUE)

# ------
# 2024 -
# ------

library(data.table)

itens_2024 <- fread(input='exploration/2024/MICRODADOS/microdados_enem_2024/DADOS/ITENS_PROVA_2024.csv')

# --- 1. Linguagens e Códigos (LC) ---
lc_2024 <- data.table(
  area      = "LC",
  ano       = 2024,
  codigo    = as.numeric(c(1395, 1396, 1397, 1398,
                           1399, 1400,
                           1402, 1403)),
  cor       = c("Azul", "Amarela", "Verde", "Branca",
                "Verde (Ampliada)", "Verde (Superampliada)",
                "Laranja - Adaptada Ledor", "Roxa - Videoprova - Libras"),
  tipo = c(rep("1", 6), rep("2", 1), rep("3", 1))
)

# --- 2. Ciências Humanas (CH) ---
ch_2024 <- data.table(
  area      = "CH",
  ano       = 2024,
  codigo    = as.numeric(c(1383, 1384, 1385, 1386,
                           1387, 1388,
                           1390, 1391)), # P1
  cor       = c("Azul", "Amarela", "Branca", "Verde",
                "Verde (Ampliada)", "Verde (Superampliada)",
                "Laranja - Adaptada Ledor", "Roxa - Videoprova - Libras"),
  tipo = c(rep("1", 6), rep("2", 1), rep("3", 1))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2024 <- data.table(
  area      = "CN",
  ano       = 2024,
  codigo    = as.numeric(c(1419, 1420, 1421, 1422,
                           1423, 1424,
                           1426, 1427)), # P1
  cor       = c("Azul", "Amarela", "Verde", "Cinza",
                "Verde (Ampliada)", "Verde (Superampliada)",
                "Laranja - Adaptada Ledor", "Roxa - Videoprova - Libras"),
  tipo = c(rep("1", 6), rep("2", 1), rep("3", 1))
)

# --- 4. Matemática (MT) ---
mt_2024 <- data.table(
  area      = "MT",
  ano       = 2024,
  codigo    = as.numeric(c(1407, 1408, 1409, 1410,
                           1411, 1412,
                           1414, 1415)),  # P1
  cor       = c("Azul", "Amarela", "Verde", "Cinza",
                "Verde (Ampliada)", "Verde (Superampliada)",
                "Laranja - Adaptada Ledor", "Roxa - Videoprova - Libras"),
  tipo = c(rep("1", 6), rep("2", 1), rep("3", 1))
)

# --- UNINDO TUDO ---
dic_2024 <- rbind(lc_2024, ch_2024, cn_2024, mt_2024)

# Salvando no pacote
usethis::use_data(dic_2024, overwrite = TRUE)
