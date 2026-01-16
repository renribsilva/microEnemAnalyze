# ------
# 2019 -
# ------

library(data.table)

itens_2019 <- fread(input='exploration/2019/MICRODADOS/microdados_enem_2019/DADOS/ITENS_PROVA_2019.csv')

# --- 1. Linguagens e Códigos (LC) ---
lc_2019 <- data.table(
  area      = "LC",
  ano       = 2019,
  codigo    = as.numeric(c(511, 512, 513, 514, 521, 525, 551, 552, 553, 554, 565)),
  cor       = c("Azul", "Amarela", "Rosa", "Branca",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras",
                "Azul", "Amarela", "Branca", "Rosa",
                "Laranja - Adaptada Ledor"),
  aplicacao = c(rep("P1", 6), rep("P2", 5))
)

# --- 2. Ciências Humanas (CH) ---
ch_2019 <- data.table(
  area      = "CH",
  ano       = 2019,
  codigo    = as.numeric(c(507, 508, 509, 510, 520, 524, 547, 548, 549, 550, 564)),
  cor       = c("Azul", "Amarela", "Branca", "Rosa",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras",
                "Azul", "Amarela", "Branca", "Rosa",
                "Laranja - Adaptada Ledor"),
  aplicacao = c(rep("P1", 6), rep("P2", 5))
)

# --- 3. Ciências da Natureza (CN) ---
cn_2019 <- data.table(
  area      = "CN",
  ano       = 2019,
  codigo    = as.numeric(c(503, 504, 505, 506, 519, 523, 543, 544, 545, 546)),
  cor       = c("Azul", "Amarela", "Cinza", "Rosa",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras",
                "Amarela", "Cinza", "Azul", "Rosa"),
  aplicacao = c(rep("P1", 6), rep("P2", 4))
)

# --- 4. Matemática (MT) ---
mt_2019 <- data.table(
  area      = "MT",
  ano       = 2019,
  codigo    = as.numeric(c(515, 516, 517, 518, 522, 526, 555, 556, 557, 558)),
  cor       = c("Azul", "Amarela", "Rosa", "Cinza",
                "Laranja - Adaptada Ledor",
                "Verde - Videoprova - Libras",
                "Amarela", "Cinza", "Azul", "Rosa"),
  aplicacao = c(rep("P1", 6), rep("P2", 4))
)

# --- UNINDO TUDO ---
dic_2019 <- rbind(lc_2019, ch_2019, cn_2019, mt_2019)

# Salvando no pacote
usethis::use_data(dic_2019, overwrite = TRUE)

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

