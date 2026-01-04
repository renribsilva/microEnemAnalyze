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
