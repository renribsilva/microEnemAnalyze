#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character(
  "~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2025/"
)

#-----------------------------
# Escreve os arquivos json -
#-----------------------------

# Escreve json de itens
write_itens(path_json = path_json, ano = 2025)

# Escreve json de dic
write_dic(path_json = path_json, ano = 2025)

# Escreve o traço de probabilidade
write_probtrace(path_json = path_json, ano = 2025)

# Escreve o traço de informação
write_iteminfo(path_json = path_json, ano = 2025)

#-------------------------------------------------------------
# Escreve curva característica do teste de todos os cadernos -
#-------------------------------------------------------------

score_lc <- data.table::fread("exploration/2025/MICRODADOS/score_lc.csv")
score_ch <- data.table::fread("exploration/2025/MICRODADOS/score_ch.csv")
score_cn <- data.table::fread("exploration/2025/MICRODADOS/score_cn.csv")
score_mt <- data.table::fread("exploration/2025/MICRODADOS/score_mt.csv")

score <- list(score_lc, score_ch, score_cn, score_mt)

col_names <- names(data.table::fread(
  "exploration/2025/MICRODADOS/at_least_one_presence.csv",
  nrows = 1
))

colunas_necessarias <- col_names[grepl("^(NU_NOTA_|CO_PROVA_)", col_names)]

# Importa dados
data <- data.table::fread(
  "exploration/2025/MICRODADOS/at_least_one_presence.csv",
  select = colunas_necessarias
)

# Escreve a curva característica do exame por caderno, para um determinado ano
write_tcc(data = data, score = score, path_json = path_json, ano = 2025)
