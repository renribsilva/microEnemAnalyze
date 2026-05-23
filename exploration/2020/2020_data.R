#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character(
  "~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2020/"
)

#-----------------------------
# Escreve os arquivos json -
#-----------------------------

# Escreve json de itens
write_itens(path_json = path_json, ano = 2020)

# Escreve json de dic
write_dic(path_json = path_json, ano = 2020)

# Escreve o traço de probabilidade
write_probtrace(path_json = path_json, ano = 2020)

# Escreve o traço de informação
write_iteminfo(path_json = path_json, ano = 2020)

#-------------------------------------------------------------
# Escreve curva característica do teste de todos os cadernos -
#-------------------------------------------------------------

score_lc <- data.table::fread("exploration/2020/MICRODADOS/score_lc.csv")
score_ch <- data.table::fread("exploration/2020/MICRODADOS/score_ch.csv")
score_cn <- data.table::fread("exploration/2020/MICRODADOS/score_cn.csv")
score_mt <- data.table::fread("exploration/2020/MICRODADOS/score_mt.csv")

score <- list(score_lc, score_ch, score_cn, score_mt)

# Importa dados
data <- data.table::fread(
  "exploration/2020/MICRODADOS/at_least_one_presence.csv"
)

# Escreve a curva característica do exame por caderno, para um determinado ano
write_tcc(data = data, score = score, path_json = path_json, ano = 2020)
