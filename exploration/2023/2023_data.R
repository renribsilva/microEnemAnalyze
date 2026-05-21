#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character(
  "~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2023/"
)

#-----------------------------
# Escreve os arquivos json -
#-----------------------------

# Escreve json de itens
write_itens(path_json = path_json, ano = 2023)

# Escreve json de dic
write_dic(path_json = path_json, ano = 2023)

# Escreve o traço de probabilidade
write_probtrace(path_json = path_json, ano = 2023)

# Escreve o traço de informação
write_iteminfo(path_json = path_json, ano = 2023)

#-------------------------------------------------------------
# Escreve curva característica do teste de todos os cadernos -
#-------------------------------------------------------------

score_lc <- fread("exploration/2023/MICRODADOS/score_lc.csv")
score_ch <- fread("exploration/2023/MICRODADOS/score_ch.csv")
score_cn <- fread("exploration/2023/MICRODADOS/score_cn.csv")
score_mt <- fread("exploration/2023/MICRODADOS/score_mt.csv")

score <- list(score_lc, score_ch, score_cn, score_mt)

# Importa dados
data <- fread("exploration/2023/MICRODADOS/at_least_one_presence.csv")

# Escreve a curva característica do exame por caderno, para um determinado ano
write_tcc(data = data, score = score, path_json = path_json, ano = 2023)
