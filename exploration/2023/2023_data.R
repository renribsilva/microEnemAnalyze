#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2023/")

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

score_LC <- fread("exploration/2023/MICRODADOS/score_LC.csv")
score_CH <- fread("exploration/2023/MICRODADOS/score_CH.csv")
score_CN <- fread("exploration/2023/MICRODADOS/score_CN.csv")
score_MT <- fread("exploration/2023/MICRODADOS/score_MT.csv")

score <- list(score_LC,  score_CH, score_CN, score_MT)

# Importa dados
data <- fread("exploration/2023/MICRODADOS/at_least_one_presence.csv")

# Escreve a curva característica do exame por caderno, para um determinado ano
write_tcc(data = data, score = score, path_json = path_json, ano = 2023)
