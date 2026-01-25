#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2021/notas-e-acertos/")

#--------------------------------------------------------------------
# Escreve tabela com frequências de acerto e erro de todos os itens -
#--------------------------------------------------------------------

score_LC <- fread("exploration/2021/MICRODADOS/score_LC.csv")
score_CH <- fread("exploration/2021/MICRODADOS/score_CH.csv")
score_CN <- fread("exploration/2021/MICRODADOS/score_CN.csv")
score_MT <- fread("exploration/2021/MICRODADOS/score_MT.csv")

# Precisa de identificadores
data <- list(LC = score_LC, CH = score_CH, CN = score_CN, MT = score_MT)

write_score_describe(data = data, path_json = path_json)
