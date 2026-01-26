#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2022/resposta-ao-item/")

#--------------------------------------------------------------------
# Escreve tabela com frequências de acerto e erro de todos os itens -
#--------------------------------------------------------------------

score_LC <- fread("exploration/2022/MICRODADOS/score_LC.csv")
score_CH <- fread("exploration/2022/MICRODADOS/score_CH.csv")
score_CN <- fread("exploration/2022/MICRODADOS/score_CN.csv")
score_MT <- fread("exploration/2022/MICRODADOS/score_MT.csv")

data <- list(score_LC, score_CH, score_CN, score_MT)

write_score_table(data = data, path_json = path_json)

write_score_graph(data = data, path_json = path_json)

