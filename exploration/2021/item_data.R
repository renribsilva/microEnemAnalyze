#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2021/resposta-ao-item/")

#--------------------------------------------------------------------
# Escreve tabela com frequências de acerto e erro de todos os itens -
#--------------------------------------------------------------------

score_LC <- fread("exploration/2021/MICRODADOS/score_LC.csv")
score_ch <- fread("exploration/2021/MICRODADOS/score_ch.csv")
score_cn <- fread("exploration/2021/MICRODADOS/score_cn.csv")
score_mt <- fread("exploration/2021/MICRODADOS/score_mt.csv")

data <- list(score_LC, score_ch, score_cn, score_mt)

write_score_table(data = data, path_json = path_json)

write_score_graph(data = data, path_json = path_json)

