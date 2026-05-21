#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2020/resposta-ao-item/")

#--------------------------------------------------------------------
# Escreve tabela com frequências de acerto e erro de todos os itens -
#--------------------------------------------------------------------

score_lc <- fread("exploration/2020/MICRODADOS/score_lc.csv")
score_ch <- fread("exploration/2020/MICRODADOS/score_ch.csv")
score_cn <- fread("exploration/2020/MICRODADOS/score_cn.csv")
score_mt <- fread("exploration/2020/MICRODADOS/score_mt.csv")

data <- list(score_lc, score_ch, score_cn, score_mt)

write_score_table(data = data, path_json = path_json)

write_score_graph(data = data, path_json = path_json)

