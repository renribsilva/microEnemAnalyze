#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character(
  "~/Área\ de\ trabalho/DEV/NEXT/microenem/
  src/app/(home)/JSON/2022/notas-e-acertos/"
)

#--------------------------------------------------------------------
# Escreve tabela com frequências de acerto e erro de todos os itens -
#--------------------------------------------------------------------

score_lc <- fread("exploration/2022/MICRODADOS/score_lc.csv")
score_ch <- fread("exploration/2022/MICRODADOS/score_ch.csv")
score_cn <- fread("exploration/2022/MICRODADOS/score_cn.csv")
score_mt <- fread("exploration/2022/MICRODADOS/score_mt.csv")

# Precisa de identificadores
data <- list(LC = score_lc, CH = score_ch, CN = score_cn, MT = score_mt)

write_score_describe(data = data, path_json = path_json)
