#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character(
  file.path(
    "~/Área\ de\ trabalho/DEV/NEXT/microenem",
    "src/app/(home)/JSON/2025/resposta-ao-item/"
  )
)

#--------------------------------------------------------------------
# Escreve tabela com frequências de acerto e erro de todos os itens -
#--------------------------------------------------------------------

# 1. Definição dos prefixos para ignorar
prefixos_ignore <- c(
  "NU_ANO",
  "NU_INSCRICAO",
  "TP_LINGUA",
  "NU_SCORE",
  "TP_PRESENCA",
  "CO_PROVA",
  "NU_NOTA",
  "TX_RESPOSTAS",
  "TX_GABARITO"
)

regex_ignore <- paste0("^(", paste(prefixos_ignore, collapse = "|"), ")")

fread_otimizado <- function(caminho_arquivo) {
  cabecalho <- names(data.table::fread(caminho_arquivo, nrows = 0))
  colunas_para_ignorar <- cabecalho[grepl(regex_ignore, cabecalho)]
  colunas_para_ignorar <- colunas_para_ignorar[
    !grepl("^(NU_ANO|NU_NOTA_|CO_PROVA_)", colunas_para_ignorar)
  ]
  data.table::fread(caminho_arquivo, drop = colunas_para_ignorar)
}

score_lc <- fread_otimizado("exploration/2025/MICRODADOS/score_lc.csv")
score_ch <- fread_otimizado("exploration/2025/MICRODADOS/score_ch.csv")
score_cn <- fread_otimizado("exploration/2025/MICRODADOS/score_cn.csv")
score_mt <- fread_otimizado("exploration/2025/MICRODADOS/score_mt.csv")

data <- list(score_lc, score_ch, score_cn, score_mt)

write_score_table(data = data, path_json = path_json)

write_score_graph(data = data, path_json = path_json)
