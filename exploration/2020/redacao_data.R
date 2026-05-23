#---------------------------------------
# Importa os at_least_one_presence.csv -
#---------------------------------------

data <- data.table::fread(
  input = file.path(
    "~/Downloads",
    "microdados_enem_2020/DADOS/MICRODADOS_ENEM_2020.csv"
  ),
  encoding = "UTF-8"
)

#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character(
  "~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2020/redacao/"
)

#--------------------------------------
# Escreve tabela com dados da redação -
#---------------------------------------

write_notas_redacao(data = data, path_json = path_json)

write_status_redacao(data = data, path_json = path_json)

write_comp_redacao(data = data, path_json = path_json)
