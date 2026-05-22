#---------------------------------------
# Importa os at_least_one_presence.csv -
#---------------------------------------

data <- fread(
  input = "~/Downloads/
  microdados_enem_2023/DADOS/MICRODADOS_ENEM_2023.csv",
  encoding = "UTF-8"
)

#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character(
  "~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2023/redacao/"
)

#--------------------------------------
# Escreve tabela com dados da redação -
#---------------------------------------

write_notas_redacao(data = data, path_json = path_json)

write_status_redacao(data = data, path_json = path_json)

write_comp_redacao(data = data, path_json = path_json)
