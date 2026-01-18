#---------------------------------------
# Importa os at_least_one_presence.csv -
#---------------------------------------

data <- fread("exploration/2019/MICRODADOS/at_least_one_presence.csv")

#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/redacao/json/")

#--------------------------------------
# Escreve tabela com dados da redação -
#---------------------------------------

write_notas_redacao(data = data, path_json = path_json)

write_status_redacao(data = data, path_json = path_json)

write_comp_redacao(data = data, path_json = path_json)
