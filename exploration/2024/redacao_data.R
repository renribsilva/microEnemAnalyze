#---------------------------------------
# Importa os at_least_one_presence.csv -
#---------------------------------------

data <- fread("exploration/2024/MICRODADOS/microdados_enem_2024/DADOS/RESULTADOS_2024.csv")

#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2024/redacao/")

#--------------------------------------
# Escreve tabela com dados da redação -
#---------------------------------------

write_notas_redacao(data = data, path_json = path_json)

write_status_redacao(data = data, path_json = path_json)

write_comp_redacao(data = data, path_json = path_json)
