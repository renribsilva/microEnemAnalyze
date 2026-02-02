#--------------------------------
# Importa os dados.csv -
#--------------------------------

filtered <- fread("exploration/2019/MICRODADOS/microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv")

#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2019/visao-geral/socials/")

#----------------------------------------------------
# Frequência absoluta e relativa das faixas etárias -
#----------------------------------------------------

write_fx_etaria(filtered, path_json = path_json)

write_sexo(filtered, path_json = path_json)

write_cor_raca(filtered, path_json = path_json)
