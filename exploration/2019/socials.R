#--------------------------------
# Importa os table_lc_score.csv -
#--------------------------------

filtered <- fread("exploration/2019/MICRODADOS/at_least_one_presence_nt.csv")

#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/visao-geral/json/socials/")

#----------------------------------------------------
# Frequência absoluta e relativa das faixas etárias -
#----------------------------------------------------

write_fx_etaria(filtered, path_json = path_json)

write_sexo(filtered, path_json = path_json)

write_cor_raca(filtered, path_json = path_json)
