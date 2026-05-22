#--------------------------------
# Importa os dados.csv -
#--------------------------------

filtered <- fread(
  input = "~/Downloads/
  microdados_enem_2021/DADOS/MICRODADOS_ENEM_2021.csv",
  encoding = "UTF-8"
)

#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character(
  "~/Área\ de\ trabalho/DEV/NEXT/microenem/
  src/app/(home)/JSON/2021/visao-geral/socials/"
)

#----------------------------------------------------
# Frequência absoluta e relativa das faixas etárias -
#----------------------------------------------------

write_fx_etaria(filtered, path_json = path_json)

write_sexo(filtered, path_json = path_json)

write_cor_raca(filtered, path_json = path_json)
