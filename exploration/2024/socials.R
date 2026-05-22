#--------------------------------
# Importa os dados.csv -
#--------------------------------

filtered <- fread(
  input = "~/Downloads/
  microdados_enem_2024/DADOS/PARTICIPANTES_2024.csv",
  encoding = "UTF-8"
)

#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character(
  "~/Área\ de\ trabalho/DEV/NEXT/microenem/
  src/app/(home)/JSON/2024/visao-geral/socials/"
)

#----------------------------------------------------
# Frequência absoluta e relativa das faixas etárias -
#----------------------------------------------------

write_fx_etaria(filtered, path_json = path_json)

write_sexo(filtered, path_json = path_json)

write_cor_raca(filtered, path_json = path_json)
