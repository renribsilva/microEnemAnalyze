#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/json/")

#-----------------------------
# Escreve os arquivos json -
#-----------------------------

# Escreve o traço de probabilidade das provas
write_probtrace(path_json = path_json, ano = 2019)

# Escreve json de itens
write_itens(path_json = path_json, ano = 2019)

# Escreve json de dic
write_dic(path_json = path_json, ano = 2019)
