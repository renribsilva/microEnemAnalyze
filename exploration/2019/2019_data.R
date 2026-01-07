#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/json/")

#-----------------------------
# Escreve os arquivos json -
#-----------------------------

# Escreve json de itens
write_itens(path_json = path_json, ano = 2019)

# Escreve json de dic
write_dic(path_json = path_json, ano = 2019)

# Escreve o traço de probabilidade
write_probtrace(path_json = path_json, ano = 2019)

#-------------------------------------------------------------
# Escreve curva característica do teste de todos os cadernos -
#-------------------------------------------------------------

# Importa dados
data <- fread("exploration/2019/MICRODADOS/at_least_one_presence_nt.csv")

# Escreve a curva característica do exame por caderno, para um determinado ano
write_tcc(data, path_json = path_json, ano = 2019)
