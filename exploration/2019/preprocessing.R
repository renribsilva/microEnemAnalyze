#----------------------------------------------------------------------
# Importa os microdados e filtra para presentes em ao menos uma prova -
#----------------------------------------------------------------------

# Importa os microdados
table <- fread("exploration/2019/MICRODADOS/microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv")

# Caminho para gravar o csv
path_csv <- as.character("exploration/2019/MICRODADOS")

# Escreve um csv menor, filtrado para presentes em ao menos um dia da prova
filter_presence(table, path_csv = path_csv)
