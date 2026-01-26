#----------------------------------------------------------------------
# Importa os microdados e filtra para presentes em ao menos uma prova -
#----------------------------------------------------------------------

# Importa os microdados
table <- fread("exploration/2023/MICRODADOS/microdados_enem_2023/DADOS/MICRODADOS_ENEM_2023.csv")
# sort(unique(table$CO_PROVA_LC))
# sort(unique(table$CO_PROVA_CH))
# sort(unique(table$CO_PROVA_CN))
# sort(unique(table$CO_PROVA_MT))
# Caminho para gravar o csv
path_csv <- as.character("exploration/2023/MICRODADOS/at_least_one_presence.csv")

# Escreve um csv menor, filtrado para presentes em ao menos um dia da prova
filter_presence(table, path_csv = path_csv)

#-----------------------------------------------------
# Importa os microdados e filtra para não treineiros -
#-----------------------------------------------------

# Importa os microdados
# table <- fread("exploration/2023/MICRODADOS/microdados_enem_2023/DADOS/MICRODADOS_ENEM_2023.csv")

# Caminho para gravar o csv
# path_csv <- as.character("exploration/2023/MICRODADOS/at_least_one_presence_nt.csv")

# Escreve um csv menor, filtrado para presentes em ao menos
# um dia da prova e não treineiros
# filter_presence(table, path_csv = path_csv, nt = TRUE)

#-------------------------------------------------
# Escreve arquivos csv com scores para cada área -
#-------------------------------------------------

# Importa os microdados
data <- fread("exploration/2023/MICRODADOS/at_least_one_presence.csv")

# Caminho para gravar o csv
path_csv <- as.character("exploration/2023/MICRODADOS")

# Escreve arquivos csv com score para cada área
write_score(data, path_csv = path_csv, ano = 2023)
