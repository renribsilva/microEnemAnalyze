#----------------------------------------------------------------------
# Importa os microdados e filtra para presentes em ao menos uma prova -
#----------------------------------------------------------------------

# Importa os microdados
table <- data.table::fread(
  input = file.path(
    "~/Downloads",
    "microdados_enem_2021/DADOS/MICRODADOS_ENEM_2021.csv"
  ),
  encoding = "UTF-8"
)

path_csv <- as.character(
  "exploration/2021/MICRODADOS/at_least_one_presence.csv"
)

# Escreve um csv menor, filtrado para presentes em ao menos um dia da prova
filter_presence(table, path_csv = path_csv)

#-------------------------------------------------
# Escreve arquivos csv com scores para cada área -
#-------------------------------------------------

# Importa os microdados
data <- data.table::fread(
  "exploration/2021/MICRODADOS/at_least_one_presence.csv"
)

# Caminho para gravar o csv
path_csv <- as.character("exploration/2021/MICRODADOS")

# Escreve arquivos csv com score para cada área
write_score(data, path_csv = path_csv, ano = 2021)
