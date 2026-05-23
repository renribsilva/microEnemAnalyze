#-------------------------------------------------------
# Escreve os arquivos json para aplicação web          -
# atenção: esse script pode demorar para ser executado -
#-------------------------------------------------------

# Importa os microdados
table1 <- data.table::fread(
  input = file.path(
    "~/Downloads",
    "microdados_enem_2024/DADOS/PARTICIPANTES_2024.csv"
  ),
  encoding = "UTF-8"
)
table2 <- data.table::fread(
  input = file.path(
    "~/Downloads",
    "microdados_enem_2024/DADOS/RESULTADOS_2024.csv"
  ),
  encoding = "UTF-8"
)

# Caminho para gravar o json
path_json <- as.character(
  file.path(
    "~/Área\ de\ trabalho/DEV/NEXT/microenem",
    "src/app/(home)/JSON/2024/visao-geral/overview/"
  )
)

#-----------------------
# Inscritos e presença -
#-----------------------

# Escreve json sobre inscritos na prova
write_inscritos(table1, path_json = path_json)

# Escreve json sobre presença em ao menos um dia na prova
write_presence_day(table2, path_json = path_json, day = 1)

# Escreve json sobre presença em ao menos um dia na prova
write_presence_day(table2, path_json = path_json, day = 2)
