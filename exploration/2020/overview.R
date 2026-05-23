#-------------------------------------------------------
# Escreve os arquivos json para aplicação web          -
# atenção: esse script pode demorar para ser executado -
#-------------------------------------------------------

# Importa os microdados
table <- data.table::fread(
  input = file.path(
    "~/Downloads",
    "microdados_enem_2020/DADOS/MICRODADOS_ENEM_2020.csv"
  ),
  encoding = "UTF-8"
)

# Caminho para gravar o json
path_json <- as.character(
  "~/Área\ de\ trabalho/DEV/NEXT/microenem/
  src/app/(home)/JSON/2020/visao-geral/overview/"
)

#-----------------------
# Inscritos e presença -
#-----------------------

# Escreve json sobre inscritos na prova
write_inscritos(table, path_json = path_json)

# Escreve json sobre presença em ao menos um dia na prova
write_presence_day(table, path_json = path_json, day = 1)

# Escreve json sobre presença em ao menos um dia na prova
write_presence_day(table, path_json = path_json, day = 2)
