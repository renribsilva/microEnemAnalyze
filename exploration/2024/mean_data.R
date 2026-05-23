#-----------------------
# Path e data -
#-----------------------

path <- as.character(
  file.path(
    "~/Área\ de\ trabalho/DEV/NEXT/microenem",
    "src/app/(home)/JSON/2024/media-simples/"
  )
)

data <- data.table::fread(
  "exploration/2024/MICRODADOS/at_least_one_presence.csv"
)

#-----------------------
# Executions -
#-----------------------

write_mean_table(data = data, path = path)

write_mean_describe(data = data, path = path)
