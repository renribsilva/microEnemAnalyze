#-----------------------
# Path e data -
#-----------------------

path <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2022/media-simples/")
data <- fread("exploration/2022/MICRODADOS/at_least_one_presence.csv")

#-----------------------
# Executions -
#-----------------------

write_mean_table(data = data, path = path)

write_mean_describe(data = data, path = path)


