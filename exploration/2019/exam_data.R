#-------------------------------------------------------------
# Escreve curva característica do teste de todos os cadernos -
#-------------------------------------------------------------

# Cria caminho para salvar o arquivo JSON
path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/dados-do-exame/json/")

# Escreve a curva característica do exame por caderno, para um determinado ano
write_tcc(path_json = path_json, ano = 2019)

#---------------------------------------------
# Importa score de HUMANAS e escreve JSON -
#---------------------------------------------

# Importa os scores
data <- fread("exploration/2019/MICRODADOS/score_LC.csv")

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/dados-do-exame/json/LC/")

# Escreve a descrição unidimensional dos dados
write_describe(data, path_json = path_json)

# Escreve um array contendo todas as notas
write_notas(data, path_json = path_json)

# Escreve a densidade da distribuião das notas
write_density(data, path_json = path_json)

#---------------------------------------------
# Importa score de HUMANAS e escreve JSON -
#---------------------------------------------

# Importa os scores
data <- fread("exploration/2019/MICRODADOS/score_CH.csv")

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/dados-do-exame/json/CH/")

# Escreve a descrição unidimensional dos dados
write_describe(data, path_json = path_json)

# Escreve um array contendo todas as notas
write_notas(data, path_json = path_json)

# Escreve a densidade da distribuião das notas
write_density(data, path_json = path_json)

#---------------------------------------------
# Importa score de NATUREZA e escreve JSON -
#---------------------------------------------

# Importa os scores
data <- fread("exploration/2019/MICRODADOS/score_CN.csv")

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/dados-do-exame/json/CN/")

# Escreve a descrição unidimensional dos dados
write_describe(data, path_json = path_json)

# Escreve um array contendo todas as notas
write_notas(data, path_json = path_json)

# Escreve a densidade da distribuião das notas
write_density(data, path_json = path_json)

#---------------------------------------------
# Importa score de MATEMÁTICA e escreve JSON -
#---------------------------------------------

# Importa os scores
data <- fread("exploration/2019/MICRODADOS/score_MT.csv")

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/dados-do-exame/json/MT/")

# Escreve a descrição unidimensional dos dados
write_describe(data, path_json = path_json)

# Escreve um array contendo todas as notas
write_notas(data, path_json = path_json)

# Escreve a densidade da distribuião das notas
write_density(data, path_json = path_json)
