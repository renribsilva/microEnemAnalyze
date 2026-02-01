#---------------------------------------------
# Importa score de LINGUAGENS e escreve JSON -
#---------------------------------------------

# Importa os scores
data <- fread("exploration/2021/MICRODADOS/score_LC.csv")

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2021/dificuldade-do-exame/LC/")

# Escreve a descrição unidimensional dos dados
write_describe_notas(data, path_json = path_json)

# Escreve um array contendo todas as notas
# write_notas(data, path_json = path_json)

# Escreve a densidade da distribuião das notas
write_density_notas(data, path_json = path_json)

# Escreve a frequência de acertos
write_frequency_acertos(data, path_json = path_json)

#---------------------------------------------
# Importa score de HUMANAS e escreve JSON -
#---------------------------------------------

# Importa os scores
data <- fread("exploration/2021/MICRODADOS/score_CH.csv")

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2021/dificuldade-do-exame/CH/")

# Escreve a descrição unidimensional dos dados
write_describe_notas(data, path_json = path_json)

# Escreve um array contendo todas as notas
# write_notas(data, path_json = path_json)

# Escreve a densidade da distribuião das notas
write_density_notas(data, path_json = path_json)

# Escreve a frequência de acertos
write_frequency_acertos(data, path_json = path_json)

#---------------------------------------------
# Importa score de NATUREZA e escreve JSON -
#---------------------------------------------

# Importa os scores
data <- fread("exploration/2021/MICRODADOS/score_CN.csv")

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2021/dificuldade-do-exame/CN/")

# Escreve a descrição unidimensional dos dados
write_describe_notas(data, path_json = path_json)

# Escreve um array contendo todas as notas
# write_notas(data, path_json = path_json)

# Escreve a densidade da distribuião das notas
write_density_notas(data, path_json = path_json)

# Escreve a frequência de acertos
write_frequency_acertos(data, path_json = path_json)

#---------------------------------------------
# Importa score de MATEMÁTICA e escreve JSON -
#---------------------------------------------

# Importa os scores
data <- fread("exploration/2021/MICRODADOS/score_MT.csv")

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2021/dificuldade-do-exame/MT/")

# Escreve a descrição unidimensional dos dados
write_describe_notas(data, path_json = path_json)

# Escreve um array contendo todas as notas
# write_notas(data, path_json = path_json)

# Escreve a densidade da distribuião das notas
write_density_notas(data, path_json = path_json)

# Escreve a frequência de acertos
write_frequency_acertos(data, path_json = path_json)

