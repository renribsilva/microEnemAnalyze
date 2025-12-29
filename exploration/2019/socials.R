#--------------------------------
# Importa os table_lc_score.csv -
#--------------------------------

filtered <- fread("exploration/2019/MICRODADOS/at_least_one_presence.csv")

#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/json/socials/")

#----------------------------------------------------
# Frequência absoluta e relativa das faixas etárias -
#----------------------------------------------------

write_fx_etaria(filtered, path_json = path_json)

# Faixa etária
labels_etaria <- c(
  "Menor de 20 anos", # 1 a 4
  "20-25 anos",       # 5 a 10
  "26-30 anos",       # 11
  "31-35 anos",       # 12
  "36-40 anos",       # 13
  "41-45 anos",       # 14
  "46-50 anos",       # 15
  "51-55 anos",       # 16
  "56-60 anos",       # 17
  "Maior de 60 anos"  # 18 a 20
)

# Gerar as frequências
fx_etaria_abs <- table(filtered$TP_FAIXA_ETARIA)
fx_etaria_abs_grouped <- c(
  "1-4" = sum(fx_etaria_abs[1:4]),
  "5-10" = sum(fx_etaria_abs[5:10]),
  fx_etaria_abs[11:17],
  "18-20" = sum(fx_etaria_abs[18:20])
)

fx_etaria_rel_grouped <- prop.table(fx_etaria_abs_grouped) * 100 # Em porcentagem

# Criar a estrutura para o Chart.js
objeto_etaria <- list(
  # Garantimos que os nomes venham do nosso vetor de labels, ignorando os nomes do table()
  labels = labels_etaria,
  datasets = list(
    list(
      data = as.numeric(round(fx_etaria_rel_grouped, 2)),
      abs_values = as.numeric(fx_etaria_abs_grouped)
    )
  )
)

# Exportar para arquivo JSON
write_json(objeto_etaria, path = file.path(path_json, "faixa_etaria.json"), pretty = TRUE, auto_unbox = TRUE)

#-------------------------------------------------
# Frequência absoluta e relativa do tipo de sexo -
#-------------------------------------------------

# Faixa etária
labels_sexo <- c(
  "Feminino",
  "Masculino")

# Gerar as frequências
sexo_abs <- table(filtered$TP_SEXO)
sexo_rel <- prop.table(sexo_abs) * 100 # Em porcentagem

# Criar a estrutura para o Chart.js
objeto_sexo <- list(
  # Garantimos que os nomes venham do nosso vetor de labels, ignorando os nomes do table()
  labels = labels_sexo,
  datasets = list(
    list(
      data = as.numeric(round(sexo_rel, 2)),
      abs_values = as.numeric(sexo_abs)
    )
  )
)

# Exportar para arquivo JSON
write_json(objeto_sexo, path = file.path(path_json, "sexo.json"), pretty = TRUE, auto_unbox = TRUE)

#-------------------------------------------------
# Frequência absoluta e relativa do tipo de cor ou raça -
#-------------------------------------------------

labels_cor_raca <- c(
  "Não declarado",
  "Branca",
  "Preta",
  "Parda",
  "Amarela",
  "Indígena")

# Gerar as frequências
cor_raca_abs <- table(filtered$TP_COR_RACA)
cor_raca_rel <- prop.table(cor_raca_abs) * 100

# Criar a estrutura para o Treemap (Array de Objetos)
# Cada linha do dataframe se tornará um objeto no JSON
df_treemap <- data.frame(
  label = labels_cor_raca,
  value = as.numeric(round(cor_raca_rel, 2)),
  abs = as.numeric(cor_raca_abs)
)

objeto_cor_raca <- list(
  datasets = list(
    list(
      tree = df_treemap, # O jsonlite converterá isso para [ {label: "Branca", value: 36.97, abs: ...}, ... ]
      key = "value",
      groups = list("label")
    )
  )
)

# Exportar para arquivo JSON
write_json(objeto_cor_raca, path = file.path(path_json, "cor_raca.json"), pretty = TRUE, auto_unbox = TRUE)

