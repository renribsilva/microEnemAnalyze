#------------------------
# Importa os microdados -
#------------------------

table <- fread("2019/MICRODADOS/microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv")

# Filtra colunas
# table <- table %>%
#   dplyr::select(-NU_ANO,
#                 -TP_ESTADO_CIVIL, -TP_NACIONALIDADE,
#                 -TP_ST_CONCLUSAO, -TP_ANO_CONCLUIU,
#                 -CO_MUNICIPIO_ESC, -NO_MUNICIPIO_ESC, 
#                 -CO_UF_ESC, -SG_UF_ESC, -TP_DEPENDENCIA_ADM_ESC,
#                 -TP_LOCALIZACAO_ESC, -TP_SIT_FUNC_ESC,
#                 -Q001, -Q002, -Q003, -Q004, -Q005, -Q006, -Q007, 
#                 -Q008, -Q009, -Q010, -Q011, -Q012, -Q013, -Q014, 
#                 -Q015, -Q016, -Q017, -Q018, -Q019, -Q020, -Q021,
#                 -Q022, -Q023, -Q024, -Q025)

#-----------------------------
# Caminho para gravar o json -
#-----------------------------

path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/2019/json/overview/")

#------------
# Inscritos -
#------------

inscritos <- as.integer()

if (!any(is.na(table$NU_INSCRICAO))) {
  inscritos <- as.integer(length(table$NU_INSCRICAO))
} else {
  stop("Merda")
}

treineiros <- table(table$IN_TREINEIRO)
treineiros_f <- prop.table(treineiros)
tabela_treineiros <- t(as.data.table(rbind(treineiros, treineiros_f)))
total_treineiros <- c(sum(tabela_treineiros[,1]), sum(tabela_treineiros[,2]))

if (as.integer(inscritos) == as.integer(total_treineiros[1])) {
  tabela_treineiros <- rbind(tabela_treineiros, total_treineiros)
  tabela_treineiros[,2] <- round(tabela_treineiros[,2]*100, 2)
} else {
  stop ("Merda")
}

objeto_inscritos <- list(
  list(
    grupo = "Inscritos",
    total = as.numeric(tabela_treineiros[,1][3]),
    freq = as.numeric(tabela_treineiros[,2][3]),
    subRows = list(
      list(grupo = "Não treineiros",
           total = as.numeric(tabela_treineiros[,1][1]),
           freq = as.numeric(tabela_treineiros[,2][1])
           ),
      list(grupo = "Treineiros", 
           total = as.numeric(tabela_treineiros[,1][2]),
           freq = as.numeric(tabela_treineiros[,2][2]))
    )
  )
)

write_json(objeto_inscritos, path = file.path(path_json, "inscritos.json"), pretty = TRUE, auto_unbox = TRUE)

#-----------------------
# Rotulagem de fatores -
#-----------------------

# table$IN_TREINEIRO <- factor(table$IN_TREINEIRO, levels = c(1,0),  labels=c('Sim','Não'))
# table$TP_SEXO <- factor(table$TP_SEXO, levels = c('M','F'), labels=c('Maculino','Feminino'))
# table$TP_COR_RACA <- factor(table$TP_COR_RACA, levels = c(0,1,2,3,4,5,6),
#                                labels=c('Não declarado',
#                                         'Branca','Preta',
#                                         'Parda','Amarela',
#                                         'Indígena',
#                                         'Não dispõe da informação'))
# table$TP_ESCOLA <- factor(table$TP_ESCOLA, levels = c(1,2,3,4),
#                              labels=c('Não respondeu',
#                                       'Pública',
#                                       'Privada',
#                                       'Exterior'))
# table$TP_PRESENCA_CN <- factor(table$TP_PRESENCA_CN, levels = c(0,1,2),
#                                    labels=c('Faltou na prova',
#                                            'Presente na prova',
#                                            'Eliminado na prova'))
# table$TP_PRESENCA_CH <- factor(table$TP_PRESENCA_CH, levels = c(0,1,2),
#                                   labels=c('Faltou na prova',
#                                            'Presente na prova',
#                                            'Eliminado na prova'))
# table$TP_PRESENCA_LC <- factor(table$TP_PRESENCA_LC, levels = c(0,1,2),
#                                   labels=c('Faltou na prova',
#                                            'Presente na prova',
#                                            'Eliminado na prova'))
# table$TP_PRESENCA_MT <- factor(table$TP_PRESENCA_MT, levels = c(0,1,2),
#                                   labels=c('Faltou na prova',
#                                            'Presente na prova',
#                                            'Eliminado na prova'))
# table$CO_PROVA_CN <- factor(table$CO_PROVA_CN, levels = c(503,504,505,506,519,523,543,544,545,546),
#                                labels=c('Azul','Amarela','Cinza',
#                                         'Rosa','Laranja - Adaptada Ledor',
#                                         'Verde - Videoprova - Libras)',
#                                         'Amarela (Reaplicação)',
#                                         'Cinza (Reaplicação)',
#                                         'Azul (Reaplicação)',
#                                         'Rosa (Reaplicação)'))
# table$CO_PROVA_CH <- factor(table$CO_PROVA_CH, levels = c(507,508,509,510,520,524,547,548,549,550,564),
#                                labels=c('Azul','Amarela','Branca',
#                                        'Rosa','Laranja - Adaptada Ledor',
#                                        'Verde - Videoprova - Libras)',
#                                        'Azul (Reaplicação)',
#                                        'Amarela (Reaplicação)',
#                                        'Branca (Reaplicação)',
#                                        'Rosa (Reaplicação)',
#                                        'Laranja - Adaptada Ledor (Reaplicação)'))
# table$CO_PROVA_LC <- factor(table$CO_PROVA_LC, levels = c(511,512,513,514,521,525,551,552,553,554,565),
#                                labels=c('Azul','Amarela','Rosa','Branca',
#                                         'Laranja - Adaptada Ledor',
#                                         'Verde - Videoprova - Libras)',
#                                         'Azul (Reaplicação)',
#                                         'Amarela (Reaplicação)',
#                                         'Branca (Reaplicação)',
#                                         'Rosa (Reaplicação)',
#                                         'Laranja - Adaptada Ledor (Reaplicação)'))
# table$CO_PROVA_MT <- factor(table$CO_PROVA_MT, levels = c(515,516,517,518,522,526,555,556,557,558),
#                                 labels=c('Azul','Amarela','Rosa',
#                                          'Cinza',
#                                          'Laranja - Adaptada Ledor',
#                                          'Verde - Videoprova - Libras)',
#                                          'Amarela (Reaplicação)',
#                                          'Cinza (Reaplicação)',
#                                          'Azul (Reaplicação)',
#                                          'Rosa (Reaplicação)'))
# table$ TP_LINGUA <- factor(table$ TP_LINGUA, levels = c(0,1),
#                                labels=c('Inglês','Espanhol'))
# table$TP_STATUS_REDACAO <- factor(table$TP_STATUS_REDACAO, levels = c(1,2,3,4,5,6,7,8,9),
#                                      labels=c('Sem problemas',
#                                               'Anulada','Cópia Texto Motivador',
#                                               'Em Branco','Fere Direitos Humanos',
#                                               'Fuga ao tema',
#                                               'Não atendimento ao tipo',
#                                               'Texto insuficiente',
#                                               'Parte desconectada'))

#--------------------------------------
# Filtra para pelo menos uma presença -
#--------------------------------------

batch_size <- 50000
total_rows <- nrow(table)
num_batches <- ceiling((total_rows/batch_size))

at_least_one_presence <- data.table()

for (i in 1:num_batches) {
  start_row <- (i-1)*batch_size+1
  end_row <- min(i*batch_size, total_rows)
  dados_batch <- table[start_row:end_row]
  dados_batch_filtered <- dados_batch[
    (TP_PRESENCA_LC == 1 | TP_PRESENCA_CH == 1 | TP_PRESENCA_CN == 1 | TP_PRESENCA_MT == 1)
  ]
  at_least_one_presence <- rbindlist(list(at_least_one_presence, dados_batch_filtered))
  cat("Batch", i, "de", num_batches, "processado", "(",start_row, " até ", end_row,")\n")
  rm(start_row, end_row, dados_batch, dados_batch_filtered)
  gc()
}

inscritos_filtered <- as.integer()

if (!any(is.na(at_least_one_presence$NU_INSCRICAO))) {
  inscritos_filtered <- as.integer(length(at_least_one_presence$NU_INSCRICAO))
} else {
  stop("Merda")
}

treineiros_filtered <- table(at_least_one_presence$IN_TREINEIRO)
treineiros_filtered_f <- prop.table(treineiros_filtered)
tabela_treineiros_filtered <- t(as.data.table(rbind(treineiros_filtered, treineiros_filtered_f)))
total_treineiros_filtered <- c(sum(tabela_treineiros_filtered[,1]), sum(tabela_treineiros_filtered[,2]))

if (as.integer(inscritos_filtered) == as.integer(total_treineiros_filtered[1])) {
  tabela_treineiros_filtered <- rbind(tabela_treineiros_filtered, total_treineiros_filtered)
  tabela_treineiros_filtered[,2] <- round(tabela_treineiros_filtered[,2]*100, 2)
} else {
  stop ("Merda")
}

objeto_presenca <- list(
  list(
    grupo = "Presentes na prova",
    total = as.numeric(tabela_treineiros_filtered[,1][3]),
    freq = as.numeric(tabela_treineiros_filtered[,2][3]),
    subRows = list(
      list(grupo = "Não treineiros*",
           total = as.numeric(tabela_treineiros_filtered[,1][1]),
           freq = as.numeric(tabela_treineiros_filtered[,2][1])
      ),
      list(grupo = "Treineiros", 
           total = as.numeric(tabela_treineiros_filtered[,1][2]),
           freq = as.numeric(tabela_treineiros_filtered[,2][2]))
    )
  )
)

write_json(objeto_presenca, path = file.path(path_json, "presenca.json"), pretty = TRUE, auto_unbox = TRUE)
write.csv(at_least_one_presence, file = paste0("2019/MICRODADOS/at_least_one_presence.csv"), row.names = FALSE)

#--------------------------------
# Filtra para presença no dia 1 -
#--------------------------------

table <- table %>%
  dplyr::select("NU_INSCRICAO", "TP_PRESENCA_LC", "TP_PRESENCA_CH",
                "TP_PRESENCA_CN", "TP_PRESENCA_MT")
rm(at_least_one_presence)
gc()

presence_day1 <- data.table()

for (i in 1:num_batches) {
  start_row <- (i-1)*batch_size+1
  end_row <- min(i*batch_size, total_rows)
  dados_batch <- table[start_row:end_row]
  dados_batch_filtered <- dados_batch[
    (TP_PRESENCA_LC == 1 | TP_PRESENCA_CH == 1)
  ]
  presence_day1 <- rbindlist(list(presence_day1, dados_batch_filtered))
  cat("Batch", i, "de", num_batches, "processado", "(",start_row, " até ", end_row,")\n")
  rm(start_row, end_row, dados_batch, dados_batch_filtered)
  gc()
}

inscritos_filtered <- as.integer()

if (!any(is.na(presence_day1$NU_INSCRICAO))) {
  inscritos_filtered <- as.integer(length(presence_day1$NU_INSCRICAO))
} else {
  stop("Merda")
}

objeto_presenca_dia1 <- list(
  list(
    grupo = "Presentes na prova",
    total = inscritos_filtered,
    abst = round(((inscritos-inscritos_filtered)/inscritos)*100, 2)
  )
)

write_json(objeto_presenca_dia1, path = file.path(path_json, "presenca_dia1.json"), pretty = TRUE, auto_unbox = TRUE)

#--------------------------------
# Filtra para presença no dia 2 -
#--------------------------------

rm(presence_day1)
gc()

presence_day2 <- data.table()

for (i in 1:num_batches) {
  start_row <- (i-1)*batch_size+1
  end_row <- min(i*batch_size, total_rows)
  dados_batch <- table[start_row:end_row]
  dados_batch_filtered <- dados_batch[
    (TP_PRESENCA_CN == 1 | TP_PRESENCA_MT == 1)
  ]
  presence_day2 <- rbindlist(list(presence_day2, dados_batch_filtered))
  cat("Batch", i, "de", num_batches, "processado", "(",start_row, " até ", end_row,")\n")
  rm(start_row, end_row, dados_batch, dados_batch_filtered)
  gc()
}

inscritos_filtered <- as.integer()

if (!any(is.na(presence_day2$NU_INSCRICAO))) {
  inscritos_filtered <- as.integer(length(presence_day2$NU_INSCRICAO))
} else {
  stop("Merda")
}

objeto_presenca_dia2 <- list(
  list(
    grupo = "Presentes na prova",
    total = inscritos_filtered,
    abst = round(((inscritos -inscritos_filtered)/inscritos)*100, 2)
  )
)

write_json(objeto_presenca_dia2, path = file.path(path_json, "presenca_dia2.json"), pretty = TRUE, auto_unbox = TRUE)
