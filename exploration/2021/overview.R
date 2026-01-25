#-------------------------------------------------------
# Escreve os arquivos json para aplicação web          -
# atenção: esse script pode demorar para ser executado -
#-------------------------------------------------------

# Importa os microdados
table <- fread("exploration/2021/MICRODADOS/microdados_enem_2021/DADOS/MICRODADOS_ENEM_2021.csv")

# Caminho para gravar o json
path_json <- as.character("~/Área\ de\ trabalho/DEV/NEXT/microenem/src/app/(home)/JSON/2021/visao-geral/overview/")

#-----------------------
# Inscritos e presença -
#-----------------------

# Escreve json sobre inscritos na prova
write_inscritos(table, path_json = path_json)

# Escreve json sobre presença em ao menos um dia na prova
write_presence(table, path_json = path_json)

# Escreve json sobre presença em ao menos um dia na prova
write_presence_day(table, path_json = path_json, day = 1)

# Escreve json sobre presença em ao menos um dia na prova
write_presence_day(table, path_json = path_json, day = 2)

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
