library(data.table)
library(dplyr)

names <- c("TP_ENSINO",
           "TP_ST_CONCLUSAO",
           "TP_PRESENCA_LC",
           "TP_PRESENCA_CH",
           "TP_PRESENCA_CN",
           "TP_PRESENCA_MT",
           "TX_RESPOSTAS_LC",
           "TX_RESPOSTAS_CH",
           "TX_RESPOSTAS_CN",
           "TX_RESPOSTAS_MT",
           "TX_GABARITO_LC",
           "TX_GABARITO_CH",
           "TX_GABARITO_CN",
           "TX_GABARITO_MT",
           "CO_PROVA_LC",
           "CO_PROVA_CH",
           "CO_PROVA_CN",
           "CO_PROVA_MT",
           "NU_NOTA_LC",
           "NU_NOTA_CH",
           "NU_NOTA_CN",
           "NU_NOTA_MT",
           "Q73")

data <- fread("exploration/2009/MICRODADOS/microdados_enem_2009/DADOS/MICRODADOS_ENEM_2009.csv", select = names)
names(data)
data_LC <- data %>%
  dplyr::filter(TP_ENSINO == 1,      # Escola Pública
         TP_ST_CONCLUSAO == 1,      # Concluintes
         TP_PRESENCA_LC == 1,
         Q73 == "A") %>%
  slice(1:300000)

write.csv(data_LC, file = "exploration/2009/MICRODADOS/sample_constantes_LC.csv")

data_CH <- data %>%
  dplyr::filter(TP_ENSINO == 1,      # Escola Pública
         TP_ST_CONCLUSAO == 1,      # Concluintes
         TP_PRESENCA_CH == 1,
         Q73 == "A") %>%
  slice(1:300000)

write.csv(data_CH, file = "exploration/2009/MICRODADOS/sample_constantes_CH.csv")

data_CN <- data %>%
  dplyr::filter(TP_ENSINO == 1,      # Escola Pública
         TP_ST_CONCLUSAO == 1,      # Concluintes
         TP_PRESENCA_CN == 1,
         Q73 == "A") %>%
  slice(1:300000)

write.csv(data_CN, file = "exploration/2009/MICRODADOS/sample_constantes_CN.csv")

data_MT <- data %>%
  dplyr::filter(TP_ENSINO == 1,      # Escola Pública
         TP_ST_CONCLUSAO == 1,      # Concluintes
         TP_PRESENCA_MT == 1,
         Q73 == "A") %>%
  slice(1:300000)

write.csv(data_MT, file = "exploration/2009/MICRODADOS/sample_constantes_MT.csv")
