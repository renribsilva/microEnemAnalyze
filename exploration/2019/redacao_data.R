library(data.table)

data <- fread("exploration/2019/MICRODADOS/at_least_one_presence.csv")
names(data)
cols <- c("NU_INSCRICAO",
          "TP_STATUS_REDACAO",
          "NU_NOTA_COMP1", "NU_NOTA_COMP2", "NU_NOTA_COMP3", "NU_NOTA_COMP4","NU_NOTA_COMP5", "NU_NOTA_REDACAO",
          "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT")
data_redacao <- data[, ..cols]
table(data_redacao$TP_STATUS_REDACAO)
table(data_redacao$NU_NOTA_COMP1)
table(data_redacao$NU_NOTA_REDACAO)
