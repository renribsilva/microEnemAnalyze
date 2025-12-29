#--------------------------------
# Importa os table_filtered.csv -
#--------------------------------

filtered <- fread("2019/MICRODADOS/table_filtered.csv")
source("2019/process_area.R")

filtered <- filtered %>%
  dplyr::filter(NU_NOTA_CH != 0) %>%
  dplyr::filter(NU_NOTA_CH != 0) %>%
  dplyr::filter(NU_NOTA_CN != 0) %>%
  dplyr::filter(NU_NOTA_MT != 0)

#----------------------------------
# Importa os parâmetros dos itens -
#----------------------------------

itens_2019 <- fread(input='2019/MICRODADOS/microdados_enem_2019/DADOS/ITENS_PROVA_2019.csv')

itens_2019_filtered <- itens_2019 %>%
  dplyr::filter(CO_PROVA == 511 | CO_PROVA == 512 | CO_PROVA == 513 | CO_PROVA == 514 |
                CO_PROVA == 507 | CO_PROVA == 508 | CO_PROVA == 509 | CO_PROVA == 510 |
                CO_PROVA == 503 | CO_PROVA == 504 | CO_PROVA == 505 | CO_PROVA == 506 |
                CO_PROVA == 515 | CO_PROVA == 516 | CO_PROVA == 517 | CO_PROVA == 518 )

#----------------------------------
# Normaliza o vetor de linguagens -
#----------------------------------

# Vetores lógicos para decidir transformação
cond_L0 <- filtered$TP_LINGUA == 0 & nchar(filtered$TX_GABARITO_CH) > 45
cond_L1 <- filtered$TP_LINGUA == 1 & nchar(filtered$TX_GABARITO_CH) > 45

# Sempre remover "9" das respostas onde as condições forem verdade
cond_total <- cond_L0 | cond_L1
filtered$TX_RESPOSTAS_CH[cond_total] <- gsub("9", "", filtered$TX_RESPOSTAS_CH[cond_total])

# Ajuste do gabarito para TP_LINGUA == 0
filtered$TX_GABARITO_CH[cond_L0] <- paste0(
  substr(filtered$TX_GABARITO_CH[cond_L0], 1, 5),
  substr(filtered$TX_GABARITO_CH[cond_L0], 11, 9999)
)

# Ajuste do gabarito para TP_LINGUA == 1
filtered$TX_GABARITO_CH[cond_L1] <- substr(filtered$TX_GABARITO_CH[cond_L1], 6, 9999)

#-----------------------------------------------------
# Relação entre quantidade de acertos e proficiência -
#-----------------------------------------------------

# Cria matriz de binários para os acertos
CH_mat <- process_area(filtered$TX_RESPOSTAS_CH, filtered$TX_GABARITO_CH)

# Plota a distribuição
mirt::plot(filtered$NU_NOTA_CH, 
           rowSums(CH_mat), 
           main = "Relação entre quantidade de acertos e proficiência", 
           xlab = "Proficiência", 
           ylab = "Quantidade de acertos",
           # xlim = c(-4, 4),  # Ajustando o limite do eixo X para de -4 a 4
           ylim = c(min(rowSums(CH_mat)), max(rowSums(CH_mat))),  # Ajuste automático do limite do eixo Y
           cex = 0.1,
           cex.axis = 1)

abline(v = 684, lty = 2, lwd = 0.5, col = 'gray')
abline(h = 36, lty = 2, lwd = 0.5, col = 'gray')

#-------------
# Meus dados -
#-------------

# Encontra meus dados
my_data <- filtered %>%
  dplyr::filter(NU_NOTA_CH == 684) %>%
  dplyr::filter(NO_MUNICIPIO_PROVA == "Votuporanga")

my_score <- process_area(my_data$TX_RESPOSTAS_CH, my_data$TX_GABARITO_CH)

my_test <- my_data$CO_PROVA_CH

my_prof <- (my_data$NU_NOTA_CH - 500) / 100

my_pars <- itens_2019_filtered %>%
  dplyr::filter(CO_PROVA == my_test) %>%
  dplyr::filter(TP_LINGUA == 0 | is.na(TP_LINGUA)) %>%
  arrange(CO_POSICAO)

#---------------------------------------------
# Resumo estatístico das notas de linguagens -
#---------------------------------------------

describe(filtered$NU_NOTA_CH)

#---------------------------------------------------------------
# Traceline de probabilidades de erro e acerto de cada questão -
#---------------------------------------------------------------

theta <- seq(-6, 6, by = .01)
ls_traceline <- list()

# função para calcular a probabilidade de cada theta
cci_3pl <- function(theta, a, b, c) {
  c + (1 - c) / (1 + exp(-a * (theta - b)))
}

# itera sobre os itens e extrai um traceline de cada um
for (i in 1:ncol(my_score)) {
  # Verifica comprimento de my_score
  if (ncol(my_score) != nrow(my_pars) ) {
    stop(sprintf(
      "Erro: o número de itens de my_score é diferente do número de itens de my_pars",
      nrow(my_score),
      ncol(my_pars),
    ))
  }
  # Cria um traceline vazio
  traceline <- data.frame(
    p1 = rep(NA_real_, length(theta)),
    p0 = rep(NA_real_, length(theta)),
    theta = rep(NA_real_, length(theta))
  )
  #itera para cada theta
  for (k in 1:length(theta)) {
    p <- cci_3pl(theta[k], my_pars[i,]$NU_PARAM_A, my_pars[i,]$NU_PARAM_B, my_pars[i,]$NU_PARAM_C) 
    if (is.na(p)) p <- 0.5
    traceline[k,]$p1 <- p
    traceline[k,]$p0 <- 1 - p
    traceline[k,]$theta <- theta[k]
  }
  # guarda o traceline em uma lista
  ls_traceline[[i]] <- traceline
  cat("processando questão", i, "\n")
}

# str(ls_traceline)
head(ls_traceline)

add_item_curve <- function(p, q, my_score, ls_traceline, my_prof, alpha = 0.4) {
  resp <- my_score[1, q]
  col  <- if (resp == 1) "blue" else "red"
  yvar <- if (resp == 1) "p1" else "p0"
  p <- p +
    geom_line(
      data = ls_traceline[[q]],
      aes(x = theta, y = .data[[yvar]]),
      alpha = alpha,
      color = col
    )
  return(p)
}
z <- ggplot()
item <- 14
z <- add_item_curve(z, item, my_score, ls_traceline, my_prof, alpha = 0.4)
z +
  geom_vline(xintercept = my_prof, linetype = "dashed", linewidth = 0.5) +
  labs(
    x = "Proficiência (θ)",
    y = "Probabilidade",
    title = paste("Curva característica do item:", item)
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

#-----------------------------------------------------------------
# Produto das probabilidades, segundo o vetor de erros e acertos -
#-----------------------------------------------------------------

# # Inicia um vetor vazio
# prod_prob <- numeric(length(theta))
# 
# # Itera sobre os valores de theta, considerando erros e acertos
# for (t in seq_len(length(theta))) {
#   if (as.integer(ncol(my_score)) != as.integer(length(ls_traceline))) {
#     stop(sprintf(
#       "Erro: o número de itens de my_score é diferente do número de itens de ls_traceline",
#       nrow(my_score)
#     ))
#   }
#   probs_t <- c()
#   for (q in 1:length(ls_traceline)) {
#     if (my_score[1,q] == 1) {
#       probs_t[q] <- ls_traceline[[q]]$p1[t] 
#     } else if (my_score[1,q] == 0) {
#       probs_t[q] <- ls_traceline[[q]]$p0[t] 
#     }
#   }
#   prod_prob[t] <- prod(probs_t, na.rm = TRUE)
# }
# 
# df_prod_prob <- data.frame(theta = theta, probabilidade = prod_prob)
# df_prod_prob
# 
# ggplot() +
#   geom_line(data = df_prod_prob, aes(x = theta*100+500, y = probabilidade), linewidth = 0.5) +
#   scale_y_log10() +
#   labs(
#     x = "Proeficiência",
#     y = "Produto das probabilidades",
#     title = "Poeficiência dado um vetor de acertos"
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid = element_blank(),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
#   )
# 
# # Theta e Probabilidade
# Theta <- df_prod_prob$theta
# L_theta <- df_prod_prob$probabilidade  # produto das probabilidades dos itens
# 
# # Função densidade a priori (normal padrão)
# p_theta <- dnorm(Theta, mean = 0, sd = 2)
# 
# # Posterior (não normalizada)
# posterior <- L_theta * p_theta
# 
# plot(Theta*100+500, posterior, type = "l",
#      main = paste("Curva a Posteriori - Obs", obs),
#      xlab = "Proficiência (Theta)",
#      ylab = "Posterior (não normalizada)",
#      lwd = 2, log = "y")
# 
# # EAP manual
# theta_EAP <- sum(Theta * posterior) / sum(posterior)
# theta_EAP*100 + 500  # transformação para escala ENEM
# 
# # Adicionando a estimativa EAP
# abline(v = theta_EAP*100+500, lty = 2, lwd = 1)
# text(theta_EAP*100+500, max(posterior)*0.9, labels = paste0("Proeficiência = ", round(theta_EAP*100+500,3)),
#      pos = 4)

