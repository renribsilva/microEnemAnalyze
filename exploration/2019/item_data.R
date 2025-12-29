#--------------------------------
# Importa os table_lc_score.csv -
#--------------------------------

filtered <- fread("exploration/2019/MICRODADOS/at_least_one_presence.csv")
lc_score <- fread("2019/MICRODADOS/score_lc.csv")
source("2019/process_area.R")

itens_2019 <- fread(input='2019/MICRODADOS/microdados_enem_2019/DADOS/ITENS_PROVA_2019.csv')
itens_2019_filtered <- itens_2019 %>%
  dplyr::filter(CO_PROVA == 511 | CO_PROVA == 512 | CO_PROVA == 513 | CO_PROVA == 514 | CO_PROVA == 521 | CO_PROVA == 525 |
                CO_PROVA == 507 | CO_PROVA == 508 | CO_PROVA == 509 | CO_PROVA == 510 | CO_PROVA == 520 | CO_PROVA == 524 |
                CO_PROVA == 503 | CO_PROVA == 504 | CO_PROVA == 505 | CO_PROVA == 506 | CO_PROVA == 519 | CO_PROVA == 523 |
                CO_PROVA == 515 | CO_PROVA == 516 | CO_PROVA == 517 | CO_PROVA == 518 | CO_PROVA == 522 | CO_PROVA == 526)

#------------------------------------------------
# Calcula frequência de acertos de cada questão -
#------------------------------------------------

# Definir quais colunas são os itens (ajuste o intervalo conforme seu dataframe)
ing <- paste0("QI_", 1:5)
esp <- paste0("QE_", 1:5)
oth <- paste0("Q_", 6:45)
colunas_itens <- c(ing, esp, oth)

dados_ing <- dplyr::filter(lc_score, TP_LINGUA == 0)
dados_esp <- dplyr::filter(lc_score, TP_LINGUA == 1)

# Criar a lista de tabelas para cada item
lista_ing <- lapply(ing, function(item) {
  tab_abs_ing <- table(dados_ing[[item]], useNA = "ifany")
  tab_rel_ing <- round(prop.table(tab_abs_ing) * 100, 2)

  resultado_ing <- cbind(f_abs = tab_abs_ing, f_rel = tab_rel_ing)
  return(resultado_ing)
})

# Criar a lista de tabelas para cada item
lista_esp <- lapply(esp, function(item) {
  tab_abs_esp <- table(dados_esp[[item]], useNA = "ifany")
  tab_rel_esp <- round(prop.table(tab_abs_esp) * 100, 2)

  resultado_esp <- cbind(f_abs = tab_abs_esp, f_rel = tab_rel_esp)
  return(resultado_esp)
})

# Criar a lista de tabelas para cada item
lista_oth <- lapply(oth, function(item) {
  tab_abs_oth <- table(lc_score[[item]], useNA = "ifany")
  tab_rel_oth <- round(prop.table(tab_abs_oth) * 100, 2)

  resultado_oth <- cbind(f_abs = tab_abs_oth, f_rel = tab_rel_oth)
  return(resultado_oth)
})

names(lista_ing) <- ing
names(lista_esp) <- esp
names(lista_oth) <- oth

lc_completo <- c(
  lista_ing,
  lista_esp,
  lista_oth
)

lc_completo

#-----------------------------
# monta lista contendo dados -
#-----------------------------

# # Plota a distribuição
# mirt::plot(lc_score$NU_NOTA_LC,
#            rowSums(LC_mat),
#            main = "Relação entre quantidade de acertos e proficiência",
#            xlab = "Proficiência",
#            ylab = "Quantidade de acertos",
#            # xlim = c(-4, 4),  # Ajustando o limite do eixo X para de -4 a 4
#            ylim = c(min(rowSums(LC_mat)), max(rowSums(LC_mat))),  # Ajuste automático do limite do eixo Y
#            cex = 0.1,
#            cex.axis = 1)
#
# abline(v = 628.7, lty = 2, lwd = 0.5, col = 'gray')
# abline(h = 33, lty = 2, lwd = 0.5, col = 'gray')

#-------------
# Meus dados -
#-------------

# Encontra meus dados
my_data <- filtered %>%
  dplyr::filter(NU_NOTA_LC == 628.7) %>%
  dplyr::filter(NO_MUNICIPIO_PROVA == "Votuporanga")

my_data <- filtered %>%
  dplyr::filter(NU_NOTA_LC == 322)
my_data <- my_data[1,]

my_score <- process_area(my_data$TX_RESPOSTAS_LC, my_data$TX_GABARITO_LC, keepna = TRUE)
colunas_validas <- colSums(is.na(my_score)) == 0
my_score <- my_score[, colunas_validas, drop = FALSE]

my_score <- my_score[, -(6:10), drop = FALSE]

my_test <- my_data$CO_PROVA_LC

my_prof <- (my_data$NU_NOTA_LC - 500) / 100

my_pars <- itens_2019_filtered %>%
  dplyr::filter(CO_PROVA == my_test) %>%
  dplyr::filter(TP_LINGUA == 0 | is.na(TP_LINGUA)) %>%
  arrange(CO_POSICAO)

#---------------------------------------------
# Resumo estatístico das notas de linguagens -
#---------------------------------------------

# describe(lc_score$NU_NOTA_LC)

#---------------------------------------------------------------
# Traceline de probabilidades de erro e acerto de cada questão -
#---------------------------------------------------------------

theta <- seq(-4, 4, by = .01)
ls_traceline <- list()

cci_3pl <- function(theta, a, b, c) {
  c + ((1 - c) / (1 + exp(-a * (theta - b))))
}

if (ncol(my_score) != nrow(my_pars)) {
  stop("Erro: o número de itens de my_score é diferente do número de itens de my_pars")
}

if (nrow(my_pars) != 45) {
  stop("Erro: inconsistências na quantidade de itens de my_pars")
}

# 2. Processamento vetorizado
# Usamos lapply para iterar sobre os índices dos itens
ls_traceline <- lapply(1:nrow(my_pars), function(i) {

  # Captura parâmetros do item atual
  ai <- my_pars$NU_PARAM_A[i]
  bi <- my_pars$NU_PARAM_B[i]
  ci <- my_pars$NU_PARAM_C[i]

  # A mágica acontece aqui: theta é um vetor, então p_vector será um vetor
  p_vector <- cci_3pl(theta, ai, bi, ci)

  # Retorna um data.frame para este item
  data.frame(
    p1 = p_vector,
    p0 = 1 - p_vector,
    theta = theta,
    item = i # Adicionado para identificar o item depois
  )
})

add_item_curve <- function(p, q, my_score, ls_traceline, my_prof, alpha = 0.4) {
  resp <- my_score[1, q]
  col  <- if (resp == 1) "blue" else "red"
  yvar <- if (resp == 1) "p1" else "p0"
  p <- p +
    geom_line(
      data = ls_traceline[[q]],
      aes(x = theta*100+500, y = .data[[yvar]]),
      alpha = alpha,
      color = col
    )
  return(p)
}

item <- 5
z <- ggplot()
z <- add_item_curve(z, item, my_score, ls_traceline, my_prof, alpha = 0.4)
z +
  geom_vline(xintercept = my_prof*100+500, linetype = "dashed", linewidth = 0.5) +
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

# Inicia um vetor vazio
prod_prob <- numeric(length(theta))

# Itera sobre os valores de theta, considerando erros e acertos
for (t in seq_len(length(theta))) {
  if (as.integer(ncol(my_score)) != as.integer(length(ls_traceline))) {
    stop(sprintf(
      "Erro: o número de itens de my_score é diferente do número de itens de ls_traceline",
      nrow(my_score)
    ))
  }
  probs_t <- c()
  for (q in 1:length(ls_traceline)) {
    if (my_score[1,q] == 1) {
      probs_t[q] <- ls_traceline[[q]]$p1[t]
    } else if (my_score[1,q] == 0) {
      probs_t[q] <- ls_traceline[[q]]$p0[t]
    }
  }
  prod_prob[t] <- prod(probs_t, na.rm = TRUE)
}

df_prod_prob <- data.frame(theta = theta, probabilidade = prod_prob)
df_prod_prob
df_prod_prob[which.max(df_prod_prob$probabilidade),]

ggplot() +
  geom_line(data = df_prod_prob, aes(x = theta*100+500, y = probabilidade), linewidth = 0.5) +
  scale_y_log10() +
  labs(
    x = "Proeficiência",
    y = "Produto das probabilidades",
    title = "Poeficiências dado um vetor de acertos"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# Theta e Probabilidade
Theta <- df_prod_prob$theta
L_theta <- df_prod_prob$probabilidade  # produto das probabilidades dos itens

# Função densidade a priori (normal padrão)
p_theta <- dnorm(Theta, mean = 0, sd = 1.2)

# Posterior (não normalizada)
posterior <- L_theta * p_theta

plot(Theta*100+500, posterior, type = "l",
     main = paste("Curva a Posteriori - Obs"),
     xlab = "Proficiência (Theta)",
     ylab = "Posterior (não normalizada)",
     lwd = 2, log = "y")

# EAP manual
theta_EAP <- sum(Theta * posterior) / sum(posterior)
theta_EAP*100 + 500  # transformação para escala ENEM

# Adicionando a estimativa EAP
abline(v = theta_EAP*100+500, lty = 2, lwd = 1)
text(theta_EAP*100+500, max(posterior)*0.9, labels = paste0("Proeficiência = ", round(theta_EAP*100+500,3)),
     pos = 4)

my_data$NU_INSCRICAO
which(filtered$NU_NOTA_LC == 628.7 & filtered$NU_INSCRICAO == my_data$NU_INSCRICAO)
lc_score[which(filtered$NU_NOTA_LC == 628.7 & filtered$NU_INSCRICAO == my_data$NU_INSCRICAO),]
