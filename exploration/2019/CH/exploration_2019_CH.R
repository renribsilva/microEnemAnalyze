# -------------------------------------------------------------------------------------------
# Visualizando a Curva Característica do Teste (CCT) e a Curva de Informação do Teste (CIT) -
# -------------------------------------------------------------------------------------------

model_CH_pars <- readRDS("2019/CH/model_CH_pars.rds")

# Sem calibração
# model_LC@Fit
# mirt::plot(model_LC)
# mirt::plot(model_LC, type = "info")
# mirt::plot(model_LC, type = "infoSE")
# mirt::plot(model_LC, type = "trace")
# mirt::plot(model_LC, type = "infotrace")
# mirt::plot(model_LC, type = "trace", facet_item = F)
# mirt::plot(model_LC, type = "infotrace", facet_item = F)

# model_LC_SE@Fit
# mirt::plot(model_LC_SE, MI=200)
# mirt::plot(model_LC_SE, type = "info", MI=200)
# mirt::plot(model_LC_SE, type = "infoSE", MI=200)
# mirt::plot(model_LC_SE, type = "trace", MI=200)
# mirt::plot(model_LC_SE, type = "infotrace", MI=200)
# mirt::plot(model_LC_SE, type = "trace", facet_item = F, MI=200)
# mirt::plot(model_LC_SE, type = "infotrace", facet_item = F, MI=200)

# Com calibração
# model_LC_pars@Fit
# mirt::plot(model_LC_pars)
# mirt::plot(model_LC_pars, type = "info")
# mirt::plot(model_LC_pars, type = "infoSE")
# mirt::plot(model_LC_pars, type = "trace")
mirt::plot(model_CH_pars, type = "infotrace")
# mirt::plot(model_LC_pars, type = "trace", facet_item = F)
# mirt::plot(model_LC_pars, type = "infotrace", facet_item = F)

# model_LC_SE_pars@Fit
# mirt::plot(model_LC_SE_pars, MI=200)
# mirt::plot(model_LC_SE_pars, type = "info", MI=200)
# mirt::plot(model_LC_SE_pars, type = "infoSE", MI=200)
# mirt::plot(model_LC_SE_pars, type = "trace", MI=200)
# mirt::plot(model_LC_SE_pars, type = "infotrace", MI=200)
# mirt::plot(model_LC_SE_pars, type = "trace", facet_item = F, MI=200)
# mirt::plot(model_LC_SE_pars, type = "infotrace", facet_item = F, MI=200)

#---------------------------
# Escores e proeficiências -
#---------------------------

modelo_coef <- mirt::coef(model_CH_pars, simplify = TRUE, IRTpars = TRUE)
invalid_items <- which(abs(modelo_coef$items[, "b"]) > 6)
modelo_coef$items[invalid_items, ] <- NA

# número de itens do modelo
n_itens <- extract.mirt(model_CH_pars, "nitems")
w <- rep(1, n_itens)
w[invalid_items] <- 0

# estima proficiências ignorando esses itens
prof_total <- fscores(model_CH_pars, method = "EAP", item_weights = w, na.rm = FALSE)
prof_vector <- prof_total[, 1]
CH_mat <- model_CH_pars@Data$data
escore_vector <- as.vector(rowSums(CH_mat))

# Encontra meus dados
obs <- 132699
min <- as.numeric(which(rowSums(CH_mat) == 0)[1])
max <- as.numeric(which(rowSums(CH_mat) == 43)[1])
var <- 1

#--------------------------------------------
# Informações sobre a dificuldade dos itens -
#--------------------------------------------

wrightMap(prof_vector, modelo_coef$items[, 2])

#--------------------------------------------------------
# Relação entre quantificação de acertos e proficiência -
#--------------------------------------------------------

par(mar = c(5, 5, 2, 2))
mirt::plot(prof_vector*100+500, 
           escore_vector, 
           main = "Relação entre quantidade de acertos e proficiência", 
           xlab = "Proficiência", 
           ylab = "Escore total bruto",
           # xlim = c(-4, 4),  # Ajustando o limite do eixo X para de -4 a 4
           ylim = c(min(escore_vector), max(escore_vector)),  # Ajuste automático do limite do eixo Y
           cex = 0.1,
           cex.axis = 1)

abline(v = prof_vector[obs]*100+500, lty = 2, lwd = 0.5, col = 'gray')
abline(h = escore_vector[obs], lty = 2, lwd = 0.5, col = 'gray')

cor(prof_vector, escore_vector, method = "pearson")

#----------------------------------------
# Probabilidade e proeficiencia do item -
#----------------------------------------

# se um item tem uma dificuldade de θ = 2, isso significa que um
# respondente precisa de uma proficiência de 2 para ter 50% de chance 
# de acertar esse item

itemplot(model_CH_pars, var)
modelo_coef$items[,2][var]

#---------------------------------------------------------
# Encontrando preficiência e probabilidade para um theta -
#---------------------------------------------------------

# single item probabilty tracelines for Item 1
extr_var <- extract.item(model_CH_pars, var)
Theta <- matrix(seq(-6,6, by = .01))
traceline <- probtrace(extr_var, Theta)
# Extraindo a coluna de probabilidade de acerto (por exemplo, P.0)
prob_acerto <- traceline[, 2]  # Probabilidade de acerto
# Selecionando um valor de Theta específico (por exemplo, Theta = 1)
prof_especifico <- prof_vector[obs]
prob_especifica <- prob_acerto[which.min(abs(Theta - prof_especifico))]
# Plotando a curva de probabilidade para o Item 1
data <- data.frame(Theta = Theta, Probabilidade = prob_acerto)
plot(Theta, prob_acerto, type = "l", 
     main = "Curva de Probabilidade de Acerto para o Item 1",
     xlab = "Proficiência", ylab = "Probabilidade de Acerto", lwd = 2)

# Adicionando um ponto sobre a curva, em vez de segmentos
points(prof_especifico, prob_especifica, 
       pch = 19, cex = 1.5)  # pch=19 é um ponto sólido, cex ajusta o tamanho

abline(v = prof_especifico, lty = 2, lwd = 1)  # Linha vertical no ponto

rotulo <- paste0("(", format(round(prof_especifico, 3), nsmall = 3), ", ", format(round(prob_especifica, 3), nsmall = 3), ")")

# Adicionando o texto no gráfico
text(prof_especifico, prob_especifica, labels = rotulo, 
     pos = 4, col = "black", cex = 0.8)

#-------------------------------------------------------------------------
# Curva de distribuição dos produto das probabilidades por proeficiência -
#-------------------------------------------------------------------------

df_traceline <- list()
Theta <- matrix(seq(-6, 6, by = 0.01))

for (i in 1:ncol(CH_mat)) {
  extr.i <- extract.item(model_CH_pars, i)
  traceline <- probtrace(extr.i, Theta)  # probabilidade para todos os níveis de Theta
  # Seleciona a coluna correta com base no LC_mat
  if (CH_mat[obs, i] == 1) {
    prob_item <- traceline[, 2]  # P.1
  } else if (CH_mat[obs, i] == 0) {
    prob_item <- traceline[, 1]  # P.0
  }
  
  # Coloca o data.frame dentro da lista corretamente
  df_traceline[[i]] <- data.frame(Probabilidade = prob_item, Theta = Theta)
}

# Número de Theta
n_theta <- length(df_traceline[[1]]$Theta)
n_itens <- length(df_traceline)
# Inicializa o vetor para o produto das probabilidades
prod_prob <- numeric(n_theta)

# Loop sobre cada nível de Theta
for (j in 1:n_theta) {
  # Extrai as probabilidades do j-ésimo Theta para todos os itens
  probs_j <- sapply(df_traceline, function(df) df$Probabilidade[j])
  # Calcula o produto das probabilidades
  prod_prob[j] <- prod(probs_j)
}

# Cria o data.frame final
prod_itens <- data.frame(Theta = df_traceline[[1]]$Theta, Probabilidade = prod_prob)
prod_itens
# Visualizando as primeiras linhas
head(prod_itens)

# Plotando o produto das probabilidades
plot(prod_itens$Theta*100+500, prod_itens$Probabilidade, type = "l",
     main = "Distribuição dos produtos de probabilidades por proeficiência",
     xlab = "Proficiência (Theta)", ylab = "Produto das Probabilidades",
     lwd = 2, log = "y")  # eixo Y logarítmico para visualizar valores muito pequenos

#------------------------------------------------
# Calcula a nota propriamente dita (método EAP) -
#------------------------------------------------

# Theta e Probabilidade
Theta <- prod_itens$Theta
L_theta <- prod_itens$Probabilidade  # produto das probabilidades dos itens

# Função densidade a priori (normal padrão)
p_theta <- dnorm(Theta, mean = 0, sd = 1)

# Posterior (não normalizada)
posterior <- L_theta * p_theta

plot(Theta*100+500, posterior, type = "l",
     main = paste("Curva a Posteriori - Obs", obs),
     xlab = "Proficiência (Theta)",
     ylab = "Posterior (não normalizada)",
     lwd = 2)

# EAP manual
theta_EAP <- sum(Theta * posterior) / sum(posterior)
theta_EAP*100 + 500  # transformação para escala ENEM

# Adicionando a estimativa EAP
abline(v = theta_EAP*100+500, lty = 2, lwd = 1)
text(theta_EAP*100+500, max(posterior)*0.9, labels = paste0("Proeficiência = ", round(theta_EAP*100+500,3)),
     pos = 4)

# Desvio padrão
theta_var <- sum((Theta - theta_EAP)^2 * posterior) / sum(posterior)
theta_se <- sqrt(theta_var)
theta_se*100

#---------------------------------------------------
# Nota propriamente dita usado fscores diretamente -
#---------------------------------------------------

# Minha nota – 628,7
# Nota mínima – 322,0
# Nota máxima – 801,7
# Nota média geral – 520,9

prof_vector[obs]*100+500
escore_vector[obs]

prof_vector[min]*100+500
escore_vector[min]

prof_vector[max]*100+500
escore_vector[max]

#------------------------------------------------------------------------
# Contribuição que cada item tem na nota final, comparado ao seu oposto -
#------------------------------------------------------------------------

resp <- as.vector(LC_mat[obs,])

theta_full <- fscores(
  model_LC_pars,
  response.pattern = resp,  # <- vetor, não lista
  method = "EAP",
  item_weights = w
)

contrib <- sapply(seq_along(resp), function(i) {
  
  resp_alt <- resp
  
  # inverte a resposta do item i (0 ↔ 1)
  resp_alt[i] <- 1 - resp_alt[i]
  
  theta_alt <- fscores(
    model_LC_pars,
    response.pattern = resp_alt,
    method = "EAP",
    item_weights = w
  )
  
  (theta_full[1]*100+500)-(theta_alt[1]*100+500)
})

contrib

