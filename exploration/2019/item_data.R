
library(dplyr)
library(ggplot2)
library(mirtCAT)

# 1. Carregar constantes e dados
load("data/constantes.rda")
load("data/itens_2019.rda")
codigo <- 512
area_atual <- "LC" # Ajuste conforme o nome na sua tabela de constantes

const_lc <- constantes %>% filter(area == area_atual)
k_area <- const_lc$k
d_area <- const_lc$d

# 2. Filtrar itens e selecionar a língua (Ex: Inglês)
# Filtramos para ter exatamente 45 itens antes de criar o modelo
itens_caderno <- itens_2019 %>%
  filter(CO_PROVA == codigo) %>%
  arrange(TP_LINGUA, CO_POSICAO) %>%
  filter(is.na(TP_LINGUA) | TP_LINGUA == 0 | TP_LINGUA == "") # Apenas comuns + Inglês

# 3. Preparar parâmetros para o mirt
itens_mirt <- itens_caderno[,c("NU_PARAM_A", "NU_PARAM_B", "NU_PARAM_C")]
names(itens_mirt) <- c('a1', 'd', 'g')
itens_mirt$a1 <- as.numeric(itens_mirt$a1)
itens_mirt$d <- as.numeric(itens_mirt$a1) * -as.numeric(itens_caderno$NU_PARAM_B)
itens_mirt$g <- as.numeric(itens_mirt$g)

# 4. Gerar objeto mirt com 45 itens
mod_45 <- mirtCAT::generate.mirt_object(itens_mirt, '3PL')

# 5. Estimar escore esperado (TCC)
Theta <- matrix(seq(-4, 4, length.out = 40))
# Agora o expected.test funcionará perfeitamente pois o mod só tem 45 itens
escore_esperado <- mirt::expected.test(mod_45, Theta)

# 6. Criar Dataframe com a Escala ENEM
df_trace <- data.frame(
  Theta = as.vector(Theta),
  Escala_ENEM = as.vector(Theta) * k_area + d_area,
  Escore = escore_esperado
)

# 7. Plotar
ggplot(df_trace, aes(x = Escala_ENEM, y = Escore)) +
  geom_line(color = "blue", size = 1.2) +
  # Linha guia para o b médio
  geom_vline(xintercept = mean(itens_caderno$NU_PARAM_B, na.rm = TRUE) * k_area + d_area,
             linetype = "dashed", color = "orange") +
  scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(
    title = paste("Trace do Teste (TCC) - Caderno:", codigo),
    subtitle = "Escala ENEM (Linguagens) | Filtro: Itens Comuns + Inglês",
    x = "Proficiência (Escala ENEM)",
    y = "Número Esperado de Acertos (0-45)"
  ) +
  theme_minimal()

# 1. Extrair os coeficientes como matriz
coefs <- mirt::coef(mod_45, simplify = TRUE)$items

# 2. Calcular b individual de cada item: b = -d / a1
# Precisamos garantir que estamos pegando apenas os vetores numéricos
b_itens <- -coefs[, "d"] / coefs[, "a1"]

# 3. Calcular a média ignorando possíveis problemas
b_extraido_mod <- mean(b_itens, na.rm = TRUE)

# 4. Converter para Escala ENEM
b_enem_mod <- (b_extraido_mod * k_area) + d_area

# 5. Exibir com cli
cli::cli_h1("Extração Direta do Objeto MOD")
cli::cli_alert_info("B médio (escala original) extraído do mod: {.val {round(b_extraido_mod, 3)}}")
cli::cli_alert_success("B médio (escala ENEM) extraído do mod: {.val {round(b_enem_mod, 1)}}")

# manualmente

library(dplyr)
library(ggplot2)

# 1. Obter probabilidades (tracos) e definir parâmetros
# Assume que write_probtrace retorna uma matriz/df com colunas P0, P1, P0, P1...
tracos <- write_probtrace(ano = 2019, co_prova = 512)
theta_range <- seq(-4, 4, length.out = nrow(tracos))

# 2. Filtrar itens e identificar línguas
itens_caderno <- itens_2019 %>%
  filter(CO_PROVA == 512) %>%
  mutate(NU_PARAM_B = as.numeric(NU_PARAM_B))

idx_comum <- which(is.na(itens_caderno$TP_LINGUA) | itens_caderno$TP_LINGUA == "" | itens_caderno$TP_LINGUA == " ")
idx_ing   <- which(itens_caderno$TP_LINGUA == 0)
idx_esp   <- which(itens_caderno$TP_LINGUA == 1)

# 3. Mapear colunas de acerto (P1) - Se o arquivo tem P0 e P1, P1 são as colunas pares
cols_comum <- idx_comum * 2
cols_ing   <- idx_ing * 2
cols_esp   <- idx_esp * 2

# 4. Cálculo Manual da TCC (Soma das probabilidades de acerto)
tcc_ing <- rowSums(tracos[, c(cols_comum, cols_ing)])
tcc_esp <- rowSums(tracos[, c(cols_comum, cols_esp)])

# 5. Aplicar Constantes da Escala ENEM
# Certifique-se que 'area_atual' está definida (ex: area_atual <- "LC")
const_area <- constantes %>% filter(area == area_atual)
k_area <- const_area$k
d_area <- const_area$d

# 6. Preparar DataFrame para o GGPLOT
df_plot <- data.frame(
  theta_enem = rep(theta_range * k_area + d_area, 2),
  acertos_esperados = c(tcc_ing, tcc_esp),
  lingua = rep(c("Inglês", "Espanhol"), each = length(theta_range))
)

# 7. Cálculo de b médio para as linhas verticais (Escala ENEM)
b_medio_ing_enem <- mean(itens_caderno$NU_PARAM_B[c(idx_comum, idx_ing)], na.rm = TRUE) * k_area + d_area
b_medio_esp_enem <- mean(itens_caderno$NU_PARAM_B[c(idx_comum, idx_esp)], na.rm = TRUE) * k_area + d_area

# 8. Gráfico Manual
ggplot(df_plot, aes(x = theta_enem, y = acertos_esperados, color = lingua)) +
  geom_line(size = 1.2) +
  # Linhas de dificuldade média
  geom_vline(xintercept = b_medio_ing_enem, linetype = "dashed", color = "#00BFC4") +
  geom_vline(xintercept = b_medio_esp_enem, linetype = "dashed", color = "#F8766D") +
  # Estética
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
  labs(
    title = paste("TCC (Trace do Teste) - Caderno:", 512),
    subtitle = "Eixo X transformado para Escala ENEM | Total 45 itens",
    x = "Proficiência (Escala ENEM)",
    y = "Número Esperado de Acertos",
    color = "Língua Estrangeira"
  ) +
  theme_minimal()

# 9. Diagnóstico Final via CLI
maior_ganho_idx <- which.max(diff(tcc_ing))
theta_inflexao_enem <- theta_range[maior_ganho_idx] * k_area + d_area

cli::cli_h1("Análise Caderno 512")
cli::cli_alert_info("Dificuldade Média ENEM (Inglês): {.val {round(b_medio_ing_enem, 1)}}")
cli::cli_alert_info("Dificuldade Média ENEM (Espanhol): {.val {round(b_medio_esp_enem, 1)}}")
cli::cli_alert_success("Ponto de maior discriminação (Inglês): {.val {round(theta_inflexao_enem, 1)}} na Escala ENEM")

#-------------
# Meus dados -
#-------------

library(data.table)
library(dplyr)

# Carregamento de dados
my_data <- fread("exploration/2019/MICRODADOS/my_data.csv")
load("data/itens_2019.rda")
load("data/constantes.rda") # Carrega o df 'constantes' com colunas area, k, d

my_score <- process_score(my_data$TX_RESPOSTAS_MT, my_data$TX_GABARITO_MT)
my_test <- my_data$CO_PROVA_MT

# Executa a função do JSON que criamos antes
write_probtrace(ano = 2019, co_prova = my_test)

# Parâmetros dos itens
my_pars <- itens_2019 %>%
  dplyr::filter(CO_PROVA == my_test) %>%
  dplyr::filter(TP_LINGUA == 0 | is.na(TP_LINGUA)) %>%
  arrange(CO_POSICAO)

#---------------------------------------------------------------
# Traceline de probabilidades -
#---------------------------------------------------------------

theta <- seq(-4, 4, by = .01)
cci_3pl <- function(theta, a, b, c) {
  c + ((1 - c) / (1 + exp(-a * (theta - b))))
}

if (ncol(my_score) != nrow(my_pars)) stop("Erro: discrepância itens score vs pars")

ls_traceline <- lapply(1:nrow(my_pars), function(i) {
  p_vector <- cci_3pl(theta, my_pars$NU_PARAM_A[i], my_pars$NU_PARAM_B[i], my_pars$NU_PARAM_C[i])
  data.frame(p1 = p_vector, p0 = 1 - p_vector, theta = theta)
})

#-----------------------------------------------------------------
# Verossimilhança (Likelihood) -
#-----------------------------------------------------------------

prod_prob <- numeric(length(theta))
for (t in seq_len(length(theta))) {
  probs_t <- sapply(1:length(ls_traceline), function(q) {
    if (my_score[1, q] == 1) ls_traceline[[q]]$p1[t] else ls_traceline[[q]]$p0[t]
  })
  prod_prob[t] <- prod(probs_t, na.rm = TRUE)
}

#-----------------------------------------------------------------
# Cálculo EAP e Transformação Final -
#-----------------------------------------------------------------

Theta <- theta
L_theta <- prod_prob
p_theta <- dnorm(Theta, mean = 0, sd = 1) # Priori
posterior <- L_theta * p_theta

# Estimativa EAP
theta_EAP <- sum(Theta * posterior) / sum(posterior)

# APLICAÇÃO DAS CONSTANTES DO ENVIRONMENT
# Definimos a área (LC no caso) para filtrar o multiplicador k e o deslocamento d
area_atual <- "MT"

# Cálculo da Nota ENEM seguindo a regra solicitada
nota_final <- round(
  theta_EAP * constantes[constantes$area == area_atual, 'k'] +
    constantes[constantes$area == area_atual, 'd'],
  1
)

# Resultados no console
cli::cli_h1("Calibragem de Nota ENEM")
cli::cli_alert_info("Theta EAP: {.val {round(theta_EAP, 4)}}")
cli::cli_alert_success("Nota ENEM Calculada: {.val {nota_final}}")
cli::cli_alert_info("Nota Original Microdados: {.val {my_data$NU_NOTA_MT}}")

# install.packages("ggplot2")
# 1. Recuperar as constantes de MT
k_mt <- constantes[constantes$area == "MT", "k"]
d_mt <- constantes[constantes$area == "MT", "d"]

# 2. Criar data frame com X na escala transformada
df_plot_transf <- data.frame(
  theta_escolar = (Theta * k_mt) + d_mt, # Transformação X = theta * k + d
  posterior_bruta = posterior
)

# 3. Transformar o ponto do EAP para a anotação
eap_escolar <- (theta_EAP * k_mt) + d_mt

# 4. Gráfico
library(ggplot2)

ggplot(df_plot_transf, aes(x = theta_escolar, y = posterior_bruta)) +
  geom_line(color = "#2c3e50", size = 1.2) +
  geom_area(fill = "#3498db", alpha = 0.3) +

  # Escala Logarítmica no Y (conforme pedido anteriormente)
  scale_y_log10() +

  # Linha vertical no EAP transformado
  geom_vline(xintercept = eap_escolar, linetype = "dashed", color = "red", size = 0.8) +

  # Anotação com a Nota final
  annotate("text", x = eap_escolar + 50, y = max(df_plot_transf$posterior_bruta),
           label = paste0("Nota ENEM: ", round(eap_escolar, 1)), color = "red", fontface = "bold") +

  labs(
    title = paste("Distribuição Posterior - Escala ENEM (MT)"),
    subtitle = paste("Caderno:", my_test),
    x = "Escala de Proficiência (0 - 1000)",
    y = "Posterior (Escala Log)"
  ) +
  theme_minimal() +
  # Ajusta os breaks para valores comuns na escala ENEM (ex: de 100 em 100)
  scale_x_continuous(breaks = seq(0, 1000, by = 100))
