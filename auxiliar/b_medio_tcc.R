# ----------------------------------------------------------------
# Geração do modelo dos carderno com base nos parâmetros do ENEM -
# ----------------------------------------------------------------

library(dplyr)
library(mirtCAT)
library(ggplot2)

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

# 1. Extrair os coeficientes como matriz
coefs <- mirt::coef(mod_45, simplify = TRUE)$items

# 2. Calcular b individual de cada item: b = -d / a1
b_itens <- -coefs[, "d"] / coefs[, "a1"]

# 3. Calcular a média ignorando possíveis problemas
b_extraido_mod <- mean(b_itens, na.rm = TRUE)

# 4. Converter para Escala ENEM
b_enem_mod <- (b_extraido_mod * k_area) + d_area

# 5. Exibir com cli
cli::cli_h1("Extração Direta do Objeto MOD")
cli::cli_alert_info("B médio (escala original) extraído do mod: {.val {round(b_extraido_mod, 3)}}")
cli::cli_alert_success("B médio (escala ENEM) extraído do mod: {.val {round(b_enem_mod, 1)}}")

# -----------------------------
# Geração 'manual' do b médio -
# -----------------------------

# 1. Obter probabilidades (tracos) e definir parâmetros
# Assume que write_probtrace retorna uma matriz/df com colunas P0, P1, P0, P1...
tracos <- write_probtrace(ano = 2019, co_prova = 512)
theta_range <- seq(-4, 4, length.out = nrow(tracos))

# 2. Filtrar itens e identificar línguas
itens_caderno <- itens_2019 %>%
  filter(CO_PROVA == 512) %>%
  arrange(TP_LINGUA, CO_POSICAO) %>%
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
