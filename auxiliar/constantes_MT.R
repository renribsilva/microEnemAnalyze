#-------------
# Meus dados -
#-------------
library(data.table)
library(dplyr)

# Carregamento e Filtro
my_data <- fread("exploration/2019/MICRODADOS/at_least_one_presence.csv")

# data_full <- my_data %>%
#   dplyr::filter(NU_NOTA_LC > 0 & NU_NOTA_CH > 0 & NU_NOTA_CN > 0 & NU_NOTA_MT > 0)
data_filtered <- my_data %>%
  dplyr::filter(!is.na(NU_NOTA_MT) & NU_NOTA_MT > 0)

data_max <- data_filtered %>%
  dplyr::filter(NU_NOTA_MT == max(NU_NOTA_MT)) %>%
  head(1) # Caso haja empate, pega o primeiro

write.csv(data_max, file = "exploration/2019/MICRODADOS/data_max_MT.csv")
write.csv(data_min, file = "exploration/2019/MICRODADOS/data_min_MT.csv")

data_min <- data_filtered %>%
  dplyr::filter(NU_NOTA_MT == min(NU_NOTA_MT)) %>%
  head(1) # Caso haja empate, pega o primeiro

# Sorteia 30 índices para a amostra de cálculo
data <- rbind(data_max, data_min)

load("data/itens_2019.rda")
load("data/constantes.rda")

area_atual <- "MT"

score <- matrix(nrow = nrow(data), ncol = 45)
co_prova <- character(nrow(data)) # Inicializado como vetor de caracteres

for (i in 1:nrow(data)) {
  score[i,] <- process_score(data[i,]$TX_RESPOSTAS_MT, data[i,]$TX_GABARITO_MT)
  co_prova[i] <- data[i,]$CO_PROVA_MT
}

#---------------------------------------------------------------
# Traceline de probabilidades -
#---------------------------------------------------------------
theta <- seq(-4, 4, by = .01)
cci_3pl <- function(theta, a, b, c) {
  c + ((1 - c) / (1 + exp(-a * (theta - b))))
}

ls_traceline <- list()

for (k in 1:nrow(data)) {
  pars <- itens_2019 %>%
    dplyr::filter(CO_PROVA == co_prova[k]) %>%
    arrange(CO_POSICAO)

  # ALTERAÇÃO MÍNIMA: O índice deve ser 'k' para não sobrescrever
  ls_traceline[[k]] <- lapply(1:nrow(pars), function(idx) {
    p_vector <- cci_3pl(theta, pars$NU_PARAM_A[idx], pars$NU_PARAM_B[idx], pars$NU_PARAM_C[idx])
    data.frame(p1 = p_vector, p0 = 1 - p_vector, theta = theta)
  })
}

#-----------------------------------------------------------------
# Verossimilhança (Likelihood) -
#-----------------------------------------------------------------
prod_prob <- list()

for (m in 1:nrow(data)) {
  # ALTERAÇÃO MÍNIMA: Precisamos de um vetor para guardar os resultados do grid
  v_likelihood <- numeric(length(theta))

  for (t in seq_len(length(theta))) {
    probs_t <- sapply(1:length(ls_traceline[[m]]), function(q) {
      if (score[m, q] == 1) ls_traceline[[m]][[q]]$p1[t] else ls_traceline[[m]][[q]]$p0[t]
    })
    v_likelihood[t] <- prod(probs_t, na.rm = TRUE)
  }
  prod_prob[[m]] <- v_likelihood
}

#-----------------------------------------------------------------
# Cálculo EAP e Transformação Final -
#-----------------------------------------------------------------
Theta <- theta
p_theta <- dnorm(Theta, mean = 0, sd = 1)

theta_EAP <- list()

for (j in 1:nrow(data)) {
  L_theta <- prod_prob[[j]]
  posterior <- L_theta * p_theta
  # Estimativa EAP
  theta_EAP[[j]] <- sum(Theta * posterior) / sum(posterior)
}

# Matriz de resultados (2 colunas bastam: Theta e Nota)
mat_res <- matrix(nrow = nrow(data), ncol = 2)

for (r in 1:nrow(data)) {
  mat_res[r,1] <- theta_EAP[[r]]
  mat_res[r,2] <- data[r,]$NU_NOTA_MT
}

modelo <- lm(mat_res[,2] ~ mat_res[,1])

d_calculado <- as.numeric(coef(modelo)[1]) # Intercepto
k_calculado <- as.numeric(coef(modelo)[2]) # Inclinação (Slope)

cat("Novos valores via Regressão:\n")
cat("k:", k_calculado, " d:", d_calculado, "\n")

# Exibição dos resultados
cat("--- Constantes Calculadas para", area_atual, "---\n")
cat("k:", k_calculado, "\n")
cat("d:", d_calculado, "\n")
cat("--------------------------------------------\n")

#-----------------------------------------------------------------
# 1. Configuração de Parâmetros e Constantes
#-----------------------------------------------------------------

library(data.table)
library(dplyr)
library(cli)

area_atual  <- "MT" # Mude aqui para "MT", "CH", "CN" ou "LC"

# Carregar arquivo de constantes
if (file.exists("data/constantes.rda")) {
  load("data/constantes.rda")
}

#-----------------------------------------------------------------
# 2. Função de Cálculo EAP (Modularizada)
#-----------------------------------------------------------------
calcular_nota_enem <- function(caminho_csv, constantes_df, area = "MT") {

  # Carregamento dos dados do aluno
  dados_aluno <- fread(caminho_csv)
  load("data/itens_2019.rda")

  # NOMES DINÂMICOS DE COLUNAS
  col_prova <- paste0("CO_PROVA_", area)
  col_nota  <- paste0("NU_NOTA_", area)
  col_resp  <- paste0("TX_RESPOSTAS_", area)
  col_gaba  <- paste0("TX_GABARITO_", area)

  # Preparação de Score e Prova
  prova_id <- dados_aluno[[col_prova]]
  score    <- process_score(dados_aluno[[col_resp]], dados_aluno[[col_gaba]])

  # Parâmetros dos Itens
  pars <- itens_2019 %>%
    dplyr::filter(CO_PROVA == prova_id)

  # AJUSTE PARA LINGUAGENS (LC): Filtra Inglês (0) ou Espanhol (1)
  if (area == "LC") {
    pars <- pars %>% dplyr::filter(TP_LINGUA == dados_aluno$TP_LINGUA | is.na(TP_LINGUA))
  } else {
    pars <- pars %>% dplyr::filter(TP_LINGUA == 0 | is.na(TP_LINGUA))
  }

  pars <- pars %>% arrange(CO_POSICAO)

  if (ncol(score) != nrow(pars)) stop(paste("Erro de dimensão no arquivo:", caminho_csv))

  # Traceline e Likelihood
  theta   <- seq(-4, 4, by = .01)
  cci_3pl <- function(theta, a, b, c) { c + ((1 - c) / (1 + exp(-a * (theta - b)))) }

  v_likelihood <- numeric(length(theta))
  ls_probs <- lapply(1:nrow(pars), function(i) {
    p <- cci_3pl(theta, pars$NU_PARAM_A[i], pars$NU_PARAM_B[i], pars$NU_PARAM_C[i])
    list(p1 = p, p0 = 1 - p)
  })

  for (t in seq_len(length(theta))) {
    probs_t <- sapply(1:length(ls_probs), function(q) {
      if (score[1, q] == 1) ls_probs[[q]]$p1[t] else ls_probs[[q]]$p0[t]
    })
    v_likelihood[t] <- prod(probs_t, na.rm = TRUE)
  }

  # EAP (Expected A Posteriori)
  p_theta   <- dnorm(theta, mean = 0, sd = 1)
  posterior <- v_likelihood * p_theta
  theta_eap <- sum(theta * posterior) / sum(posterior)

  # Transformação Linear
  k_val <- constantes_df[constantes_df$area == area, 'k']
  d_val <- constantes_df[constantes_df$area == area, 'd']

  nota_calc <- round(theta_eap * k_val + d_val, 1)

  # Retorno de resultados
  return(list(
    theta = theta_eap,
    nota_calc = nota_calc,
    nota_orig = dados_aluno[[col_nota]],
    nome = basename(caminho_csv)
  ))
}

#-----------------------------------------------------------------
# 3. Execução dos Testes (Nomes de arquivos dinâmicos)
#-----------------------------------------------------------------
arquivos_teste <- c(
  paste0("exploration/2019/MICRODADOS/data_max_", area_atual, ".csv"),
  paste0("exploration/2019/MICRODADOS/data_min_", area_atual, ".csv"),
  "exploration/2019/MICRODADOS/my_data.csv"
)

cli::cli_h1("Calibragem de Nota ENEM: {area_atual}")

for (arq in arquivos_teste) {
  if (file.exists(arq)) {
    res <- calcular_nota_enem(arq, constantes, area_atual)

    cli::cli_h2("Arquivo: {res$nome}")
    cli::cli_alert_info("Theta EAP: {.val {round(res$theta, 4)}}")
    cli::cli_alert_success("Nota Calculada: {.val {res$nota_calc}}")
    cli::cli_alert_warning("Nota Original: {.val {res$nota_orig}}")
    cli::cli_alert("Diferença: {.val {round(res$nota_calc - res$nota_orig, 2)}}")
  }
}

