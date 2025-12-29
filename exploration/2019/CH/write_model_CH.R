#------------------------
# Importa os sample.csv -
#------------------------

sample <- fread("2019/CH/sample_CH.csv")
source("2019/priors.R")
source("2019/process_area.R")

#-----------------------------------
# Cria a matriz de acertos e erros -
#-----------------------------------

# processa cada área
CH_mat <- process_area(sample$TX_RESPOSTAS_CH, sample$TX_GABARITO_CH)

# combina todas as áreas na ordem LC, CH, CN, MT
colnames(CH_mat) <- paste0("q", 46:90)

#---------------------------------
# Cria o modelo mirt 3PL para LC -
#---------------------------------

# Ajudar o parâmetro g para o caso de 'chute' para 5 alternativas
# https://download.inep.gov.br/avaliacao_da_alfabetizacao/documentos_tecnicos/especificacoes_configuracoes_equalizacao_mirt.pdf

mod <- 'F = 1-45'

pars <- mirt(CH_mat, model = mod, itemtype = "3PL", pars = "values")
idx_a1 <- which(pars$name == "a1")
idx_g <- which(pars$name == "g")
idx_u <- which(pars$name == "u")
idx_d <- which(pars$name == "d")

# prior logit-normal
pars$prior.type[idx_a1] <- priors$type[1]
pars$prior_1[idx_a1]    <- priors$p1[1]
pars$prior_2[idx_a1]    <- priors$p2[1]

pars$prior.type[idx_g] <- priors$type[2]
pars$prior_1[idx_g]    <- priors$p1[2]
pars$prior_2[idx_g]    <- priors$p2[2]

pars$prior.type[idx_d] <- priors$type[3]
pars$prior_1[idx_d]   <- priors$p1[3]
pars$prior_2[idx_d]   <- priors$p2[3]

pars$prior.type[idx_u] <- priors$type[4]
pars$prior_1[idx_u]   <- priors$p1[4]
pars$prior_2[idx_u]   <- priors$p2[4]

# modelo com calibração
model_CH_pars <- mirt(CH_mat, model = mod, itemtype = "3PL", pars = pars)
saveRDS(model_CH_pars, "2019/CH/model_CH_pars.rds")
