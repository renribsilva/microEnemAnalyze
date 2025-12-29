#------------------------
# Importa os sample.csv -
#------------------------

sample <- fread("2019/CN/sample_CN.csv")
source("2019/process_area.R")
source("2019/priors.R")

#-----------------------------------
# Cria a matriz de acertos e erros -
#-----------------------------------

# processa cada área
CN_mat <- process_area(sample$TX_RESPOSTAS_CN, sample$TX_GABARITO_CN)

# combina todas as áreas na ordem LC, CH, CN, MT
colnames(CN_mat) <- paste0("q", 91:135)

#---------------------------------
# Cria o modelo mirt 3PL para LC -
#---------------------------------

# Ajudar o parâmetro g para o caso de 'chute' para 5 alternativas
# https://download.inep.gov.br/avaliacao_da_alfabetizacao/documentos_tecnicos/especificacoes_configuracoes_equalizacao_mirt.pdf

mod <- 'F = 1-45'

pars <- mirt(CN_mat, model = mod, itemtype = "3PL", pars = "values")
# pars <- mirt(CH_mat, model = mod, itemtype = "3PL", pars = "values")
# pars <- mirt(CN_mat, model = mod, itemtype = "3PL", pars = "values")
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
model_CN_pars <- mirt(CN_mat, model = mod, itemtype = "3PL", pars = pars)
saveRDS(model_CN_pars, "2019/CN/model_CN_pars.rds")
