#-----------------------------
# Importa table_filtered.csv -
#-----------------------------

filtered <- fread("2019/MICRODADOS/at_least_one_presence.csv")
source("2019/process_area.R")

n <- nrow(filtered)

#----------------------------------
# Importa os parâmetros dos itens -
#----------------------------------

itens_2019 <- fread(input='2019/MICRODADOS/microdados_enem_2019/DADOS/ITENS_PROVA_2019.csv')
itens_2019_filtered <- itens_2019 %>%
  dplyr::filter(CO_PROVA == 511 | CO_PROVA == 512 | CO_PROVA == 513 | CO_PROVA == 514 | CO_PROVA == 521 | CO_PROVA == 525 |
                  CO_PROVA == 507 | CO_PROVA == 508 | CO_PROVA == 509 | CO_PROVA == 510 | CO_PROVA == 520 | CO_PROVA == 524 |
                  CO_PROVA == 503 | CO_PROVA == 504 | CO_PROVA == 505 | CO_PROVA == 506 | CO_PROVA == 519 | CO_PROVA == 523 |
                  CO_PROVA == 515 | CO_PROVA == 516 | CO_PROVA == 517 | CO_PROVA == 518 | CO_PROVA == 522 | CO_PROVA == 526)

#-------------------
# Ciências humanas -
#-------------------

# Ciências humanas
CH_provas <- list(
  azul    = "507",
  amarela = "508",
  branca  = "509",
  rosa    = "510",
  laranja = "520",
  verde   = "524"
)

seq_code_CH <- list(
  azul = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CH_provas$azul)) %>%
    dplyr::arrange(CO_POSICAO),
  
  branca = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CH_provas$branca)) %>%
    dplyr::arrange(CO_POSICAO),
  
  rosa = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CH_provas$rosa)) %>%
    dplyr::arrange(CO_POSICAO),
  
  amarela = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CH_provas$amarela)) %>%
    dplyr::arrange(CO_POSICAO),
  
  laranja = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CH_provas$laranja)) %>%
    dplyr::arrange(CO_POSICAO),
  
  verde = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CH_provas$verde)) %>%
    dplyr::arrange(CO_POSICAO)
)

gab_amarela_CH <- filtered[which(filtered$CO_PROVA_CH == 508)[1],]$TX_GABARITO_CH

ch_gabaritos <- filtered$TX_GABARITO_CH
ch_respostas  <- filtered$TX_RESPOSTAS_CH
ch_score_item <- matrix(NA_character_, nrow = n, ncol = 45)
ch_score_nu <- matrix(NA_character_, nrow = n, ncol = 1)

for (i in seq_len(n)) {
  
  prova_origem <- as.character(filtered$CO_PROVA_CH[i])
  
  if (prova_origem %in% c(CH_provas$azul, CH_provas$branca, CH_provas$rosa, CH_provas$laranja, CH_provas$verde)) {
    
    seq_origem <- switch (
      prova_origem,
      "507" = seq_code_CH$azul,
      "509" = seq_code_CH$branca,
      "510" = seq_code_CH$rosa,
      "520" = seq_code_CH$laranja,
      "524" = seq_code_CH$verde,
      NULL # Caso a prova não esteja mapeada
    )
    
    if (is.null(seq_origem)) stop(sprintf("Erro: prova_origem %d não está mapeada", prova_origem))
    
    resp_orig_string <- filtered$TX_RESPOSTAS_CH[i]
    gab_orig_string <- filtered$TX_GABARITO_CH[i]
    resp_orig_vetor  <- strsplit(resp_orig_string, "")[[1]]
    
    # Validação de comprimento
    if (nchar(resp_orig_string) != 45) stop(sprintf("Erro: Tamanho da string TX_RESPOSTAS é %d na linha %d", nchar(resp_orig_string), i))
    if (nchar(gab_orig_string) != 45) stop(sprintf("Erro: Tamanho da string TX_GABARITO é %d na linha %d", nchar(gab_orig_string), i))
    
    # Vetorização com match
    indices <- match(seq_code_CH$amarela$CO_ITEM, seq_origem$CO_ITEM)
    
    if (length(indices) != 45) stop(sprintf("Erro: Tamanho de índices é %d na linha %d", nchar(resp_orig_string), i))
    if (any(is.na(indices))) stop(paste("Índice não encontrado não encontrado na prova", prova_origem, "linha", i))
    
    novo_txt_respostas <- as.character()
    novo_txt_respostas <- paste0(resp_orig_vetor[indices], collapse = "")
    
    if (nchar(novo_txt_respostas) != 45) stop(sprintf("Erro: Novo TX_RESPOSTAS tem comprimento %d na lina %d", nchar(resp_orig_string), i))
    if (nchar(gab_amarela_CH) != 45) stop(sprintf("Erro: TX_GABARITO da prova amarela tem comprimento %d na lina %d", nchar(resp_orig_string), i))
    
    # Validação de Score (Soma de acertos deve ser idêntica)
    score_novo <- process_area(novo_txt_respostas, gab_amarela_CH, keepna = TRUE)
    score_orig <- process_area(resp_orig_string, gab_orig_string, keepna = TRUE)
    
    if (sum(score_novo, na.rm = TRUE) == sum(score_orig, na.rm = TRUE)) {
      ch_gabaritos[i] <- gab_amarela_CH
      ch_respostas[i]  <- novo_txt_respostas
      ch_score_item[i, ] <- score_novo
      ch_score_nu[i, ] <- sum(score_novo, na.rm = TRUE)
    } else {
      stop(sprintf("Erro integridade na linha %d: Original %d != Novo %d", i, score_orig, score_novo))
    }
  } else if (prova_origem == CH_provas$amarela) {
    score_novo <- process_area(filtered$TX_RESPOSTAS_CH[i], filtered$TX_GABARITO_CH[i], keepna = TRUE)
    ch_score_item[i, ] <- score_novo
    ch_score_nu[i, ] <- sum(score_novo, na.rm = TRUE)
  }
  
  if (i %% 1000 == 0) cat("Processado:", i, "/", n, "\n")
}

ch_score <- data.frame()
filtered_essential <- filtered[, c(
  "NU_INSCRICAO", 
  "TP_PRESENCA_CH", 
  "CO_PROVA_CH",
  "NU_NOTA_CH", 
  "TP_LINGUA", 
  "TX_RESPOSTAS_CH",
  "TX_GABARITO_CH")]

if (unique(nchar(ch_gabaritos)) == 45 &
    unique(nchar(ch_respostas)) == 45 &
    unique(ncol(ch_score_item)) == 45) {
  colnames(ch_score_item) <- paste0("Q_", 46:90)
  storage.mode(ch_score_item) <- "integer"
  ch_score <- as.data.frame(cbind(
    filtered_essential,
    TX_RESPOSTAS_CH_NORM = ch_respostas,
    TX_GABARITO_CH_NORM = ch_gabaritos,
    NU_SCORE_CH = as.integer(ch_score_nu[, 1]),
    ch_score_item))
} else {
  stop("Merda") 
}

write.csv(ch_score, file = paste0("2019/MICRODADOS/score_ch.csv"), row.names = FALSE)

rm(list = ls(all.names = TRUE))
gc()
