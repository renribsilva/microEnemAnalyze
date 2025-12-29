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

#-------------
# Matemática -
#-------------

# Matemática
MT_provas <- list(
  azul    = "515",
  amarela = "516",
  rosa    = "517",
  cinza   = "518",
  laranja = "522",
  verde   = "526"
)

seq_code_MT <- list(
  azul = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(MT_provas$azul)) %>%
    dplyr::arrange(CO_POSICAO),
  
  rosa = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(MT_provas$rosa)) %>%
    dplyr::arrange(CO_POSICAO),
  
  cinza = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(MT_provas$cinza)) %>%
    dplyr::arrange(CO_POSICAO),
  
  amarela = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(MT_provas$amarela)) %>%
    dplyr::arrange(CO_POSICAO),
  
  laranja = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(MT_provas$laranja)) %>%
    dplyr::arrange(CO_POSICAO),
  
  verde = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(MT_provas$verde)) %>%
    dplyr::arrange(CO_POSICAO)
)

gab_amarela_MT <- filtered[which(filtered$CO_PROVA_MT == 516)[1],]$TX_GABARITO_MT

mt_gabaritos <- filtered$TX_GABARITO_MT
mt_respostas  <- filtered$TX_RESPOSTAS_MT
mt_score_item <- matrix(NA_character_, nrow = n, ncol = 45)
mt_score_nu <- matrix(NA_character_, nrow = n, ncol = 1)

for (i in seq_len(n)) {
  
  prova_origem <- as.character(filtered$CO_PROVA_MT[i])
  
  if (prova_origem %in% c(MT_provas$azul, MT_provas$rosa, MT_provas$cinza, MT_provas$laranja, MT_provas$verde)) {
    
    seq_origem <- switch (
      prova_origem,
      "515" = seq_code_MT$azul,
      "517" = seq_code_MT$rosa,
      "518" = seq_code_MT$cinza,
      "522" = seq_code_MT$laranja,
      "526" = seq_code_MT$verde,
      NULL # Caso a prova não esteja mapeada
    )
    
    if (is.null(seq_origem)) stop(sprintf("Erro: prova_origem %d não está mapeada", prova_origem))
    
    resp_orig_string <- filtered$TX_RESPOSTAS_MT[i]
    gab_orig_string <- filtered$TX_GABARITO_MT[i]
    resp_orig_vetor  <- strsplit(resp_orig_string, "")[[1]]
    
    # Validação de comprimento
    if (nchar(resp_orig_string) != 45) stop(sprintf("Erro: Tamanho da string TX_RESPOSTAS é %d na linha %d", nchar(resp_orig_string), i))
    if (nchar(gab_orig_string) != 45) stop(sprintf("Erro: Tamanho da string TX_GABARITO é %d na linha %d", nchar(gab_orig_string), i))
    
    # Vetorização com match
    indices <- match(seq_code_MT$amarela$CO_ITEM, seq_origem$CO_ITEM)
    
    if (length(indices) != 45) stop(sprintf("Erro: Tamanho de índices é %d na linha %d", nchar(resp_orig_string), i))
    if (any(is.na(indices))) stop(paste("Índice não encontrado não encontrado na prova", prova_origem, "linha", i))
    
    novo_txt_respostas <- as.character()
    novo_txt_respostas <- paste0(resp_orig_vetor[indices], collapse = "")
    
    if (nchar(novo_txt_respostas) != 45) stop(sprintf("Erro: Novo TX_RESPOSTAS tem comprimento %d na lina %d", nchar(resp_orig_string), i))
    if (nchar(gab_amarela_MT) != 45) stop(sprintf("Erro: TX_GABARITO da prova amarela tem comprimento %d na lina %d", nchar(resp_orig_string), i))
    
    # Validação de Score (Soma de acertos deve ser idêntica)
    score_novo <- process_area(novo_txt_respostas, gab_amarela_MT, keepna = TRUE)
    score_orig <- process_area(resp_orig_string, gab_orig_string, keepna = TRUE)
    
    if (sum(score_novo, na.rm = TRUE) == sum(score_orig, na.rm = TRUE)) {
      mt_gabaritos[i] <- gab_amarela_MT
      mt_respostas[i]  <- novo_txt_respostas
      mt_score_item[i, ] <- score_novo
      mt_score_nu[i, ] <- sum(score_novo, na.rm = TRUE)
    } else {
      stop(sprintf("Erro integridade na linha %d: Original %d != Novo %d", i, score_orig, score_novo))
    }
  } else if (prova_origem == MT_provas$amarela) {
    score_novo <- process_area(filtered$TX_RESPOSTAS_MT[i], filtered$TX_GABARITO_MT[i], keepna = TRUE)
    mt_score_item[i, ] <- score_novo
    mt_score_nu[i, ] <- sum(score_novo, na.rm = TRUE)
  }
  
  if (i %% 1000 == 0) cat("Processado:", i, "/", n, "\n")
}

mt_score <- data.frame()
filtered_essential <- filtered[, c(
  "NU_INSCRICAO", 
  "TP_PRESENCA_MT", 
  "CO_PROVA_MT",
  "NU_NOTA_MT", 
  "TP_LINGUA", 
  "TX_RESPOSTAS_MT",
  "TX_GABARITO_MT")]

if (unique(nchar(mt_gabaritos)) == 45 &
    unique(nchar(mt_respostas)) == 45 &
    unique(ncol(mt_score_item)) == 45) {
  colnames(mt_score_item) <- paste0("Q_", 136:180)
  storage.mode(mt_score_item) <- "integer"
  mt_score <- as.data.frame(cbind(
    filtered_essential,
    TX_RESPOSTAS_MT_NORM = mt_respostas,
    TX_GABARITO_MT_NORM = mt_gabaritos,
    NU_SCORE_MT = as.integer(mt_score_nu[, 1]),
    mt_score_item))
} else {
  stop("Merda") 
}

write.csv(mt_score, file = paste0("2019/MICRODADOS/score_mt.csv"), row.names = FALSE)

rm(list = ls(all.names = TRUE))
gc()
