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
# Linguagens -
#-------------

# Linguagens
LC_provas <- list(
  azul    = "511",
  amarela = "512",
  rosa    = "513",
  branca  = "514",
  laranja = "521",
  verde   = "525"
)

seq_code_LC <- list(
  azul = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(LC_provas$azul)) %>%
    dplyr::arrange(TP_LINGUA, CO_POSICAO),
  
  rosa = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(LC_provas$rosa)) %>%
    dplyr::arrange(TP_LINGUA, CO_POSICAO),
  
  branca = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(LC_provas$branca)) %>%
    dplyr::arrange(TP_LINGUA, CO_POSICAO),
  
  amarela = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(LC_provas$amarela)) %>%
    dplyr::arrange(TP_LINGUA, CO_POSICAO),
  
  laranja = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(LC_provas$laranja)) %>%
    dplyr::arrange(TP_LINGUA, CO_POSICAO),
  
  verde = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(LC_provas$verde)) %>%
    dplyr::arrange(TP_LINGUA, CO_POSICAO)
)

# seq_code_LC$azul
# seq_code_LC$rosa
# seq_code_LC$branca
# seq_code_LC$amarela
# seq_code_LC$laranja
# seq_code_LC$verde

gab_amarela_LC <- filtered[which(filtered$CO_PROVA_LC == 512)[1],]$TX_GABARITO_LC
# nchar(gab_amarela_LC)

lc_gabaritos <- filtered$TX_GABARITO_LC
lc_respostas  <- filtered$TX_RESPOSTAS_LC
lc_score_item <- matrix(NA_character_, nrow = n, ncol = 50)
lc_score_nu <- matrix(NA_character_, nrow = n, ncol = 1)
# unique(nchar(lc_gabaritos))
# unique(nchar(lc_respostas))

for (i in seq_len(n)) {
  
  prova_origem <- as.character(filtered$CO_PROVA_LC[i])
  
  if (prova_origem %in% c(LC_provas$azul, LC_provas$rosa, LC_provas$branca, LC_provas$laranja, LC_provas$verde)) {
    
    seq_origem <- switch (
      prova_origem,
      "511" = seq_code_LC$azul,
      "513" = seq_code_LC$rosa,
      "514" = seq_code_LC$branca,
      "521" = seq_code_LC$laranja,
      "525" = seq_code_LC$verde,
      NULL # Caso a prova não esteja mapeada
    )
    
    if (is.null(seq_origem)) stop(sprintf("Erro: prova_origem %d não está mapeada", prova_origem))
    
    resp_orig_string <- filtered$TX_RESPOSTAS_LC[i]
    gab_orig_string <- filtered$TX_GABARITO_LC[i]
    resp_orig_vetor  <- strsplit(resp_orig_string, "")[[1]]
    
    # Validação de comprimento
    if (nchar(resp_orig_string) != 50) stop(sprintf("Erro: Tamanho da string TX_RESPOSTAS é %d na linha %d", nchar(resp_orig_string), i))
    if (nchar(gab_orig_string) != 50) stop(sprintf("Erro: Tamanho da string TX_GABARITO é %d na linha %d", nchar(gab_orig_string), i))
    
    # Vetorização com match
    indices <- match(seq_code_LC$amarela$CO_ITEM, seq_origem$CO_ITEM)
    
    if (length(indices) != 50) stop(sprintf("Erro: Tamanho de índices é %d na linha %d", nchar(resp_orig_string), i))
    if (any(is.na(indices))) stop(paste("Índice não encontrado não encontrado na prova", prova_origem, "linha", i))
    
    novo_txt_respostas <- as.character()
    novo_txt_respostas <- paste0(resp_orig_vetor[indices], collapse = "")
    
    if (nchar(novo_txt_respostas) != 50) stop(sprintf("Erro: Novo TX_RESPOSTAS tem comprimento %d na linha %d", nchar(resp_orig_string), i))
    if (nchar(gab_amarela_LC) != 50) stop(sprintf("Erro: TX_GABARITO da prova amarela tem comprimento %d na lina %d", nchar(resp_orig_string), i))
    
    # Validação de Score (Soma de acertos deve ser idêntica)
    score_novo <- process_area(novo_txt_respostas, gab_amarela_LC, keepna = TRUE)
    score_orig <- process_area(resp_orig_string, gab_orig_string, keepna = TRUE)
    
    if (sum(score_novo, na.rm = TRUE) == sum(score_orig, na.rm = TRUE)) {
      lc_gabaritos[i] <- gab_amarela_LC
      lc_respostas[i]  <- novo_txt_respostas
      lc_score_item[i, ] <- score_novo
      lc_score_nu[i, ] <- sum(score_novo, na.rm = TRUE)
    } else {
      stop(sprintf("Erro integridade na linha %d: Original %d != Novo %d", i, score_orig, score_novo))
    }
  } else if (prova_origem == LC_provas$amarela) {
    score_novo <- process_area(filtered$TX_RESPOSTAS_LC[i], filtered$TX_GABARITO_LC[i], keepna = TRUE)
    lc_score_item[i, ] <- score_novo
    lc_score_nu[i, ] <- sum(score_novo, na.rm = TRUE)
  }
  
  if (i %% 1000 == 0) cat("Processado:", i, "/", n, "\n")
}

lc_score <- data.frame()
filtered_essential <- filtered[, c(
  "NU_INSCRICAO", 
  "TP_PRESENCA_LC", 
  "CO_PROVA_LC",
  "NU_NOTA_LC", 
  "TP_LINGUA", 
  "TX_RESPOSTAS_LC",
  "TX_GABARITO_LC")]

if (unique(nchar(lc_gabaritos)) == 50 &
    unique(nchar(lc_respostas)) == 50 &
    unique(ncol(lc_score_item)) == 50) {
  nomes_QI <- paste0("QI_", 1:5)
  nomes_QE <- paste0("QE_", 1:5)
  nomes_restantes <- paste0("Q_", 6:45)
  todos_nomes <- c(nomes_QI, nomes_QE, nomes_restantes)
  colnames(lc_score_item) <- todos_nomes
  storage.mode(lc_score_item) <- "integer"
  lc_score <- as.data.frame(cbind(
    filtered_essential,
    TX_RESPOSTAS_LC_NORM = lc_respostas,
    TX_GABARITO_LC_NORM = lc_gabaritos,
    NU_SCORE_LC = as.integer(lc_score_nu[, 1]),
    lc_score_item))
} else {
  stop("Merda") 
}

write.csv(as.data.frame(lc_score), file = paste0("2019/MICRODADOS/score_lc.csv"), row.names = FALSE)

rm(list = ls(all.names = TRUE))
gc()
