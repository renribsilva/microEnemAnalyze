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

#-----------------------
# Ciências da natureza -
#-----------------------

# Ciências da natureza
CN_provas <- list(
  azul    = "503",
  amarela = "504",
  cinza   = "505",
  rosa    = "506",
  laranja = "519",
  verde   = "523"
)

seq_code_CN <- list(
  azul = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CN_provas$azul)) %>%
    dplyr::arrange(CO_POSICAO),
  
  cinza = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CN_provas$cinza)) %>%
    dplyr::arrange(CO_POSICAO),
  
  rosa = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CN_provas$rosa)) %>%
    dplyr::arrange(CO_POSICAO),
  
  amarela = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CN_provas$amarela)) %>%
    dplyr::arrange(CO_POSICAO),
  
  laranja = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CN_provas$laranja)) %>%
    dplyr::arrange(CO_POSICAO),
  
  verde = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == as.integer(CN_provas$verde)) %>%
    dplyr::arrange(CO_POSICAO)
)

gab_amarela_CN <- filtered[which(filtered$CO_PROVA_CN == 504)[1],]$TX_GABARITO_CN

cn_gabaritos <- filtered$TX_GABARITO_CN
cn_respostas  <- filtered$TX_RESPOSTAS_CN
cn_score_item <- matrix(NA_character_, nrow = n, ncol = 45)
cn_score_nu <- matrix(NA_character_, nrow = n, ncol = 1)

for (i in seq_len(n)) {
  
  prova_origem <- as.character(filtered$CO_PROVA_CN[i])
  
  if (prova_origem %in% c(CN_provas$azul, CN_provas$cinza, CN_provas$rosa, CN_provas$laranja, CN_provas$verde)) {
    
    seq_origem <- switch (
      prova_origem,
      "503" = seq_code_CN$azul,
      "505" = seq_code_CN$cinza,
      "506" = seq_code_CN$rosa,
      "519" = seq_code_CN$laranja,
      "523" = seq_code_CN$verde,
      NULL # Caso a prova não esteja mapeada
    )
    
    if (is.null(seq_origem)) stop(sprintf("Erro: prova_origem %d não está mapeada", prova_origem))
    
    resp_orig_string <- filtered$TX_RESPOSTAS_CN[i]
    gab_orig_string <- filtered$TX_GABARITO_CN[i]
    resp_orig_vetor  <- strsplit(resp_orig_string, "")[[1]]
    
    # Validação de comprimento
    if (nchar(resp_orig_string) != 45) stop(sprintf("Erro: Tamanho da string TX_RESPOSTAS é %d na linha %d", nchar(resp_orig_string), i))
    if (nchar(gab_orig_string) != 45) stop(sprintf("Erro: Tamanho da string TX_GABARITO é %d na linha %d", nchar(gab_orig_string), i))
    
    # Vetorização com match
    indices <- match(seq_code_CN$amarela$CO_ITEM, seq_origem$CO_ITEM)
    
    if (length(indices) != 45) stop(sprintf("Erro: Tamanho de índices é %d na linha %d", nchar(resp_orig_string), i))
    if (any(is.na(indices))) stop(paste("Índice não encontrado não encontrado na prova", prova_origem, "linha", i))
    
    novo_txt_respostas <- as.character()
    novo_txt_respostas <- paste0(resp_orig_vetor[indices], collapse = "")
    
    if (nchar(novo_txt_respostas) != 45) stop(sprintf("Erro: Novo TX_RESPOSTAS tem comprimento %d na lina %d", nchar(resp_orig_string), i))
    if (nchar(gab_amarela_CN) != 45) stop(sprintf("Erro: TX_GABARITO da prova amarela tem comprimento %d na lina %d", nchar(resp_orig_string), i))
    
    # Validação de Score (Soma de acertos deve ser idêntica)
    score_novo <- process_area(novo_txt_respostas, gab_amarela_CN, keepna = TRUE)
    score_orig <- process_area(resp_orig_string, gab_orig_string, keepna = TRUE)
    
    if (sum(score_novo, na.rm = TRUE) == sum(score_orig, na.rm = TRUE)) {
      cn_gabaritos[i] <- gab_amarela_CN
      cn_respostas[i]  <- novo_txt_respostas
      cn_score_item[i, ] <- score_novo
      cn_score_nu[i, ] <- sum(score_novo, na.rm = TRUE)
    } else {
      stop(sprintf("Erro integridade na linha %d: Original %d != Novo %d", i, score_orig, score_novo))
    }
  } else if (prova_origem == CN_provas$amarela) {
    score_novo <- process_area(filtered$TX_RESPOSTAS_CN[i], filtered$TX_GABARITO_CN[i], keepna = TRUE)
    cn_score_item[i, ] <- score_novo
    cn_score_nu[i, ] <- sum(score_novo, na.rm = TRUE)
  }
  
  if (i %% 1000 == 0) cat("Processado:", i, "/", n, "\n")
}

cn_score <- data.frame()
filtered_essential <- filtered[, c(
  "NU_INSCRICAO", 
  "TP_PRESENCA_CN", 
  "CO_PROVA_CN",
  "NU_NOTA_CN", 
  "TP_LINGUA", 
  "TX_RESPOSTAS_CN",
  "TX_GABARITO_CN")]

if (unique(nchar(cn_gabaritos)) == 45 & 
    unique(nchar(cn_respostas)) == 45 &
    unique(ncol(cn_score_item)) == 45) {
  colnames(cn_score_item) <- paste0("Q_", 91:135)
  storage.mode(cn_score_item) <- "integer"
  cn_score <- as.data.frame(cbind(
    filtered_essential,
    TX_RESPOSTAS_CN_NORM = cn_respostas,
    TX_GABARITO_CN_NORM = cn_gabaritos,
    NU_SCORE_CN = as.integer(cn_score_nu[, 1]),
    cn_score_item))
} else {
  stop("Merda") 
}

write.csv(cn_score, file = paste0("2019/MICRODADOS/score_cn.csv"), row.names = FALSE)

rm(list = ls(all.names = TRUE))
gc()
