#--------------------------------
# Importa os table_filtered.csv -
#--------------------------------

filtered <- fread("2019/MICRODADOS/table_filtered.csv")
filtered <- filtered %>%
  dplyr::filter(NU_NOTA_MT != 0) %>%
  dplyr::filter(TP_PRESENCA == 1)
source("2019/process_area.R")

#--------------------
# Calcula a amostra -
#--------------------

# N é o tamanho das observações
# e é a margem de erro
# c é o intervalo de confiança
# p é a proporção esperada
calc_sample_size <- function(N, e, c, p) {
  Z <- qnorm(1-(1-c)/2)
  n0 <- (Z^2*p*(1-p))/(e^2)
  return (as.integer(ceiling(n0)))
}

sample_size <- calc_sample_size(N=Inf, e=0.005, c = 0.99, p = 0.5)

indices_aleatorios <- sample.int(n = nrow(filtered), size = sample_size*2, replace = FALSE)
sample <- filtered[indices_aleatorios, ]

# Encontra meus dados
my_score <- filtered %>%
  dplyr::filter(NU_NOTA_MT == 795.4) %>%
  dplyr::filter(NO_MUNICIPIO_PROVA == "Votuporanga")

# Inclui meus dados a sample
sample <- rbind(sample, my_score)

#----------------------------------------------------------------------
# Normaliza o vetor de repostas tendo como referência a prova amarela -
#----------------------------------------------------------------------

itens_2019 <- fread(input='2019/MICRODADOS/microdados_enem_2019/DADOS/ITENS_PROVA_2019.csv')
itens_2019_filtered <- itens_2019 %>%
  dplyr::filter(CO_PROVA == 511 | CO_PROVA == 512 | CO_PROVA == 513 | CO_PROVA == 514 |
                  CO_PROVA == 507 | CO_PROVA == 508 | CO_PROVA == 509 | CO_PROVA == 510 |
                  CO_PROVA == 503 | CO_PROVA == 504 | CO_PROVA == 505 | CO_PROVA == 506 |
                  CO_PROVA == 515 | CO_PROVA == 516 | CO_PROVA == 517 | CO_PROVA == 518 )

# Linguagens
MT_provas <- list(
  azul    = 515,
  amarela = 516,
  rosa    = 517,
  cinza  = 518
)

seq_code_MT <- list(
  azul = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == MT_provas$azul) %>%
    dplyr::filter(TP_LINGUA == 0 | is.na(TP_LINGUA)) %>%
    dplyr::arrange(CO_POSICAO),
  
  rosa = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == MT_provas$rosa) %>%
    dplyr::filter(TP_LINGUA == 0 | is.na(TP_LINGUA)) %>%
    dplyr::arrange(CO_POSICAO),
  
  cinza = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == MT_provas$cinza) %>%
    dplyr::filter(TP_LINGUA == 0 | is.na(TP_LINGUA)) %>%
    dplyr::arrange(CO_POSICAO),
  
  amarela = itens_2019_filtered %>%
    dplyr::filter(CO_PROVA == MT_provas$amarela) %>%
    dplyr::filter(TP_LINGUA == 0 | is.na(TP_LINGUA)) %>%
    dplyr::arrange(CO_POSICAO)
)

gab_amarela <- sample[which(sample$CO_PROVA_MT == 516)[1],]$TX_GABARITO_MT

for (i in seq_len(nrow(sample))) {
  
  prova_origem <- sample[i, ]$CO_PROVA_MT
  
  if (prova_origem %in% c(MT_provas$azul, MT_provas$rosa, MT_provas$cinza)) {
    
    seq_origem <- switch(
      as.character(prova_origem),
      "515" = seq_code_MT$azul,
      "517" = seq_code_MT$rosa,
      "518" = seq_code_MT$cinza
    )
    
    new_TX_RESPOSTAS_MT <- character(nrow(seq_code_MT$amarela))
    g <- strsplit(sample[i, ]$TX_RESPOSTAS_MT, "")[[1]]
    
    for (k in seq_len(nrow(seq_code_MT$amarela))) {
      
      index <- match(
        seq_code_MT$amarela$CO_ITEM[k],
        seq_origem$CO_ITEM
      )
      
      if (is.na(index)) {
        stop(sprintf(
          "Erro: CO_ITEM %s não encontrado na prova %s.",
          seq_code_MT$amarela$CO_ITEM[k],
          prova_origem
        ))
      }
      
      new_TX_RESPOSTAS_MT[k] <- g[index]
    }
    
    score1 <- process_area(
      paste0(new_TX_RESPOSTAS_MT, collapse = ""),
      gab_amarela
    )
    
    score2 <- process_area(
      sample[i, ]$TX_RESPOSTAS_MT,
      sample[i, ]$TX_GABARITO_MT
    )
    
    if (sum(score1) == sum(score2)) {
      sample[i, ]$TX_GABARITO_MT <- gab_amarela
      sample[i, ]$TX_RESPOSTAS_MT <- paste0(new_TX_RESPOSTAS_MT, collapse = "")
    } else {
      stop(sprintf(
        "Erro: Score1 é diferente de score2",
        sum(score1),
        sum(score2)
      ))
    }
  }
  cat("Linha processada:", i, "\n")
}

#----------------
# Escreve p csv -
#----------------

write.csv(sample, file = "2019/MT/sample_MT.csv", row.names = FALSE)
