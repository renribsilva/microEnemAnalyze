# ==============================================================================
# CONFIGURAÇÃO DO AMBIENTE - PROJETO ENEM
# ==============================================================================

# 1. Ativação do ambiente isolado (renv)
# ------------------------------------------------------------------------------
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# 2. Função interna para extrair dependências do DESCRIPTION
# ------------------------------------------------------------------------------
obter_deps <- function() {

  d <- read.dcf("DESCRIPTION")
  deps <- c()
  if ("Imports" %in% colnames(d)) deps <- c(deps, d[, "Imports"])
  if ("DevDependencies" %in% colnames(d)) deps <- c(deps, d[, "DevDependencies"])

  # Limpeza: remove versões (>= 1.0) e espaços
  deps <- gsub("\\s*\\(.*?\\)", "", deps)
  deps <- unlist(strsplit(deps, ",\\s*"))
  return(unique(deps))
}

# 3. Identificar pacotes e cruzar com o renv.lock
# ------------------------------------------------------------------------------
pacotes_projeto <- obter_deps()
pacotes_no_renv <- c()

# Precisamos do jsonlite para ler o lockfile
if (file.exists("renv.lock") && requireNamespace("jsonlite", quietly = TRUE)) {
  lockfile <- jsonlite::fromJSON("renv.lock")
  pacotes_no_renv <- names(lockfile$Packages)
}

# Só carregar o que estiver no DESCRIPTION e validado pelo RENV
pacotes_para_carregar <- intersect(pacotes_projeto, pacotes_no_renv)

# 4. Carregamento Silencioso
# ------------------------------------------------------------------------------
if (length(pacotes_para_carregar) == 0) {
  pacotes_para_carregar <- intersect(pacotes_projeto, utils::installed.packages()[, "Package"])
}

invisible(lapply(pacotes_para_carregar, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

# 5. Carregamento do Pacote Local (Modo Desenvolvimento)
# ------------------------------------------------------------------------------
if (interactive() && file.exists("DESCRIPTION")) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE)
  }
}

# 6. Dashboard de Inicialização
# ------------------------------------------------------------------------------
if (interactive() && requireNamespace("cli", quietly = TRUE)) {

  na_memoria <- gsub("package:", "", grep("^package:", search(), value = TRUE))

  cli::cli_alert_success("Ambiente Isolado: {.info {basename(getwd())}}")
  cat("\n", cli::style_bold(strrep("=", 70)), "\n")

  # Identifica o nome do pacote atual para destaque
  pkg_name <- if(file.exists("DESCRIPTION")) read.dcf("DESCRIPTION")[, "Package"] else ""

  if (length(na_memoria) > 0) {
    cat("\n")
    cat("  📦 PACOTES ATIVOS:\n")
    cat("  ", cli::col_green(paste(sort(na_memoria), collapse = " • ")), "\n")

    if (pkg_name %in% na_memoria) {
      cat("\n")
      cat("  📦 DO PROJETO:\n")
      cat("  ", cli::col_yellow(paste(sort(pkg_name), collapse = " • ")), "\n")
    }

    # --- NOVIDADE AQUI: Filtra e mostra o que é Dev de quem está ativo ---
    d_raw <- read.dcf("DESCRIPTION")
    dev_deps_nomes <- if("DevDependencies" %in% colnames(d_raw)) {
      unlist(strsplit(gsub("\\s*\\(.*?\\)", "", d_raw[,"DevDependencies"]), ",\\s*"))
    } else c()

    dev_ativos <- intersect(na_memoria, dev_deps_nomes)
    if(length(dev_ativos) > 0) {
      cat("\n")
      cat("  📦 DO DESENVOLVEDOR:\n")
      cat("  ", cli::col_blue(paste(sort(dev_ativos), collapse = " • ")), "\n")
    }
    # ---------------------------------------------------------------------
  }
  cat("\n")
  cat(cli::style_bold(strrep("=", 70)), "\n\n")
}

# Limpeza de variáveis auxiliares
rm(list = ls(pattern = "^(pacotes_|na_|lockfile|obter_deps|d|deps|faltantes|p$|pkg_name|dev_|d_raw)"))
