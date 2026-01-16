# ==============================================================================
# CONFIGURAÇÃO DO AMBIENTE - PROJETO ENEM
# ==============================================================================

if (file.exists("renv/activate.R")) source("renv/activate.R")

# 1. Extração estruturada do DESCRIPTION
# ------------------------------------------------------------------------------
obter_deps_categorizadas <- function() {
  if (!file.exists("DESCRIPTION")) return(list(imports = c(), dev = c()))

  d <- read.dcf("DESCRIPTION")
  limpar <- function(campo) {
    if (!campo %in% colnames(d)) return(c())
    deps <- gsub("\\s*\\(.*?\\)", "", d[, campo])
    unlist(strsplit(deps, ",\\s*"))
  }

  list(
    # 'Imports' são pacotes necessários para o funcionamento (Produção)
    imports = unique(limpar("Imports")),
    # 'Suggests'
    dev     = unique(limpar("Suggests"))
  )
}

# 2. Processamento e Carregamento
# ------------------------------------------------------------------------------
deps_list <- obter_deps_categorizadas()

# Validação com renv.lock
pacotes_no_renv <- c()
if (file.exists("renv.lock") && requireNamespace("jsonlite", quietly = TRUE)) {
  lockfile <- jsonlite::fromJSON("renv.lock")
  pacotes_no_renv <- names(lockfile$Packages)
}

imports_faltantes_no_renv <- setdiff(deps_list$imports, pacotes_no_renv)

if (length(imports_faltantes_no_renv) > 0) {
  # Ajustado de %d para %s e adicionado paste()
  stop(sprintf("As dependências [%s] não estão registradas no renv.lock. Rode renv::snapshot().",
               paste(imports_faltantes_no_renv, collapse = ", ")))
}

# 3. Carregamento das dependências
# ------------------------------------------------------------------------------

invisible(lapply(deps_list$imports, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

invisible(lapply(deps_list$dev, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

# 3. Carregamento do Pacote Local
# ------------------------------------------------------------------------------
if (interactive() && file.exists("DESCRIPTION")) {
  if (requireNamespace("pkgload", quietly = TRUE)) pkgload::load_all(".", quiet = TRUE)
}

# 4. Dashboard de Inicialização (Distinção Visual)
# ------------------------------------------------------------------------------
if (interactive() && requireNamespace("cli", quietly = TRUE)) {

  na_memoria <- gsub("package:", "", grep("^package:", search(), value = TRUE))
  pkg_name   <- if(file.exists("DESCRIPTION")) read.dcf("DESCRIPTION")[, "Package"] else ""

  cli::cli_h1("Ambiente: {basename(getwd())}")
  cat("\n")

  # --- Categoria: Produção (Imports) ---
  ativos_prod <- intersect(na_memoria, deps_list$imports)
  if(length(ativos_prod) > 0) {
    cli::cli_alert_info("PRODUÇÃO (Imports):")
    cat("  ", cli::col_green(paste(sort(ativos_prod), collapse = " • ")), "\n\n")
  }

  # --- Categoria: Desenvolvimento (Dev/Suggests) ---
  ativos_dev <- intersect(na_memoria, deps_list$dev)
  if(length(ativos_dev) > 0) {
    cli::cli_alert_info("DESENVOLVIMENTO (DevDeps):")
    cat("  ", cli::col_blue(paste(sort(ativos_dev), collapse = " • ")), "\n\n")
  }

  # --- Categoria: O Próprio Projeto ---
  if (pkg_name %in% na_memoria) {
    cli::cli_alert_success("PROJETO LOCAL ATIVO:")
    cat("  ", cli::col_yellow(paste(pkg_name, collapse = " • ")), "\n\n")
  }

  cli::cli_rule()
}

# Limpeza criteriosa
rm(list = ls(pattern = "^(pacotes_|na_|lockfile|obter_deps|deps_list|todos_|ativos_|p$|pkg_name|d$|limpar|imports_faltantes_no_renv)"))
