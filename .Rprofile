# ==============================================================================
# CONFIGURAÇÃO DO AMBIENTE - PROJETO ENEM (ESTRITAMENTE EXPLICITADO)
# ==============================================================================
options(repos = c(CRAN = sprintf("https://packagemanager.posit.co/cran/latest/bin/linux/resolute-%s/%s", R.version["arch"], substr(getRversion(), 1, 3))))
if (base::file.exists("renv/activate.R")) base::source("renv/activate.R")

# 1. Extração estruturada do DESCRIPTION
# ------------------------------------------------------------------------------
obter_deps_categorizadas <- function() {
  if (!base::file.exists("DESCRIPTION")) return(base::list(imports = base::c(), dev = base::c()))
  d <- utils::read.dcf("DESCRIPTION")
  limpar <- function(campo) {
    if (base::is.na(base::match(campo, base::colnames(d)))) return(base::c())
    deps <- base::gsub("\\s*\\(.*?\\)", "", d[, campo])
    base::unlist(base::strsplit(deps, ",\\s*"))
  }
  base::list(
    imports = base::unique(limpar("Imports")),
    dev     = base::unique(limpar("Suggests"))
  )
}

# 2. Processamento e Verificação de Dependências
# ------------------------------------------------------------------------------
deps_list <- obter_deps_categorizadas()
pacotes_no_renv <- base::c()
if (base::file.exists("renv.lock")) {
  conteudo_lock <- base::readLines("renv.lock", warn = FALSE)
  linhas_pacotes <- base::grep('^    "[a-zA-Z0-9.]+": \\{$', conteudo_lock, value = TRUE)
  pacotes_no_renv <- base::gsub('^    "|": \\{$', "", linhas_pacotes)
}

imports_faltantes <- base::setdiff(deps_list$imports, pacotes_no_renv)

if (base::length(imports_faltantes) > 0) {
  base::stop(base::sprintf("Dependências [%s] fora do renv.lock. Rode renv::snapshot().",
               base::paste(imports_faltantes, base::collapse = ", ")))
}

# 3. Carregamento das dependências (Imports e Suggests)
# ------------------------------------------------------------------------------
base::invisible(base::lapply(deps_list$imports, function(p) {
  base::suppressPackageStartupMessages(base::library(p, character.only = TRUE))
}))

base::invisible(base::lapply(deps_list$dev, function(p) {
  base::suppressPackageStartupMessages(base::library(p, character.only = TRUE))
}))

# 4. Dashboard de Inicialização Minimalista
# ------------------------------------------------------------------------------
if (base::interactive()) {
  # search é base
  na_memoria <- base::gsub("package:", "", base::grep("^package:", base::search(), value = TRUE))
  
  pkg_name <- ""
  if (base::file.exists("DESCRIPTION")) {
    pkg_name <- utils::read.dcf("DESCRIPTION")[1, "Package"]
  }

  base::cat("\n--- Ambiente:", base::basename(base::getwd()), "---\n")

  # Intersect é base
  ativos_prod <- base::intersect(na_memoria, deps_list$imports)
  if (base::length(ativos_prod) > 0) {
    base::cat("  PRODUÇÃO:", base::paste(base::sort(ativos_prod), collapse = " | "), "\n")
  }

  ativos_dev <- base::intersect(na_memoria, deps_list$dev)
  if (base::length(ativos_dev) > 0) {
    base::cat("  DEV/SUGGESTS:", base::paste(base::sort(ativos_dev), collapse = " | "), "\n")
  }

  if (!base::is.na(base::match(pkg_name, na_memoria))) {
    base::cat("  PROJETO LOCAL ATIVO:", pkg_name, "\n")
  }
  
  base::cat("------------------------------------------\n\n")
}

# 5. Limpeza de objetos auxiliares da sessão
# ------------------------------------------------------------------------------
base::rm(list = base::ls(pattern = "^(pacotes_|na_|lockfile|obter_deps|deps_list|ativos_|p$|pkg_name|d$|limpar|imports_faltantes|conteudo_lock|linhas_pacotes)"))
