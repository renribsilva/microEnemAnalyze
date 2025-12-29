# Implementa o renv
install.packages("renv")
renv::init()

# Implementa o usethis
renv::install("usethis")
usethis::use_package("usethis", type = "Suggests")

# Implementa o ronygen2
renv::install("roxygen2")
usethis::use_package("roxygen2", type = "Suggests")

# Implementa o tidyverse
renv::install("dplyr")
usethis::use_package("dplyr", type = "Suggests")
