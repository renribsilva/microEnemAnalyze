# Implementa o renv
install.packages("renv")
renv::init()

# Implementa o usethis
renv::install("usethis")
usethis::use_package("usethis", type = "Suggests")

# Implementa o usethis
renv::install("devtools")
usethis::use_package("devtools", type = "Suggests")

# Implementa o ronygen2
renv::install("roxygen2")
usethis::use_package("roxygen2", type = "Suggests")

# Implementa o dlpyr
renv::install("dplyr")
usethis::use_package("dplyr", type = "Suggests")

# Implementa o dlpyr
renv::install("data.table")
usethis::use_package("data.table", type = "Suggests")

# Implementa o dlpyr
renv::install("bit64")
usethis::use_package("bit64", type = "Suggests")
