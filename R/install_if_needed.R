install_if_needed <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}
