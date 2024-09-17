#' Install packages if they have not been isntalled yet
#'
#' @param pkg The name of the package
#' @inheritParams stringr::str_split
#'
#' @return package installed and/or loaded.
#' @export
#'
#' @examples
#' install_if_needed("dplyr")
#' # List of packages you want to ensure are installed
#'
#' packages <- c("devtools", "BiocManager", "dartRverse", "leaflet")
#' #' # Install and load each package
#' for (pkg in packages) {
#'   install_if_needed(pkg)
#' }

install_if_needed <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}


