#' Install a package only if it hasn't before. Else/then load it
#'
#' @param pkg the name of the package in quotation marks
#'
#' @return NULL Package loaded
#'
#' @import utils
#'
#' @export
#'
#' @examples
#' #' install_if_needed("dplyr")
#'
#' # List of packages you want to ensure are installed
#' packages <- c("devtools", "BiocManager", "dartRverse", "leaflet")
#' #' # Install and load each package
#' for (pkg in packages) {
#'   install_if_needed(pkg)
#' }
#'
install_if_needed <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}
