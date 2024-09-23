library(devtools)
use_git()
use_mit_license()

#first
use_r("install_if_needed") #change with the name of the function
#this opens a new script named as the function

usethis::use_r("calculate_rgb") #same

devtools::load_all() #to load the pkg with all the functions, similar to library()

#ctrl+shift+p

document()

load_all()

use_github()

check()


install()

#These functions setup parts of the package and are typically called once per package:
create_package()
use_git()
use_mit_license()
use_testthat()
use_github()
use_readme_rmd()

#You will call these functions on a regular basis, as you add functions and tests or take on dependencies:
use_r()
use_test()
use_package()
#You will call these functions multiple times per day or per hour, during development:

load_all()
document()
test()
check()

# checking R code for possible problems ... [13s] NOTE
# install_if_needed: no visible global function definition for
# 'install.packages'
# Undefined global functions or variables:
#   install.packages
# Consider adding
# importFrom("utils", "install.packages")
# to your NAMESPACE file.
