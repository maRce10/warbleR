# start up message for ggplot2
# use .onAttach for start up messages
# use .onLoad for custom options
# see https://github.com/hadley/r-pkgs/blob/master/r.rmd

.onAttach <- function(libname, pkgname) {
 if(!requireNamespace("ggplot2",quietly = TRUE))
   packageStartupMessage("\nNOTEs: 1) install ggplot2 to run coor.graph() \n
                         2) functions 'imp.raven' and 'imp.syrinx' were moved to the Rraven pacakge")
}

# .onLoad <- function(libname, pkgname) {
# if(!Sys.info()[1] == "Windows" & !requireNamespace("pbmcapply",quietly = TRUE)) install.packages("pbmcapply", repos = "http://cran.us.r-project.org")
#   }
