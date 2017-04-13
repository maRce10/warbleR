
# start up message for ggplot2
# use .onAttach for start up messages
# use .onLoad for custom options
# see https://github.com/hadley/r-pkgs/blob/master/r.rmd

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nNOTE: install ggplot2 to run coor.graph()")
}

.onLoad <- function(libname, pkgname) {
if(!Sys.info()[1] == "Windows" & !requireNamespace("pbmcapply",quietly = TRUE)) install.packages("pbmcapply")
  }
