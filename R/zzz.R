
# start up message for ggplot2
# use .onAttach for start up messages
# use .onLoad for custom options
# see https://github.com/hadley/r-pkgs/blob/master/r.rmd

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nNOTE: 1. install 'ggplot2' to run coor.graph()")
  packageStartupMessage("2. install 'parallelsugar' to run parallel computing in windows OS")
}

