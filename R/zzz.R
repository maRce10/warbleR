# start up message for ggplot2
# use .onAttach for start up messages
# use .onLoad for custom options
# see https://github.com/hadley/r-pkgs/blob/master/r.rmd

.onAttach <- function(libname, pkgname) {
  if(!requireNamespace("ggplot2",quietly = TRUE))
    packageStartupMessage("\nNOTEs: \n 1) install ggplot2 to run coor.graph() \n 2) functions 'imp.raven' and 'imp.syrinx' were moved to the Rraven package \n 3) 'low.freq' and 'high.freq' columns names shoud be renamed to 'bottom.freq' and 'top.freq' \n 3) ")
}

# set warbleR options
.onLoad <- function(libname, pkgname){
  opts <- list(
    wl = NULL,
    parallel = NULL,
    wn = NULL,
    pb = TRUE,
    bp = NULL,
    wav.path = NULL,
    img.path = NULL,
    dest.path = NULL,
    it = NULL,
    res = NULL,
    pal = NULL,
    flim = NULL,
    osci = NULL,
    collevels = NULL
  )
  
  optsx <- getOption("warbleR")
  if (!is.null(optsx)) {
    for (i in intersect(names(opts), names(optsx)))
      opts[[i]] <- optsx[[i]]
    for (i in setdiff(names(optsx), names(opts)))
      opts[[i]] <- optsx[[i]]
  }
  options("warbleR" = opts)
  invisible(NULL)
}

.onUnload <- function(libpath){
  options("warbleR" = NULL)
  invisible(NULL)
}