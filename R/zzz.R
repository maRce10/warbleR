# start up message for ggplot2
# use .onAttach for start up messages
# use .onLoad for custom options
# see https://github.com/hadley/r-pkgs/blob/master/r.rmd

# .onAttach <- function(libname, pkgname) {
  # packageStartupMessage("\nNOTE: functions have been renamed (run 'print(new_function_names)' to see new names). Both old and new names are available in this version \n Please see citation('warbleR') for use in publication")
# }

# set warbleR options
.onLoad <- function(libname, pkgname){
  opts <- list(
    bp = NULL,
    collevels = NULL,
    flim = NULL,
    it = NULL,
    res = NULL,
    osci = NULL,
    pal = NULL,
    parallel = NULL,
    pb = TRUE,
    wav.path = NULL,
    wl = NULL,
    wn = NULL
    )

  optsx <- getOption("warbleR")
  if (!is.null(optsx)) {
    for (i in intersect(names(opts), names(optsx)))
      opts[[i]] <- optsx[[i]]
    for (i in setdiff(names(optsx), names(opts)))
      opts[[i]] <- optsx[[i]]
  }
  options("warbleR" = opts)
  options("int_warbleR_steps" = c(current = 0, total = 0))
  invisible(NULL)
}



.onUnload <- function(libpath){
  options("int_warbleR_steps" = NULL)
  options("warbleR" = NULL)
  # rm_new_names()
  
  invisible(NULL)
}

