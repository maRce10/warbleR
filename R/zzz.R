# start up message for ggplot2
# use .onAttach for start up messages
# use .onLoad for custom options
# see https://github.com/hadley/r-pkgs/blob/master/r.rmd

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("\nNOTE: functions are being renamed (run 'print(name_changes)' to see new names). Both old and new names are available in this version \n Please see citation('warbleR') for use in publication")
}

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
  
  auto_detec <- autodetec
  check_sels <- checksels
  check_wavs <- checkwavs
  compare_methods <- compare.methods
  coor_test <- coor.test
  coor_graph <- coor.graph
  df_DTW <- dfDTW
  df_ts <- dfts
  ff_DTW <- ffDTW
  ff_ts <- ffts
  filter_sels <- filtersels
  fix_wavs <- fixwavs
  freq_range <- frange
  freq_range_detec <- frange.detec
  manual_loc <- manualoc
  move_imgs <- move.imgs
  sel_tailor <- seltailor
  se_ts <- sp.en.ts
  snr_specs <- snrspecs
  spec_an <- specan
  spectrograms <- specreator
  track_freqs <- trackfreqs
  color_spectro <- color.spectro
  xc_maps <- xcmaps
  quer_xc <- querxc
  
  
  name_changes <- data.frame('New names' = c("auto_detec", "check_sels", "check_wavs", "compare_methods", "coor_test", "coor_graph", "df_DTW", "df_ts", "ff_ts", "filter_sels", "fix_wavs", "freq_range", "freq_range_detec", "manual_loc", "move_imgs", "sel_tailor", "selec_table", "sim_coor_sing", "se_ts", "snr_specs", "spec_an", "spectrograms", "track_freqs", "color_spectro", "xc_maps", "quer_xc"),
                             'Old names'= c("autodetec", "checksels", "checkwavs", "compare.methods", "coor.test", "coor.graph", "dfDTW", "dfts", "ffts", "filtersels", "fixwavs", "frange", "frange.detec", "manualoc", "move.imgs", "seltailor", "selec.table", "sim.coor.sing", "sp.en.ts", "snrspecs", "specan", "specreator", "trackfreqs", "color.spectro", "xcmaps", "querxc"))
  
  
  
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


