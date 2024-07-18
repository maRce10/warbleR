#' Deprecated functions and function names
#'
#' @keywords internal
#' @details functions that have been deprecated or moved to other packages.
#' @export

ffDTW <- ff_DTW <- function(...) warning2(x = "This function has been deprecated. DTW distances between fundamental frequency contours can be calculated using `freq_DTW`")

freq_ts <- ff_ts <- function(...) warning2(x = "This function has been deprecated. Fundamental frequency contours can be calculated using `freq_ts`")

dfts <- df_ts <- function(...) warning2(x = "This function has been deprecated. Dominant frequency contours can be calculated using `freq_ts`")

sp.en.ts <- entropy_ts <- function(...) warning2(x = "This function has been deprecated. Spectral entropy contours can be calculated using `freq_ts`")

quer_ml <- function(...) warning2(x = "This function has been removed temporarily due to API changes at Macaulay Library")

xcorr.graph <- function(...) warning2(x = "This function has been deprecated as it was not compatible with changes to improve performance in 'xcorr()' (use corrplot package instead)")

manualoc <- manual_loc <- function(...) warning2(x = "This function has been deprecated. Try Raven Software (Cornell Lab of Ornithology) or Audacity to manually annotate. The Rraven package (imp_raven) can be used for importing Raven annotations: try `install.packages('Rraven')`")

find_annotations <- function(...) warning2(x = "This function has been removed due to API changes at audioblast.org")

optimize_auto_detec <- function(...) {
  
  warning2(x = "optimize_auto_detec() function has been deprecated. Use `optimize_energy_detector()` from the 'ohun' package instead. Look at the package ohun for signal detection tools") 
}

song_param <- function(...) warning2(x = "This function name has been deprecated. Use `song_analysis()` instead")

auto_detec <-
  function(...) {
    
    warning2(x = "auto_detec()/autodetec() function has been deprecated. Use `energy_detector()` from the 'ohun' package instead. Look at the package ohun for signal detection tools") 

  }

find_peaks <-
  function(...) {
    
    warning2(x = "find_peaks() function has been deprecated. Use `template_detector()` from the 'ohun' package instead. Look at the package ohun for signal detection tools") 
    
  }

check_wavs <-
  function(...) {
    
    warning2(x = "check_wavs() function name has been deprecated. Use `check_sound_files()` instead.") 
    
  }

checksels <-
  function(...) {
    
    warning2(x = "checksels() function name has been deprecated. Use `check_sels()` instead.") 
    
  }

coor.test <-
  function(...) {
    
    warning2(x = "coor.test() function name has been deprecated. Use `test_coordination()` instead.") 
    
  }

color.spectro <-
  function(...) {
    
    warning2(x = "color.spectro() function name has been deprecated. Use `color_spectro()` instead.") 
    
  }

coor.graph <-
  function(...) {
    
    warning2(x = "plot.graph() function name has been deprecated. Use `plot_coordination()` instead.") 
    
  }

dfDTW <-
  function(...) {
    
    warning2(x = "dfDTW() function name has been deprecated. Use `freq_DTW()` instead.") 
    
  }

ffDTW <-
  function(...) {
    
    warning2(x = "ffDTW() function name has been deprecated. Use `freq_DTW()` instead.") 
    
  }


df_DTW <-
  function(...) {
    
    warning2(x = "df_DTW() function name has been deprecated. Use `freq_DTW()` instead.") 
    
  }


filtersels <-
  function(...) {
    
    warning2(x = "filtersels() function name has been deprecated. Use `filter_sels()` instead.") 
    
  }

fixwavs <-
  function(...) {
    
    warning2(x = "fixwavs() function name has been deprecated. Use `fix_wavs()` instead.") 
    
  }

frange.detec <-
  function(...) {
    
    warning2(x = "frange.detec() function name has been deprecated. Use `freq_range_detec()` instead.") 
    
  }


info_wavs <-
  function(...) {
    
    warning2(x = "info_wavs() function name has been deprecated. Use `info_sound_files()` instead.") 
    
  }

lspec <-
  function(...) {
    
    warning2(x = "lspec() function name has been deprecated. Use `full_spectrograms()` instead.") 
    
  }

make.selection.table <-
  function(...) {
    
    warning2(x = "make.selection.table() function name has been deprecated. Use `selection_table()` instead.") 
    
  }


move_imgs <-
  function(...) {
    
    warning2(x = "move_imgs() function name has been deprecated. Use `move_images()` instead.") 
    
  }

mp3_2_wav <-
  function(...) {
    
    warning2(x = "mp3_2_wav() function name has been deprecated. Use `mp32wav()` instead.") 
    
  }


querxc <-
  function(...) {
    
    warning2(x = "querxc() function name has been deprecated. Use `query_xc()` instead.") 
    
  }

rename_waves_est <-
  function(...) {
    
    warning2(x = "rename_waves_est() function name has been deprecated. Use `rename_est_waves()` instead.") 
    
  }


resample_est_waves <-
  function(...) {
    
    warning2(x = "resample_est_waves() function name has been deprecated. Use `resample_est()` instead.") 
    
  }


signal_2_noise <-
  function(...) {
    
    warning2(x = "signal_2_noise() function name has been deprecated. Use `sig2noise()` instead.") 
    
  }


sim_songs <-
  function(...) {
    
    warning2(x = "sim_songs() function name has been deprecated. Use `simulate_songs()` instead.") 
    
  }

snr_specs <-
  function(...) {
    
    warning2(x = "snr_specs() function name has been deprecated. Use `snr_spectrograms()` instead.") 
    
  }

spec_param <-
  function(...) {
    
    warning2(x = "spec_param() function name has been deprecated. Use `tweak_spectro()` instead.") 
    
  }

specan <-
  function(...) {
    
    warning2(x = "specan() function name has been deprecated. Use `spectro_analysis()` instead.") 
    
  }

split_wavs <-
  function(...) {
    
    warning2(x = "split_wavs() function name has been deprecated. Use `split_sound_files()` instead.") 
    
  }

trackfreqs <- function(...) {
  
  warning2(x = "trackfreqs() function name has been deprecated. Use `track_freq_contour()` instead.") 
  
}


try_na <- function(...) {
  
  warning2(x = "try_na() function has been deprecated") 
  
}

wav_dur <- function(...) {
  
  warning2(x = "wav_dur() function name has been deprecated. Use `duration_sound_files()` instead.") 
  
}

xcmaps <- function(...) {
  
  warning2(x = "xcmaps() function name has been deprecated. Use `map_xc()` instead.") 
  
}

xcorr <- function(...) {
  
  warning2(x = "xcorr() function name has been deprecated. Use `cross_correlation()` instead.") 
  
}
