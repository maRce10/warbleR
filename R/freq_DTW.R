#' Acoustic dissimilarity using dynamic time warping on dominant frequency contours
#' 
#' \code{freq_DTW} calculates acoustic dissimilarity of frequency contours using dynamic
#' time warping. Internally it applies the \code{\link[dtw]{dtwDist}} function from the \code{dtw} package.
#' @usage freq_DTW(X = NULL, type = "dominant", wl = 512, wl.freq = 512, length.out = 20, 
#' wn = "hanning", ovlp = 70, bp = NULL, threshold = 15, threshold.time = NULL, 
#' threshold.freq = NULL, img = TRUE, parallel = 1, path = NULL, ts.df = NULL, 
#' img.suffix = "dfDTW", pb = TRUE, clip.edges = TRUE, window.type = "none", 
#' open.end = FALSE, scale = FALSE, frange.detec = FALSE,  fsmooth = 0.1, 
#' adjust.wl = TRUE, ...)
#' @param  X object of class 'selection_table', 'extended_selection_table' or data 
#' frame containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The output of \code{\link{auto_detec}} can be used as the input data frame.
#' @param type Character string to determine the type of contour to be detected. Three options are available, "dominant" (default), "fundamental" and "entropy". 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param length.out A numeric vector of length 1 giving the number of measurements of frequency desired (the length of the time series).
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param wl.freq A numeric vector of length 1 specifying the window length of the spectrogram
#' for measurements on the frequency spectrum. Default is 512. Higher values would provide 
#' more accurate measurements.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70. 
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz). Default is \code{NULL}.
#' @param threshold amplitude threshold (\%) for frequency detection. Default is 15.
#' @param threshold.time amplitude threshold (\%) for the time domain. Use for frequency detection. If \code{NULL} (default) then the 'threshold' value is used.
#' @param threshold.freq amplitude threshold (\%) for the frequency domain. Use for frequency range detection from the spectrum (see 'frange.detec'). If \code{NULL} (default) then the
#'  'threshold' value is used.
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param ts.df Optional. Data frame with frequency contour time series of signals to be compared. If provided "X" is ignored.
#' @param img.suffix A character vector of length 1 with a suffix (label) to add at the end of the names of 
#' image files. Default is \code{NULL}.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param clip.edges Logical argument to control whether edges (start or end of signal) in
#' which amplitude values above the threshold were not detected will be removed. If 
#' \code{TRUE} (default) this edges will be excluded and contours will be calculated on the
#' remaining values. Note that DTW cannot be applied if missing values (e.i. when amplitude is not detected).
#' @param window.type	\code{\link[dtw]{dtw}} windowing control parameter. Character: "none", "itakura", or a function (see \code{\link[dtw]{dtw}}).
#' @param open.end \code{\link[dtw]{dtw}} control parameter. Performs 
#' open-ended alignments (see \code{\link[dtw]{dtw}}).
#' @param scale Logical. If \code{TRUE} frequency values are z-transformed using the \code{\link[base]{scale}} function, which "ignores" differences in absolute frequencies between the signals in order to focus the 
#' comparison in the frequency contour, regardless of the pitch of signals. Default is \code{TRUE}.
#' @param frange.detec DEPRECATED.
#' @param fsmooth A numeric vector of length 1 to smooth the frequency spectrum with a mean
#'  sliding window (in kHz) used for frequency range detection (when \code{frange.detec = TRUE}). This help to average amplitude "hills" to minimize the effect of
#'  amplitude modulation. Default is 0.1.
#' @param adjust.wl Logical. If \code{TRUE} 'wl' (window length) is reset to be lower than the 
#' number of samples in a selection if the number of samples is less than 'wl'. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{\link{track_freq_contour}} for customizing
#' graphical output.
#' @return A matrix with the pairwise dissimilarity values. If img is 
#' \code{FALSE} it also produces image files with the spectrograms of the signals listed in the 
#' input data frame showing the location of the dominant frequencies.
#' @family spectrogram creators
#' @seealso \code{\link{spectrograms}} for creating spectrograms from selections,
#'  \code{\link{snr_spectrograms}} for creating spectrograms to 
#'   optimize noise margins used in \code{\link{sig2noise}} and \code{\link{freq_ts}}, \code{\link{freq_ts}}, for frequency contour overlaid spectrograms.
#'  \href{https://marce10.github.io/2016/09/12/Similarity_of_acoustic_signals_with_dynamic_time_warping_(DTW).html}{blog post on DTW similarity}
#' @export
#' @name freq_DTW
#' @details This function extracts the dominant frequency values as a time series and
#'  then calculates the pairwise acoustic dissimilarity using dynamic time warping.
#' The function uses the \code{\link[stats:approxfun]{approx}} function to interpolate values between dominant
#'  frequency  measures. If 'img' is  \code{TRUE} the function also produces image files
#'  with the spectrograms of the signals listed in the input data frame showing the
#'  location of the dominant frequencies.
#' @examples {
#' #load data
#' data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) #save sound files 
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' 
#' # dominant frequency 
#' freq_DTW(lbh_selec_table, length.out = 30, flim = c(1, 12), bp = c(2, 9), 
#' wl = 300, path = tempdir())
#' 
#' # fundamental frequency
#' freq_DTW(lbh_selec_table, type = "fundamental", length.out = 30, flim = c(1, 12), 
#' bp = c(2, 9), wl = 300, path = tempdir())
#' 
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on nov-31-2016 (MAS)

freq_DTW <-  function(X = NULL, type = "dominant", wl = 512, wl.freq = 512, length.out = 20, wn = "hanning", ovlp = 70, 
           bp = NULL, threshold = 15, threshold.time = NULL, threshold.freq = NULL, 
           img = TRUE, parallel = 1, path = NULL, ts.df = NULL,
           img.suffix = "dfDTW", pb = TRUE, clip.edges = TRUE, 
           window.type = "none", open.end = FALSE, scale = FALSE, frange.detec = FALSE,
           fsmooth = 0.1, adjust.wl = TRUE, ...){     

  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(freq_DTW)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]
  
  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]
  
  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]
  
  # set options left
  if (length(opt.argms) > 0)
    for (q in 1:length(opt.argms))
      assign(names(opt.argms)[q], opt.argms[[q]])
  
  if (is.null(X) & is.null(ts.df)) stop("either 'X' or 'ts.df' should be provided")

  # define number of steps in analysis to print message
  if (pb){
    
    steps <- getOption("int_warbleR_steps")
    if (steps[2] > 0) 
    {
      current.step <- steps[1]
      total.steps <- steps[2] 
    } else {
      total.steps <- 2
      current.step <- 1
    }
  } 
  
  
  if (!is.null(X)) {
    #if X is not a data frame
    if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
    }
  
  #stop if only 1 selection
  if (is.null(ts.df)) {if (nrow(X) == 1) stop("you need more than one selection for freq_DTW")
  
  # threshold adjustment
  if (is.null(threshold.time)) threshold.time <- threshold
  if (is.null(threshold.freq)) threshold.freq <- threshold

  #run freq_ts function
  if (pb)
  write(file = "", x = paste0("measuring dominant frequency contours (step ", current.step," of ", total.steps,"):"))
  
  # get contours
  res <- freq_ts(X, wl = wl, length.out = length.out, wn = wn, ovlp = ovlp, wl.freq = wl.freq,
              bp = bp, threshold.time = threshold.time, threshold.freq = threshold.freq, 
              img = img, parallel = parallel,
              path = path, img.suffix = img.suffix, pb = pb, clip.edges = clip.edges, adjust.wl = adjust.wl, ...)
  } else {
    
    if (!all(c("sound.files", "selec") %in% names(ts.df))) 
      stop(paste(paste(c("sound.files", "selec")[!(c("sound.files", "selec") %in% names(ts.df))], collapse=", "), "column(s) not found in ts.df"))
    
    res <- ts.df
  }
  
    #matrix of dom freq time series
  mat <- res[,3:ncol(res)]
  
  if (scale)
  mat <- t(apply(mat, 1, scale))  

  #stop if NAs in matrix
  if (any(is.na(mat))) stop("missing values in time series (frequency was not detected at
                           the start and/or end of the signal)")
  
  if (pb & is.null(ts.df))
  write(file = "", x = paste0("calculating DTW distances (step ", current.step + 1," of ", total.steps,", no progress bar):"))
  
  
  dm <- dtw::dtwDist(mat, mat, window.type = window.type, open.end = open.end)    
  
  rownames(dm) <- colnames(dm) <- paste(res$sound.files, res$selec, sep = "-")
  return(dm)
}


##############################################################################################################
#' alternative name for \code{\link{freq_DTW}}
#'
#' @keywords internal
#' @details see \code{\link{freq_DTW}} for documentation. \code{\link{dfDTW}} will be deprecated in future versions.
#' @export

dfDTW <- freq_DTW

##############################################################################################################
#' alternative name for \code{\link{freq_DTW}}
#'
#' @keywords internal
#' @details see \code{\link{freq_DTW}} for documentation. \code{\link{freq_DTW}} will be deprecated in future versions.
#' @export

df_DTW <- freq_DTW
