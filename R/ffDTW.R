#' Acoustic dissimilarity using dynamic time warping on fundamental frequency contours
#' 
#' \code{ffDTW} calculates acoustic dissimilarity of fundamental frequency contours using dynamic
#' time warping. Internally it applies the \code{\link[dtw]{dtwDist}} function from the \code{dtw} package.
#' @usage ffDTW(X, wl = 512, length.out = 20, wn = "hanning", ovlp = 70, 
#' bp = c(0, 22), threshold = 5, img = TRUE, parallel = 1, path = NULL, 
#' img.suffix = "ffDTW", pb = TRUE, clip.edges = TRUE, window.type = "none", 
#' open.end = FALSE, scale = FALSE, ...)
#' @param  X 'selection.table' object or data frame with results containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param length.out A numeric vector of length 1 giving the number of measurements of fundamental 
#' frequency desired (the length of the time series).
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70. 
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param threshold amplitude threshold (\%) for fundamental frequency detection. Default is 5.
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#'  Not available in Windows OS.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.  
#' @param img.suffix A character vector of length 1 with a sufix (label) to add at the end of the names of 
#' image files. Default is \code{NULL}.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @param clip.edges Logical argument to control whether edges (start or end of signal) in
#' which amplitude values above the threshold were not detected will be removed. If 
#' \code{TRUE} (default) this edges will be excluded and signal contour will be calculated on the
#' remainging values. Note that DTW cannot be applied if missing values (e.i. when amplitude is not detected).
#' @param window.type	\code{\link[dtw]{dtw}} windowing control parameter. Character: "none", "itakura", or a function (see \code{\link[dtw]{dtw}}).
#' @param open.end \code{\link[dtw]{dtw}} control parameter. Performs 
#' open-ended alignments (see \code{\link[dtw]{dtw}}).
#' @param scale Logical. If \code{TRUE} dominant frequency values are z-transformed using the \code{\link[base]{scale}} function, which "ignores" differences in absolute frequencies between the signals in order to focus the 
#' comparison in the frequency contour, regardless of the pitch of signals. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{\link{trackfreqs}} for customizing
#' graphical output.
#' @return A matrix with the pairwise dissimilarity values. If img is 
#' \code{FALSE} it also produces image files with the spectrograms of the signals listed in the 
#' input data frame showing the location of the fundamental frequencies.
#' @family spectrogram creators
#' @seealso \code{\link{specreator}} for creating spectrograms from selections,
#'  \code{\link{snrspecs}} for creating spectrograms to 
#'   optimize noise margins used in \code{\link{sig2noise}}
#' @export
#' @name ffDTW
#' @details This function extracts the fundamental frequency values as a time series and
#'  then calculates the pairwise acoustic dissimilarity of the selections using dynamic time warping.
#' The function uses the \code{\link[stats]{approx}} function to interpolate values between fundamental
#'  frequency  measures. If 'img' is  \code{TRUE} the function also produces image files
#'  with the spectrograms of the signals listed in the input data frame showing the
#'  location of the fundamental frequencies. Note that if no amplitude is detected at the begining or end 
#'  of the signals then NAs will be generated. On the other hand, if amplitude is not detected in between signal
#'  segments in which amplitude was detected then the values of this adjacent segments will be interpolated to fill out the missing values (e.g. no NAs in between detected amplitude segments). 
#' @seealso dfDTW \code{\link{dfts}}, \code{\link{ffts}}, \code{\link{dfDTW}}
#' @examples
#' {
#' # set the temp directory
#' setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2","selec.table"))
#' writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
#' writeWave(Phae.long1, "Phae.long1.wav")
#' 
#' # run function 
#' ffDTW(selec.table[1:4,], length.out = 30, flim = c(1, 12), img = TRUE, bp = c(1, 9), wl = 300)
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on oct-26-2016 (MAS)

ffDTW <- function(X, wl = 512, length.out = 20, wn = "hanning", ovlp = 70, 
                  bp = c(0, 22), threshold = 5, img = TRUE, parallel = 1, path = NULL, 
                  img.suffix = "ffDTW", pb = TRUE, clip.edges = TRUE,
                  window.type = "none", open.end = FALSE, scale = FALSE, ...){     
  

    #if X is not a data frame
    if(!class(X) %in% c("data.frame", "selection.table")) stop("X is not of a class 'data.frame' or 'selection table")
    
    

  #stop if only 1 selection
  if(nrow(X) == 1) stop("you need more than one selection for ffDTW")
  
  # run ffts function
  res <- ffts(X, wl = wl, length.out = length.out, wn = wn, ovlp = ovlp, 
              bp = bp, threshold = threshold, img = img, parallel = parallel,
              path = path, img.suffix = img.suffix, pb = pb, clip.edges = clip.edges, ...)
  
  #matrix of fund freq time series
  mat <- res[,3:ncol(res)]
 
  if(scale)
    mat <- t(apply(mat, 1, scale))  
  
  #stop if NAs in matrix
    if(any(is.na(mat))) stop("missing values in frequency time series (fundamental frequency was not detected at one or both extremes of the signal)")
  
  dm <- dtw::dtwDist(mat, mat, window.type = window.type, open.end = open.end)       

    rownames(dm) <- colnames(dm) <- paste(res$sound.files, res$selec, sep = "-")
  return(dm)

  }
