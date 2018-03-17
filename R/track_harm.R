#' Track harmonic frequency contour
#' 
#' \code{track_harm} tracks the frequency contour of the dominant harmonic.
#' @usage track_harm(wave, f, wl = 512, wn = "hanning", ovlp = 0, fftw = FALSE, at = NULL, 
#' tlim = NULL, threshold = 10, bandpass = NULL, clip = NULL, plot = TRUE, 
#' xlab = "Times (s)", ylab = "Frequency (kHz)",  ylim = c(0, f/2000),
#' adjust.wl = FALSE, dfrq = FALSE, ...) 
#' @param wave A 'wave' object produced by \code{\link[tuneR]{readWave}} or similar functions.
#' @param f Sampling frequency of the wave object (in Hz). Does not need to be specified if embedded in wave.
#' @param wl A numeric vector of length 1 specifying the window length for the FFT, default 
#'   is 512. 
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options. This is used for calculating the frequency spectrum (using \code{\link[seewave]{meanspec}}) and producing the spectrogram (using \code{\link[seewave]{spectro}}, if \code{plot = TRUE}). 
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive time windows, as in \code{\link[seewave]{spectro}}. Default is 0.
#' @param fftw if TRUE calls the function FFT of the library fftw. See Notes of the \code{\link[seewave]{spectro}} function.
#' Default is \code{FALSE}.
#' @param at Time position where the harmonic frequency contour has to be computed (in seconds). Default is \code{NULL}.
#' @param tlim time range in which to measure frequency contours. Default is \code{NULL} (which means it will measure 
#' across the entire wave object).
#' @param threshold Amplitude threshold (\%) for dominant frequency and detection. Default is 10.
#' @param bandpass A numeric vector of length 2 for the lower and upper limits of a frequency bandpass filter (in kHz).
#' @param clip A numeric value to select dominant frequency values according to their amplitude in reference to a maximal value of 1 for the whole signal (has to be >0 & < 1).
#' @param plot Logical, if TRUE plots the dominant frequency against time. Default is \code{TRUE}.
#' @param xlab Label of the time axis.
#' @param ylab Label of the frequency axis.
#' @param ylim A numeric vector of length 2 for the frequency limit of 
#'  the spectrogram (in kHz), as in \code{\link[seewave]{spectro}}. Default is c(0, f/2000).
#' @param adjust.wl Logical. If \code{TRUE} the 'wl' is reset to be equal at the 
#' number of samples in a selections if the samples are less than 'wl'. Default is \code{FALSE}.
#' @param dfrq Logical. If \code{TRUE} seewave's \code{\link[seewave]{dfreq}} is used instead. Default is \code{FALSE}.
#' @param ... Additional arguments to be passed to the plotting function.
#' @seealso \code{\link{trackfreqs}} for tracking frequencies iteratively on selections tables.
#' @export
#' @name track_harm
#' @details This is a modified version of seewave's \code{\link[seewave]{dfreq}} function that allows to track the frequency 
#' contour of a dominant harmonic even when the highest amplitude jumps between harmonics. The arguments and default values of the
#' original \code{\link[seewave]{dfreq}} function have been kept unchanged to facilitate switching between the 2 functions.
#' @examples
#' {
#' #Set temporary working directory
#' # setwd(tempdir())
#' 
#' #load data
#' 
#'# Check this folder
#' getwd()
#'
#'#track both frequencies 
#' 
#' }
#' @author Jerome Sueur, modified by Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on feb-22-2018 (MAS)

track_harm <- function (wave, f, wl = 512, wn = "hanning", ovlp = 0, fftw = FALSE, 
          at = NULL, tlim = NULL, threshold = 10, bandpass = NULL, 
          clip = NULL, plot = TRUE, xlab = "Times (s)", ylab = "Frequency (kHz)", 
          ylim = c(0, f/2000), adjust.wl = FALSE, dfrq = FALSE, ...) 
{
  
  if(length(inputw(wave = wave, f = f)$w) < wl) {
    if(adjust.wl) wl <- length(wave) else
      stop("number of samples lower than 'wl' (i.e. no enough samples) \n check 'adjust.wl' argument")
  } 
  
  if(dfrq) dfreq(wave, f, wl, wn, ovlp, fftw, at, tlim, threshold, bandpass, 
                 clip, plot, xlab, ylab, ylim) else {

                   
  
  if (!is.null(at) && ovlp != 0) 
    stop("The 'ovlp' argument cannot bue used in conjunction with the arguement 'at'.")
  if (!is.null(clip)) {
    if (clip <= 0 | clip >= 1) 
      stop("'clip' value has to be superior to 0 and inferior to 1")
  }
 
  input <- inputw(wave = wave, f = f)
  wave <- input$w
  f <- input$f
  rm(input)
  if (!is.null(tlim)) 
    wave <- cutw(wave, f = f, from = tlim[1], to = tlim[2])
  if (!is.null(threshold)) {
    wave <- afilter(wave = wave, f = f, threshold = threshold, 
                    plot = FALSE)
  }
  n <- nrow(wave)
  if (!is.null(at)) {
    step <- at * f
    N <- length(step)
    if (step[1] <= 0) {
      step[1] <- 1
    }
    if (step[N] + (wl/2) >= n) {
      step[N] <- n - wl
    }
    x <- c(0, at, n/f)
  } else {
    step <- seq(1, n - wl, wl - (ovlp * wl/100))
    N <- length(step)
    x <- seq(0, n/f, length.out = N)
  }
  
  step <- round(step)
  y1 <- seewave::stft(wave = wave, f = f, wl = wl, zp = 0, step = step, 
             wn = wn, fftw = fftw)
  if (!is.null(bandpass)) {
    if (length(bandpass) != 2) 
      stop("'The argument 'bandpass' should be a numeric vector of length 2'")
    if (bandpass[1] > bandpass[2]) 
      stop("The first element of 'bandpass' has to be inferior to the second element, i.e. bandpass[1] < bandpass[2]")
    if (bandpass[1] == bandpass[2]) 
      stop("The limits of the bandpass have to be different")
    lowlimit <- round((wl * bandpass[1])/f)
    upperlimit <- round((wl * bandpass[2])/f)
    y1[-(lowlimit:upperlimit), ] <- 0
  }

  # find peaks close to first dom freq
  maxi <- NULL
  y2 <- NULL
  
  for(i in 1:ncol(y1))
  {
    # standardize z between 0-1
    z <- y1[ , i]/max(y1[ , i])
    z <- ifelse(z > threshold/100, z, 0)                      
    
    # choose the maximum amplitude for the firt time window
    if (i == 1) {
      maxi[i] <- max(z)
      y2[i] <- which.max(z)
    } else {
    pks <- pracma::findpeaks(z, npeaks = 5, sortstr = TRUE)[ , 1:3]
    if(is.vector(pks)) pks <- matrix(pks, ncol = 3)
    pks[,3] <- abs(pks[ ,2] - y2[i - 1])
    maxi[i] <- pks[which.min(pks[ , 3]), 1]  
    y2[i] <- pks[which.min(pks[ , 3]), 2]    
    }
  }
  
  y2[which(maxi == 0)] <- NA
  if (!is.null(clip)) {
    maxi <- apply(y1, MARGIN = 2, FUN = max)
    y2[which(maxi < clip)] <- NA
  }
  y <- (f * y2)/(1000 * wl) - f/(1000 * wl)
  if (!is.null(at)) {
    y <- c(NA, y, NA)
  }
  if (plot) {
    plot(x = x, y = y, xaxs = "i", xlab = xlab, yaxs = "i", 
         ylab = ylab, ylim = ylim, ...)
    invisible(cbind(x, y))
    }
  else return(cbind(x, y))
  }
  
} 