#' Track harmonic frequency contour
#'
#' \code{track_harmonic} tracks the frequency contour of the dominant harmonic.
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
#' @param adjust.wl Logical. If \code{TRUE} 'wl' (window length) is reset to be lower than the
#' number of samples in a selection if the number of samples is less than 'wl'. Default is \code{FALSE}.
#' @param dfrq Logical. If \code{TRUE} seewave's \code{\link[seewave]{dfreq}} is used instead. Default is \code{FALSE}.
#' @param ... Additional arguments to be passed to the plotting function.
#' @seealso \code{\link{track_freq_contour}} for tracking frequencies iteratively on selections tables.
#' @export
#' @name track_harmonic
#' @details This is a modified version of seewave's \code{\link[seewave]{dfreq}} function that allows to track the frequency
#' contour of a dominant harmonic even when the highest amplitude jumps between harmonics. The arguments and default values of the
#' original \code{\link[seewave]{dfreq}} function have been kept unchanged to facilitate switching between the 2 functions.
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Jerome Sueur, modified by Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on feb-22-2018 (MAS)

track_harmonic <- function(wave, f, wl = 512, wn = "hanning", ovlp = 0, fftw = FALSE,
                           at = NULL, tlim = NULL, threshold = 10, bandpass = NULL,
                           clip = NULL, plot = TRUE, xlab = "Times (s)", ylab = "Frequency (kHz)",
                           ylim = c(0, f / 2000), adjust.wl = FALSE, dfrq = FALSE, ...) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(track_harmonic)

  # get warbleR options
  opt.argms <- if (!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0

  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]

  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]

  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]

  # set options left
  if (length(opt.argms) > 0) {
    for (q in seq_len(length(opt.argms))) {
      assign(names(opt.argms)[q], opt.argms[[q]])
    }
  }

  if (length(inputw(wave = wave, f = f)$w) < wl) {
    if (adjust.wl) {
      wl <- length(wave)
    } else {
      stop("number of samples lower than 'wl' (i.e. no enough samples) \n check 'adjust.wl' argument")
    }
  }

  if (!is.null(at) && ovlp != 0) {
    stop("The 'ovlp' argument cannot bue used in conjunction with the arguement 'at'.")
  }
  if (!is.null(clip)) {
    if (clip <= 0 | clip >= 1) {
      stop("'clip' value has to be superior to 0 and inferior to 1")
    }
  }

  input <- inputw(wave = wave, f = f)
  wave <- input$w
  f <- input$f
  rm(input)
  if (!is.null(tlim)) {
    wave <- cutw(wave, f = f, from = tlim[1], to = tlim[2])
  }

  if (!is.null(threshold)) {
    wave <- afilter(
      wave = wave, f = f, threshold = threshold,
      plot = FALSE
    )
  }

  n <- nrow(wave)
  if (!is.null(at)) {
    step <- at * f
    N <- length(step)
    if (step[1] <= 0) {
      step[1] <- 1
    }
    if (step[N] + (wl / 2) >= n) {
      step[N] <- n - wl
    }
    x <- c(0, at, n / f)
  } else {
    step <- seq(1, n - wl, wl - (ovlp * wl / 100))
    N <- length(step)
    x <- seq(0, n / f, length.out = N)
  }

  step <- round(step)
  y1 <- seewave::stdft(
    wave = wave, f = f, wl = wl, zp = 0, step = step,
    wn = wn
  )
  if (!is.null(bandpass)) {
    if (length(bandpass) != 2) {
      stop("'The argument 'bandpass' should be a numeric vector of length 2'")
    }
    if (bandpass[1] > bandpass[2]) {
      stop("The first element of 'bandpass' has to be inferior to the second element, i.e. bandpass[1] < bandpass[2]")
    }
    if (bandpass[1] == bandpass[2]) {
      stop("The limits of the bandpass have to be different")
    }
  }

  # lowlimit <- round((wl * bandpass[1])/f)
  # upperlimit <- round((wl * bandpass[2])/f)
  # y1[-(lowlimit:upperlimit), ] <- 0

  # freq values for each freq window   (using mid point of each window)
  freq.val <- ((1:nrow(y1) * f / wl) - (f / (wl * 2)))

  y1[freq.val < bandpass[1] | freq.val > bandpass[2]] <- 0


  if (dfrq) {
    maxi <- apply(y1, MARGIN = 2, FUN = max)
    y2 <- apply(y1, MARGIN = 2, FUN = which.max)
  } else {
    # find peaks close to first dom freq
    maxi <- NULL
    y2 <- NULL

    for (i in seq_len(ncol(y1)))
    {
      # standardize z between 0-1
      z <- y1[, i] / max(y1[, i])
      z <- ifelse(z > threshold / 100, z, 0)

      # choose the maximum amplitude for the firt time window
      if (i == 1) {
        maxi[i] <- max(z)
        y2[i] <- which.max(z)
      } else {
        pks <- pracma::findpeaks(z, npeaks = 5, sortstr = TRUE)[, 1:3]
        if (is.vector(pks)) pks <- matrix(pks, ncol = 3)
        pks[, 3] <- abs(pks[, 2] - y2[i - 1])
        maxi[i] <- pks[which.min(pks[, 3]), 1]
        y2[i] <- pks[which.min(pks[, 3]), 2]
      }
    }
  }

  y2[which(maxi == 0)] <- NA
  if (!is.null(clip)) {
    maxi <- apply(y1, MARGIN = 2, FUN = max)
    y2[which(maxi < clip)] <- NA
  }
  # y <- (f * y2)/(1000 * wl) - f/(1000 * wl)
  y <- freq.val[y2]

  if (!is.null(at)) {
    y <- c(NA, y, NA)
  }

  y <- y / 1000

  if (plot) {
    plot(
      x = x, y = y, xaxs = "i", xlab = xlab, yaxs = "i",
      ylab = ylab, ylim = ylim, ...
    )
    invisible(cbind(x, y))
  } else {
    return(cbind(x, y))
  }
}
