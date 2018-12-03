#internal warbleR function, not to be called by users. It is a modified version of seewave::dfreq that returns more accurate freq values

dfreq_wrblr_int <- function (wave, f, wl = 512, wn = "hanning", ovlp = 0, fftw = FALSE, 
          at = NULL, tlim = NULL, threshold = NULL, bandpass = NULL, 
          clip = NULL, plot = TRUE, xlab = "Times (s)", ylab = "Frequency (kHz)", 
          ylim = c(0, f/2000), ...) 
{
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
    step <- seq(1, n + 1 - wl, wl - (ovlp * wl/100))
    N <- length(step)
    x <- seq(0, n/f, length.out = N)
  }
  step <- round(step)
  y1 <- stdft(wave = wave, f = f, wl = wl, zp = 0, step = step, 
              wn = wn, fftw = fftw)
  if (!is.null(bandpass)) {
    if (length(bandpass) != 2) 
      stop("'The argument 'bandpass' should be a numeric vector of length 2'")
    if (bandpass[1] > bandpass[2]) 
      stop("The first element of 'bandpass' has to be inferior to the second element, i.e. bandpass[1] < bandpass[2]")
    if (bandpass[1] == bandpass[2]) 
      stop("The limits of the bandpass have to be different")
    # lowlimit <- round((wl * bandpass[1])/f)
    # upperlimit <- round((wl * bandpass[2])/f)
     # y1[-(lowlimit:upperlimit), ] <- 0
  
    ## changes here
  # freq values for each freq window   (using mid point of each window)
  freq.val <- ((1:nrow(y1) * f / wl) - (f / (wl * 2))) 
  
  y1[ freq.val < bandpass[1] | freq.val > bandpass[2]] <- 0
     
  }
  maxi <- apply(y1, MARGIN = 2, FUN = max)
  y2 <- apply(y1, MARGIN = 2, FUN = which.max)
  
  y2[which(maxi == 0)] <- NA
  
  if (!is.null(clip)) {
    maxi <- apply(y1, MARGIN = 2, FUN = max)
    y2[which(maxi < clip)] <- NA
  }

  ## changes here
  # y <- (f * y2)/(1000 * wl) - f/(1000 * wl)
  y <- freq.val[y2] / 1000
  
  
  if (!is.null(at)) {
    y <- c(NA, y, NA)
  }
  if (plot) {
    plot(x = x, y = y, xaxs = "i", xlab = xlab, yaxs = "i", 
         ylab = ylab, ylim = ylim, ...)
    invisible(cbind(x, y))
  } else return(cbind(x, y))
}
