specprop_wrblr_int <- function (spec, f = NULL, flim = NULL, ...) 
{
  fhz <- f

  if (is.null(f)) {
    if (is.vector(spec)) 
      stop("'f' is missing") else if (is.matrix(spec)) 
      f <- spec[nrow(spec), 1] * 2000 * nrow(spec)/(nrow(spec) -                                                       1)
  }
  
  if (is.matrix(spec)) {
    freq <- spec[, 1]
    freq <- freq * 1000
    spec <- spec[, 2]
  }
  L <- length(spec)
  wl <- L * 2
  if (any(spec < 0)) 
    stop("The frequency spectrum to be analysed should not be in dB")
  if (!is.null(flim)) {
    if (flim[1] < 0 || flim[2] > fhz/2) 
      stop("'flim' should range between 0 and f/2")
  } else {
    flim <- c(0, (f/2 - f/wl)/1000)
  }
  g <- (1000 * wl/2)/(f/2 - f/wl)
  spec <- spec[(flim[1] * g):(flim[2] * g)]
  spec <- spec[!is.na(spec)]
  L <- length(spec)
  amp <- spec/sum(spec)
  cumamp <- cumsum(amp)
  freq <- seq(from = flim[1] * 1000, to = flim[2] * 1000, 
              length.out = L)
  mean <- sum(amp * freq)
  sd <- sqrt(sum(amp * ((freq - mean)^2)))
  sem <- sd/sqrt(L)
  median <- freq[length(cumamp[cumamp <= 0.5]) + 1]
  mode <- freq[which.max(amp)]
  Q25 <- freq[length(cumamp[cumamp <= 0.25]) + 1]
  Q75 <- freq[length(cumamp[cumamp <= 0.75]) + 1]
  IQR <- Q75 - Q25
  cent <- sum(freq * amp)
  z <- amp - mean(amp)
  w <- sd(amp)
  skew <- (sum(z^3)/(L - 1))/w^3
  kurt <- (sum(z^4)/(L - 1))/w^4
  sfm <- sfm(amp)
  sh <- sh(amp)
  prec <- f/wl
  results <- list(mean = mean, sd = sd, median = median, sem = sem, 
                  mode = mode, Q25 = Q25, Q75 = Q75, IQR = IQR, cent = cent, 
                  skewness = skew, kurtosis = kurt, sfm = sfm, sh = sh, 
                  prec = prec)

    return(results)
 
}
