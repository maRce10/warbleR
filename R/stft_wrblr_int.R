#internal warbleR function, not to be called by users. It is a modified version of seewave::stft
#last modification on feb-09-2018 (MAS)
stft_wrblr_int <- function (wave, f, wl, zp, step, wn, scale = TRUE, norm = FALSE,                           correction = "none", fftw = FALSE, complex = FALSE) 
{
  if (zp < 0) 
    stop("zero-padding cannot be negative")
  W <- ftwindow(wl = wl, wn = wn, correction = correction)
  if (fftw) {
    p <- fftw::planFFT(wl + zp)
    z <- apply(as.matrix(step), 1, function(x) fftw::FFT(c(wave[x:(wl + 
                                                                     x - 1), ] * W, rep(0, zp)), plan = p))
  }
  else {
    z <- apply(as.matrix(step), 1, function(x) stats::fft(c(wave[x:(wl + 
                                                               x - 1), ] * W, rep(0, zp))))
  }
  z <- z[1:((wl + zp)%/%2), , drop = FALSE]
  z <- z/(wl + zp)
  if (complex == FALSE) {
    z <- 2 * Mod(z)
    if (scale) {
      if (norm) {
        z <- z/apply(X = z, MARGIN = 2, FUN = max)
      }
      else {
        z <- z/max(z)
      }
    }
  }
  return(z)
}
