#internal warbleR function, not to be called by users. Detects frequency range.

frd.INTFUN <- function(wave, wl = 512, fsmooth = 0.1, threshold = 10, wn = "hanning", flim = c(0, 22), bp = NULL, ovlp = 50, min.range = NULL)
{
  # get sampling rate
  f <-  wave@samp.rate
  
  # fix flim
  flim <- c(0, floor(f/2000))
  if(flim[2] > ceiling(f/2000) - 1) flim[2] <- ceiling(f/2000) - 1 
  
  
  # mean spectrum
  spc <- meanspec(wave, plot = FALSE, wl = wl, f = f, wn = wn, ovlp = ovlp)
  
  # get frequency windows length for smoothing
  step <- wave@samp.rate/wl/1000
  
  fsmooth <- fsmooth/step
  
  # number of samples
  n <- nrow(spc)
  
  # smoothing parameter
  FWL <- fsmooth - 1
  
  # smooth 
  z <- apply(as.matrix(1:(n - FWL)), 1, function(y) sum(spc[y:(y + FWL), 2]))
  zf <- seq(min(spc[,1]), max(spc[,1]), length.out = length(z))
  
  if(!is.null(bp)) { 
    #remove range outsde bp
    z <- z[zf > bp[1] & zf < bp[2]]
    zf <- zf[zf > bp[1] & zf < bp[2]]
  }
  
  # make minimum amplitude 0
  z <- z - min(z)
  z[z < 0] <- 0
  
  # normalize amplitude from 0 to 1
  z <- z/max(z)
  
  #get freqs crossing threshold
  z1 <- rep(0, length(z))
  z1[z > threshold/100] <- 1 
  z2 <- z1[2:length(z1)] - z1[1:(length(z1) - 1)]
  z2 <- c(0, z2)
  
  start <- zf[z2 == 1]
  end <- zf[z2 == -1]
  
  # set low and hi f to flim/bp if not detected
  if(!is.null(bp)) { 
    min.start <- ifelse(length(start) == 0 || is.infinite(min(start)), yes = bp[1], no = min(start))
    max.end <- ifelse(length(end) == 0 || is.infinite(min(end)), yes = bp[2], no = max(end))
    
    if(!is.null(min.range) && max.end - min.start < min.range) max.end <- bp[2]
  } else {
    min.start <- ifelse(length(start) == 0 || is.infinite(min(start)), yes = flim[1], no = min(start))
    max.end <- ifelse(length(end) == 0 || is.infinite(min(end)), yes = flim[2], no = max(end))
    
    if(!is.null(min.range) && max.end - min.start < min.range) max.end <- flim[2]
  }
  
  rl <- list(frange = data.frame(low.f = min.start, high.f = max.end), af.mat = cbind(z, zf))
  # return low and high freq
  return(rl)
}

