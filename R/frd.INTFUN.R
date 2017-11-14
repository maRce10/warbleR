#internal warbleR function, not to be called by users. Detects frequency range.

frd.INTFUN <- function(wave, wl = 512, fsmooth = 0.1, threshold = 10, wn = "hanning", flim = c(0, 22), bp = NULL, ovlp = 50)
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
  
  # add 0 to get same length than z
  z2 <- c(0, z2)
  
  #determine start and end of amplitude hills  
  strt <- zf[z2 == 1]
  nd <- zf[z2 == -1]

    #add NAs when some ends or starts where not found
    if(length(strt) != length(nd))
    {if(z1[1] == 0) nd <- c(nd, NA) else strt <- c(NA, strt)}
  
  if(length(strt) == 1){
  if(z1[1] == 1 & z1[length(z1)] == 1  & strt > nd){    
    strt <- c(NA, strt)
    nd <- c(nd , NA)
  }
}  
    # substract half a step to calculate mid point between the 2 freq windows in which the theshold has passed
    nd <- nd - (step / 2)
    strt <- strt - (step / 2)
    
  meanpeakf <- zf[which.max(z)] + (step / 2)
  
  options(warn = -1)
  min.strt <- ifelse(length(strt) == 1, strt, min(strt, na.rm = TRUE))
  max.nd <- ifelse(length(nd) == 1, nd, max(nd, na.rm = TRUE))
  
  if(!any(is.na(c(min.strt, max.nd)))) {
    if(min.strt > max.nd){
    min.strt <- NA
    max.nd <- NA
  }
    }

  rl <- list(frange = data.frame(bottom.freq = min.strt, top.freq = max.nd), af.mat = cbind(z, zf), meanpeakf = meanpeakf, detections = cbind(start.freq = strt, end.freq = nd))
  
  # return low and high freq
  return(rl)
}

