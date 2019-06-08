#internal warbleR function, not to be called by users. Detects frequency range.

frd_wrblr_int <- function(wave, wl = 512, fsmooth = 0.1, threshold = 10, wn = "hanning", bp = NULL, ovlp = 50, dB.threshold = NULL)
{
  # get sampling rate
  f <-  wave@samp.rate
  
  if(wl >= length(wave@left))  wl <- length(wave@left) - 1 
  if (wl %% 2 != 0) wl <- wl - 1
  
  # mean spectrum
  if (is.null(dB.threshold)){
    
  spc <- meanspec(wave, plot = FALSE, wl = wl, f = f, wn = wn, ovlp = ovlp)
  
  # get frequency windows length for smoothing
  step <- wave@samp.rate/wl/1000
  
  # number of samples
  n <- nrow(spc)
  
  # smoothing parameter
  if (!is.null(fsmooth))
  {
    fsmooth <- fsmooth/step
    
    FWL <- fsmooth - 1
  
  # smooth 
  z <- apply(as.matrix(1:(n - FWL)), 1, function(y) sum(spc[y:(y + FWL), 2]))
  zf <- seq(min(spc[,1]), max(spc[,1]), length.out = length(z))
  } else 
  {
    z <- spc[ , 2]
    zf <- spc[ , 1]
  }
  
  #remove range outside bp
  if (!is.null(bp)) { 
     # if there are complete freq bins within freq range
    if (any(zf > bp[1] & zf < bp[2])) 
    fbins <- which(zf > bp[1] & zf < bp[2]) else # select the one that contains the freq range
    fbins <- which.max(ifelse(zf - bp[1] > 0, NA, zf - bp[1])):which.max(ifelse(zf - bp[2] > 0, NA, zf - bp[1]))
    
    z <- z[fbins]
    zf <- zf[fbins]
  }
  
  # make minimum amplitude 0
  z <- z - min(z)
  z[z < 0] <- 0
  
  # normalize amplitude from 0 to 1
  if(length(z) > 1)
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
    if (length(strt) != length(nd))
    {if (z1[1] == 0) nd <- c(nd, NA) else strt <- c(NA, strt)}
  
  if (length(strt) == 1){
  if (z1[1] == 1 & z1[length(z1)] == 1  & strt > nd){    
    strt <- c(NA, strt)
    nd <- c(nd , NA)
  }
}  
    # substract half a step to calculate mid point between the 2 freq windows in which the theshold has passed
    nd <- nd - (step / 2)
    strt <- strt - (step / 2)
    
  meanpeakf <- zf[which.max(z)] + (step / 2)
  } else {
  
  spc <- meanspec(wave, plot = FALSE, wl = wl, f = f, wn = wn, ovlp = ovlp, dB = "max0", dBref = 2*10e-5)
 
  # get frequency windows length for smoothing
  step <- wave@samp.rate/wl/1000
  
  # number of samples
  n <- nrow(spc)

  # smoothing parameter
  if (!is.null(fsmooth))
  {
    fsmooth <- fsmooth/step
    
    FWL <- fsmooth - 1
    
    # smooth 
    z <- apply(as.matrix(1:(n - FWL)), 1, function(y) sum(spc[y:(y + FWL), 2]))
    zf <- seq(min(spc[,1]), max(spc[,1]), length.out = length(z))
  
    z <- (max(spc[ , 2]) - min(spc[ , 2]))/(max(z) - min(z)) * (z - max(z))+max(spc[ , 2])
    
    } else 
  {
    z <- spc[ , 2]
    zf <- spc[ , 1]
  }
  
  if (!is.null(bp)) { 
    #remove range outsde bp
    z <- z[zf > bp[1] & zf < bp[2]]
    zf <- zf[zf > bp[1] & zf < bp[2]]
  }
  
  z1 <- rep(0, length(z))
  z1[z > max(z) - dB.threshold] <- 1 
  z2 <- z1[2:length(z1)] - z1[1:(length(z1) - 1)]
  
  # add 0 to get same length than z
  z2 <- c(0, z2)
  
  #determine start and end of amplitude hills  
  strt <- zf[z2 == 1]
  nd <- zf[z2 == -1]
  
  #add NAs when some ends or starts where not found
  if (length(strt) != length(nd))
  {if (z1[1] == 0) nd <- c(nd, NA) else strt <- c(NA, strt)}
  
  if (length(strt) == 1){
    if (z1[1] == 1 & z1[length(z1)] == 1  & strt > nd){    
      strt <- c(NA, strt)
      nd <- c(nd , NA)
    }
  }  
  
  # step <- mean(zf[-1] - zf[1:(length(zf) - 1)])
  
  # substract half a step to calculate mid point between the 2 freq windows in which the theshold has passed
  nd <- nd - (step / 2)
  strt <- strt - (step / 2)
  meanpeakf <- zf[which.max(z)] + (step / 2)

   }

  # fix range
  # only start lower than peakf
  strt <- strt[strt <= meanpeakf]
  
  # only ends higher than peakf
  nd <- nd[nd >= meanpeakf]
  
  # get freq range
  min.strt <- ifelse(length(strt) == 1, strt, strt[which.min(meanpeakf - strt)])
  max.nd <- ifelse(length(nd) == 1, nd, nd[which.min(nd - meanpeakf)])
  
  if (!any(is.na(c(min.strt, max.nd)))) {
    if (min.strt > max.nd){
      min.strt <- NA
      max.nd <- NA
    }
  }
  
  # force nd and strt the same length adding NAs
  if(length(nd) > length(strt)) strt <- c(strt, rep(NA, length(nd) - length(strt)))
  if(length(strt) > length(nd)) nd <- c(nd, rep(NA, length(strt) - length(nd)))
  
  # save everything in a list
  rl <- list(frange = data.frame(bottom.freq = min.strt, top.freq = max.nd), af.mat = cbind(z, zf), meanpeakf = meanpeakf, detections = cbind(start.freq = strt, end.freq = nd), threshold = ifelse(is.null(dB.threshold), threshold, max(z) - dB.threshold), type = ifelse(is.null(dB.threshold), "percentage", "dB"))
  
  # return rl list
  return(rl)
}

