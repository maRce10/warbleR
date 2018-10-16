#' Measure acoustic parameters in batches of sound files
#'
#' \code{specan} measures acoustic parameters on acoustic signals for which the start and end times 
#' are provided. 
#' @usage specan(X, bp = "frange", wl = 512, wl.freq = NULL, threshold = 15,
#'  parallel = 1, fast = TRUE, path = NULL, pb = TRUE, ovlp = 50,
#' wn = "hanning", fsmooth = 0.1, harmonicity = FALSE, nharmonics = 3, ...)
#' @param X 'selection_table', 'extended_selection_table' or data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can
#' be used as the input data frame.
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz) or "frange" (default) to indicate that values in bottom.freq
#'   and top.freq columns will be used as bandpass limits.  Lower limit of
#'    bandpass filter is not applied to fundamental frequencies. 
#' @param wl A numeric vector of length 1 specifying the spectrogram window length. Default is 512. See 'wl.freq' for setting windows length independenlty in the frequency domain.
#' @param wl.freq A numeric vector of length 1 specifying the window length of the spectrogram
#' for measurements on the frecuency spectrum. Default is 512. Higher values would provide 
#' more accurate measurements. Note that this allows to increase measurement precision independently in the time and frequency domain. If \code{NULL} (default) then the 'wl' value is used. 
#' @param threshold amplitude threshold (\%) for fundamental frequency and 
#'   dominant frequency detection. Default is 15.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param fast Logical. If \code{TRUE} (default) then the peakf acoustic parameter (see below) is not computed, which 
#' substantially increases performance (~9 times faster). This argument will be removed in future version.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, used for fundamental frequency (using \code{\link[seewave]{fund}} or \code{\link[tuneR]{FF}}) and dominant frequency (using \code{\link[seewave]{dfreq}}). 
#'   Default is 50. 
#' @param wn Character vector of length 1 specifying window name. Default is hanning'. 
#' See function \code{\link[seewave]{ftwindow}} for more options.
#' @param fsmooth A numeric vector of length 1 to smooth the frequency spectrum with a mean
#'  sliding window (in kHz) used for mean peak frequency detection. This help to average 
#'  amplitude "hills" to minimize the effect of amplitude modulation. Default is 0.1.
#' @param harmonicity Logical. If \code{TRUE} harmonicity related parameters (fundamental frequency parameters [meanfun, minfun, maxfun], hn_freq, 
#' hn_width, harmonics and HNR) are measured. Note that measuring these parameters 
#' considerably increases computing time.    
#' @param nharmonics Numeric vector of length 1 setting the number of harmonics to analyze.
#' @param ... Additional parameters to be passed to \code{\link[soundgen]{analyze}}, which measures parameters related to harmonicity.
#' @return Data frame with 'sound.files' and 'selec' as in the input data frame, plus the following acoustic parameters: 
#' \itemize{
#'    \item \code{duration}: length of signal (in s)
#'    \item \code{meanfreq}: mean frequency. Weighted average of frequency by amplitude (in kHz)
#'    \item \code{sd}: standard deviation of frequency weighted by amplitude
#'    \item \code{freq.median}: median frequency. The frequency at which the signal is divided in two frequency
#'    intervals of equal energy (in kHz) 
#'    \item \code{freq.Q25}: first quartile frequency. The frequency at which the signal is divided in two 
#'    frequency intervals of 25\% and 75\% energy respectively (in kHz) 
#'    \item \code{freq.Q75}: third quartile frequency. The frequency at which the signal is divided in two
#'    frequency intervals of 75\% and 25\% energy respectively (in kHz) 
#'    \item \code{freq.IQR}: interquartile frequency range. Frequency range between 'freq.Q25' and 'freq.Q75' 
#'    (in kHz) 
#'    \item \code{time.median}: median time. The time at which the signal is divided in two time
#'    intervals of equal energy (in s) 
#'    \item \code{time.Q25}: first quartile time. The time at which the signal is divided in two
#'time intervals of 25\% and 75\% energy respectively (in s). See \code{\link[seewave]{acoustat}}
#'    \item \code{time.Q75}: third quartile time. The time at which the signal is divided in two
#'    time intervals of 75\% and 25\% energy respectively (in s). See \code{\link[seewave]{acoustat}}
#'    \item \code{time.IQR}: interquartile time range. Time range between 'time.Q25' and 'time.Q75' 
#'    (in s). See \code{\link[seewave]{acoustat}}
#'    \item \code{skew}: skewness. Asymmetry of the spectrum (see note in \code{\link[seewave]{specprop}} description) 
#'    \item \code{kurt}: kurtosis. Peakedness of the spectrum (see note in \code{\link[seewave]{specprop}} description)
#'    \item \code{sp.ent}: spectral entropy. Energy distribution of the frequency spectrum. Pure tone ~ 0; 
#'    noisy ~ 1. See \code{\link[seewave]{sh}}
#'    \item \code{time.ent}: time entropy. Energy distribution on the time envelope. Pure tone ~ 0; 
#'    noisy ~ 1. See \code{\link[seewave]{th}}
#'    \item \code{entropy}: spectral entropy. Product of time and spectral entropy \code{sp.ent * time.ent}. 
#'    See \code{\link[seewave]{H}}
#'    \item \code{sfm}: spectral flatness. Similar to sp.ent (Pure tone ~ 0; 
#'    noisy ~ 1). See \code{\link[seewave]{sfm}}
#'    \item \code{meanfun}: average of fundamental frequency measured across the acoustic signal 
#'    \item \code{minfun}: minimum fundamental frequency measured across the acoustic signal 
#'    \item \code{maxfun}: maximum fundamental frequency measured across the acoustic signal 
#'    \item \code{meandom}: average of dominant frequency measured across the acoustic signal 
#'    \item \code{mindom}: minimum of dominant frequency measured across the acoustic signal
#'    \item \code{maxdom}: maximum of dominant frequency measured across the acoustic signal 
#'    \item \code{dfrange}: range of dominant frequency measured across the acoustic signal 
#'    \item \code{modindx}: modulation index. Calculated as the cumulative absolute
#'      difference between adjacent measurements of dominant frequencies divided
#'      by the dominant frequency range. 1 means the signals is not modulated. 
#'    \item \code{startdom}:  dominant frequency measurement at the start of the signal 
#'    \item \code{enddom}: dominant frequency measurement at the end of the signal 
#'    \item \code{dfslope}: slope of the change in dominant frequency through time ((enddom-startdom)/duration). Units are kHz/s.  
#'    \item \code{peakf}: peak frequency. Frequency with the highest energy. This 
#'    parameter can take a considerable amount of time to measure. It's only 
#'    generated if \code{fast = FALSE}. It provides a more accurate measure of peak
#'    frequency than 'meanpeakf' but can be more easily affected by background noise.
#'    \item \code{meanpeakf}: mean peak frequency. Frequency with highest energy from the 
#'    mean frequency spectrum (see \code{\link[seewave]{meanspec}}). Typically more consistent than peakf.
#'    \item \code{hn_freq}: mean frequency of the 'n' upper harmonics (kHz) (see \code{\link[soundgen]{analyze}}). 
#'    Number of harmonics is defined with the argument 'nharmonics'.
#'    \item \code{hn_width}: mean bandwidth of the 'n' upper harmonics (kHz) (see \code{\link[soundgen]{analyze}}). Number of harmonics is defined with the argument 'nharmonics'.  
#'    \item \code{harmonics}: the amount of energy in upper harmonics, namely the 
#'    ratio of total spectral power above 1.25 x F0 to the total spectral power 
#'    below 1.25 x F0 (dB) (see \code{\link[soundgen]{analyze}}). Number of 
#'    harmonics is defined with the argument 'nharmonics'.
#'    \item \code{HNR}: harmonics-to-noise ratio (dB). A measure of the harmonic content generated by \code{\link[soundgen]{getPitchAutocor}}.
#' }
#' @export
#' @name specan
#' @details The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used 
#'  directly without any additional modification. The function measures 29 acoustic parameters (if \code{fast = TRUE}) on 
#'  each selection in the data frame. Most parameters are produced internally by 
#'  \code{\link[seewave]{specprop}}, \code{\link[seewave]{fpeaks}}, \code{\link[seewave]{fund}},
#'  and \code{\link[seewave]{dfreq}} from the package seewave and \code{\link[soundgen]{analyze}} 
#'  from the package soundgen. NAs are produced for fundamental and dominant 
#'  frequency measures when there are no amplitude values above the threshold. 
#'  Additional parameters can be provided to the internal function \code{\link[soundgen]{analyze}}, which measures parameters related to harmonicity.
#' @examples
#' {
#' # First set temporary folder
# setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' 
#' # measure acoustic parameters
#' sp_param <- specan(X = selec.table[1:8,], pb = FALSE)
#' 
#' # measuring peakf
#' sp_param <- specan(X = selec.table[1:8,], pb = FALSE, fast = FALSE)
#' 
#' # measuring harmonic-related parameters using progress bar
#' sp_param <- specan(X = selec.table[1:8,], harmonicity = TRUE)
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) and Grace Smith Vidaurre
#last modification on mar-13-2018 (MAS)

specan <- function(X, bp = "frange", wl = 512, wl.freq = NULL, threshold = 15,
                   parallel = 1, fast = TRUE, path = NULL, pb = TRUE, ovlp = 50, 
                   wn = "hanning", fsmooth = 0.1, harmonicity = FALSE, nharmonics = 3, ...){
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(specan)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
  # rename path for sound files
  names(opt.argms)[names(opt.argms) == "wav.path"] <- "path"
  
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
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else {if (!dir.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  if (!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if (all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'start' and 'end' must be numeric")
  
  #if any start higher than end stop
  if (any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))  
  
  #if any selections longer than 20 secs warning
  if (any(X$end - X$start>20)) warning(paste(length(which(X$end - X$start>20)), "selection(s) longer than 20 sec"))
  
  # bp checking
  if (bp[1] != "frange")
  {if (!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if (!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")} 
  } else
  {if (!any(names(X) == "bottom.freq") & !any(names(X) == "top.freq")) stop("'bp' = frange requires bottom.freq and top.freq columns in X")
    if (any(is.na(c(X$bottom.freq, X$top.freq)))) stop("NAs found in bottom.freq and/or top.freq") 
    if (any(c(X$bottom.freq, X$top.freq) < 0)) stop("Negative values found in bottom.freq and/or top.freq") 
    if (any(X$top.freq - X$bottom.freq < 0)) stop("top.freq should be higher than bottom.freq")
  }
  
  if (!is_extended_selection_table(X)){
  #return warning if not all sound files were found
  fs <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
    write(file = "", x = paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% fs) 
  if (length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
    X <- X[d, ]
  }
  }
  
  # wl adjustment
  if (is.null(wl.freq)) wl.freq <- wl
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")

  #create function to run within Xapply functions downstream
  spFUN <- function(i, X, bp, wl, threshold) { 
    
    # read wave object
    r <- read_wave(X = X, index = i)
    
    if (bp[1] == "frange") b <- c(X$bottom.freq[i], X$top.freq[i]) else b <- bp

     #in case bp its higher than can be due to sampling rate
    if (b[2] > ceiling(r@samp.rate/2000) - 1) b[2] <- ceiling(r@samp.rate/2000) - 1 
    
    # add a bit above and below to ensure range limits are included
    bpfr <- b
    bpfr <- bpfr + c(-0.2, 0.2)  
    if (bpfr[1] < 0) bpfr[1] <- 0
    if (bpfr[2] > ceiling(r@samp.rate/2000) - 1) bpfr[2] <- ceiling(r@samp.rate/2000) - 1 
  
    # freq range to measure peak freq  
    # wl is adjusted when very short signals
    frng <- frd_wrblr_int(wave = r, wl = wl.freq, fsmooth = fsmooth, threshold = threshold, wn = wn, flim = b, bp = bpfr, ovlp = ovlp)
  
    # soungen measurements
    if (harmonicity)
    {
    sg.param <- soundgen::analyze(x = as.numeric(r@left), samplingRate = r@samp.rate, silence = threshold / 100, overlap = ovlp, windowLength = wl / r@samp.rate * 1000, plot = FALSE, wn = wn, pitchCeiling = b[2] * 1000, cutFreq = b[2] * 1000, nFormants = nharmonics)
    
  names(sg.param) <- gsub("^f", "h", names(sg.param))
    
  if (all(is.na(sg.param$pitch))) 
    ff <- c(as.matrix(sg.param[, grep("pitch", names(sg.param))])) else ff <- sg.param$pitch

  sg.param[, grep("freq$|_width$", names(sg.param))] <- sg.param[, grep("freq$|_width$", names(sg.param))] / 1000
  
  sg.param <- as.data.frame(t(apply(sg.param[, grep("harmonics|HNR|_freq$|_width$", names(sg.param))], 2, mean, na.rm = TRUE)))
    
  ff <- ff[!is.na(ff)]
  
  if (length(ff) > 0)
  { 
    meanfun<-mean(ff, na.rm = TRUE) / 1000
    minfun<-min(ff, na.rm = TRUE) / 1000
    maxfun<-max(ff, na.rm = TRUE) / 1000} else meanfun <- minfun <- maxfun <- NA
  
  fun.pars <- data.frame(t(c(meanfun = meanfun, minfun = minfun, maxfun = maxfun)))
  }
      
    #frequency spectrum analysis
    songspec <- seewave::spec(r, f = r@samp.rate, plot = FALSE, wl = wl.freq, wn = wn, flim = b)
    analysis <- seewave::specprop(songspec, f = r@samp.rate, flim = b, plot = FALSE)

    #from seewave's acoustat
    m <- sspectro(r, f = r@samp.rate, wl = ifelse(wl >= length(r@left), length(r@left) - 1, wl), ovlp = ovlp, wn = wn)
    if (!is.matrix(m)) m <- as.matrix(m)
    
    fl <- b * nrow(m) * 2000/r@samp.rate
    m <- m[(fl[1]:fl[2]) + 1, ]
    if (is.vector(m)) m <- t(as.matrix(m))
    time <- seq(0, length(r)/r@samp.rate, length.out = ncol(m))
    t.cont <- apply(m, MARGIN = 2, FUN = sum)
    t.cont <- t.cont/sum(t.cont)
    t.cont.cum <- cumsum(t.cont)
    t.quartiles <- sapply(c(0.25, 0.5, 0.75), function(x) time[length(t.cont.cum[t.cont.cum <= 
                                                                                       x]) + 1])
    
    #save parameters
    meanfreq <- analysis$mean/1000
    sd <- analysis$sd/1000
    freq.median <- analysis$median/1000
    freq.Q25 <- analysis$Q25/1000
    freq.Q75 <- analysis$Q75/1000
    freq.IQR <- analysis$IQR/1000
    time.ent <- th(cbind(time, t.cont))
    time.median <- t.quartiles[2]
    time.Q25 <- t.quartiles[1]
    time.Q75 <- t.quartiles[3]
    time.IQR <- t.quartiles[3] - t.quartiles[1]
    skew <- analysis$skewness
    kurt <- analysis$kurtosis
    sp.ent <- analysis$sh
    entropy <- sp.ent * time.ent
    sfm <- analysis$sfm
    
    options(warn = -1)
    
    #Frequency with amplitude peaks 
    if (!fast) #only if fast is TRUE
      peakf <- seewave::fpeaks(songspec, f = r@samp.rate, wl = wl.freq, nmax = 3, plot = FALSE)[1, 1] else peakf <- NA
    
    #Fundamental frequency parameters
  #   if (ff.method == "seewave")
  #   ff <- seewave::fund(r, f = r@samp.rate, ovlp = ovlp, threshold = threshold, 
  #                       fmax = b[2] * 1000, plot = FALSE)[, 2] else {
  #                       if (!any(methods::slotNames(r) == "stereo")) r <- Wave(r) 
  #     ff <- tuneR::FF(tuneR::periodogram(mono(r, "left"), width = wl, 
  #       overlap = wl*ovlp/100), peakheight = (100 - threshold) / 100)/1000
  #                                    }
  
    #Dominant frecuency parameters
    y <- seewave::dfreq(r, f = r@samp.rate, wl = ifelse(wl >= length(r@left), length(r@left) - 1, wl), ovlp = ovlp, plot = FALSE, threshold = threshold, bandpass = b * 1000, fftw = TRUE)[, 2]
    
    #remove NAs
    y <- y[!is.na(y)]
    
    #remove values below and above bandpass
    y <- y[y >= b[1] & y <= b[2] & y != 0]
    
    if (length(y) > 0)
    {
    meandom <- mean(y, na.rm = TRUE)
    mindom <- min(y, na.rm = TRUE)
    maxdom <- max(y, na.rm = TRUE)
    dfrange <- maxdom - mindom
    startdom <- y[1]
    enddom <- y[length(y)]
    if (length(y) > 1 & dfrange != 0)
    modindx <- sum(sapply(2:length(y), function(j) abs(y[j] - y[j - 1])))/dfrange else modindx <- 1 
    } else meandom <- mindom <- maxdom <- dfrange <- startdom <- enddom <- modindx <- NA
    
    duration <- (X$end[i] - X$start[i])
    if (!is.na(enddom) && !is.na(startdom))
    dfslope <- (enddom -startdom)/duration else dfslope <- NA
    meanpeakf <- frng$meanpeakf 
    
    #save results
    dfres <- data.frame(sound.files = X$sound.files[i], selec = X$selec[i], duration, meanfreq, sd, freq.median, freq.Q25, freq.Q75, freq.IQR, time.median, time.Q25, time.Q75, time.IQR, skew, kurt, sp.ent, time.ent, entropy, sfm, 
                        meandom, mindom, maxdom, dfrange, modindx, startdom, enddom, dfslope, meanpeakf)
    
    # add peak freq
    if (!fast) dfres$peakf <- peakf
    
    # add soundgen parameters
    if (harmonicity)
    dfres <- cbind(dfres, sg.param, fun.pars)
    
    # add low high freq
    if (bp[1] == "frange") {
      dfres$bottom.freq <- b[1]
     dfres$top.freq <- b[2]
     }
    
    return(dfres)
    
  }
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  sp <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(i) 
  { 
    spFUN(X = X, i = i, bp = bp, wl = wl, threshold = threshold)
  }) 
  
  # put results in a single data frame
  sp <- do.call(rbind, sp)
  
  row.names(sp) <- 1:nrow(sp)
  
  return(sp)
  }


##############################################################################################################
#' alternative name for \code{\link{specan}}
#'
#' @keywords internal
#' @details see \code{\link{specan}} for documentation. \code{\link{specan}} will be deprecated in future versions.
#' @export

spec_an <- specan
