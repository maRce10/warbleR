#' Extract the dominant frequency values as a time series
#' 
#' \code{dfts} extracts the dominant frequency values as a time series.
#' of signals selected by \code{\link{manualoc}} or \code{\link{autodetec}}.
#' @usage dfts(X, wl = 512, wl.freq = 512, length.out = 20, wn = "hanning", ovlp = 70,
#' bp = c(0, 22), threshold = 15, threshold.time = NULL, threshold.freq = NULL, 
#' img = TRUE, parallel = 1, path = NULL, img.suffix = "dfts", pb = TRUE,
#' clip.edges = FALSE, leglab = "dfts", frange.detec = FALSE, fsmooth = 0.1,
#'  raw.contour = FALSE, track.harm = FALSE, adjust.wl = FALSE, ...)
#' @param  X 'selection.table' object or data frame with results containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param wl.freq A numeric vector of length 1 specifying the window length of the spectrogram
#' for measurements on the frecuency spectrum. Default is 512. Higher values would provide 
#' more accurate measurements.
#' @param length.out A numeric vector of length 1 giving the number of measurements of dominant 
#' frequency desired (the length of the time series).
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70. 
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param threshold amplitude threshold (\%) for dominant frequency detection. Default is 15. Note that amplitude 
#' threshold for time and frequency domains can be defined independently. See "threshold.time" and "threshold.freq" 
#' arguments. 
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
#' @param threshold.time amplitude threshold (\%) for the time domain. Use for dominant frequency detection. If \code{NULL} (default) then the 'threshold' value is used.
#' @param threshold.freq amplitude threshold (\%) for the frequency domain. Use for frequency range detection from the spectrum (see 'frange.detec'). If \code{NULL} (default) then the
#'  'threshold' value is used.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located.
#' @param img.suffix A character vector of length 1 with a sufix (label) to add at the end of the names of 
#' image files.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param clip.edges Logical argument to control whether edges (start or end of signal) in
#' which amplitude values above the threshold were not detected will be removed. If 
#' \code{TRUE} this edges will be excluded and signal contour will be calculated on the
#' remainging values. Default is \code{FALSE}. 
#' @param leglab A character vector of length 1 or 2 containing the label(s) of the frequency contour legend 
#' in the output image.
#' @param frange.detec Logical. Controls whether frequency range of signal is automatically 
#' detected  using the \code{\link{frange.detec}} function. If so, the range is used as the 
#' bandpass filter (overwriting 'bp' argument). Default is \code{FALSE}.
#' @param fsmooth A numeric vector of length 1 to smooth the frequency spectrum with a mean
#'  sliding window (in kHz) used for frequency range detection (when \code{frange.detec = TRUE}). This help to average amplitude "hills" to minimize the effect of
#'  amplitude modulation. Default is 0.1. 
#' @param raw.contour Logical. If \code{TRUE} then a list with the original contours 
#'  (i.e. without interpolating values to make all contours of equal length) is returned.
#' @param track.harm Logical. If true warbleR's \code{\link{track_harm}} function is 
#' used to track frequency contours. Otherwise seewave's \code{\link[seewave]{dfreq}} is used by default. 
#' @param adjust.wl Logical. If \code{TRUE} the 'wl' is reset to be equal at the 
#' number of samples in a selections if the samples are less than 'wl'. Default is \code{FALSE}.
#' @param ... Additional arguments to be passed to \code{\link{trackfreqs}}.
#' @return If \code{raw.contour = TRUE} (default) a data frame with the dominant frequency values measured across the signals.  Otherwise, a list with the raw frequency detections (i.e. without interpolating values to make all contours of equal length) is returned. If img is 
#' \code{TRUE} it also produces image files with the spectrograms of the signals listed in the 
#' input data frame showing the location of the dominant frequencies 
#' (see \code{\link{trackfreqs}} description for more details).
#' @family spectrogram creators
#' @seealso \code{\link{sig2noise}}, \code{\link{trackfreqs}}, \code{\link{sp.en.ts}}, \code{\link{ffts}}, \code{\link{ffDTW}}, \code{\link{dfDTW}}
#' @export
#' @name dfts
#' @details This function extracts the dominant frequency values as a time series. 
#' The function uses the \code{\link[stats]{approx}} function to interpolate values between dominant frequency 
#' measures. If there are no frequencies above the amplitude theshold at the begining or end 
#'  of the signals then NAs will be generated. On the other hand, if there are no frequencies 
#'  above the amplitude theshold in between signal segments in which amplitude was 
#'  detected then the values of this adjacent segments will be interpolated 
#'  to fill out the missing values (e.g. no NAs in between detected amplitude segments). 
#' 
#' @examples{
#' # set the temp directory
#' # setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2","selec.table"))
#' writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
#' writeWave(Phae.long1, "Phae.long1.wav")
#' 
#' # run function 
#' dfts(X = selec.table, length.out = 30, flim = c(1, 12), bp = c(2, 9), wl = 300)
#' 
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on march-12-2018 (MAS)

dfts <-  function(X, wl = 512, wl.freq = 512, length.out = 20, wn = "hanning", ovlp = 70, 
                  bp = c(0, 22), threshold = 15, threshold.time = NULL, threshold.freq = NULL,
                  img = TRUE, parallel = 1,
                  path = NULL, img.suffix = "dfts", pb = TRUE, clip.edges = FALSE, leglab = "dfts", frange.detec = FALSE, fsmooth = 0.1, raw.contour = FALSE, 
                  track.harm = FALSE, adjust.wl = FALSE, ...){     
  
  # reset working directory and default parameters
  wd <- getwd()
  on.exit(setwd(wd))
  op.dig <- options(digits = 5)
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type))
  
  #check path to working directory
  if(is.null(path)) path <- getwd() else {if(!file.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  #if X is not a data frame
  if(!class(X) %in% c("data.frame", "selection.table")) stop("X is not of a class 'data.frame' or 'selection table")
  
  if(!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  
  #if there are NAs in start or end stop
  if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if(all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'end' and 'selec' must be numeric")
  
  #if any start higher than end stop
  if(any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))  
  
  #if any selections longer than 20 secs stop
  if(any(X$end - X$start>20)) stop(paste(length(which(X$end - X$start>20)), "selection(s) longer than 20 sec"))  
  
  #if bp is not vector or length!=2 stop
  if(!is.null(bp)) {if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}}
  
  # If length.out is not numeric
  if(!is.numeric(length.out)) stop("'length.out' must be a numeric vector of length 1") 
  if(any(!(length.out %% 1 == 0),length.out < 1)) stop("'length.out' should be a positive integer")
  
  # threshold adjustment
  if(is.null(threshold.time)) threshold.time <- threshold
  if(is.null(threshold.freq)) threshold.freq <- threshold
  
  #return warning if not all sound files were found
  recs.wd <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files)) & pb) 
    cat(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% recs.wd) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else 
    X <- X[d, ]
  
  #if parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  if(pb) if(img) cat("Creating spectrograms overlaid with dominant frequency measurements:") else
    cat("measuring dominant frequency:") 
  
  dftsFUN <- function(X, i, bp, wl, threshold.time, threshold.freq, fsmooth, wl.freq, frange.dtc, raw.contour, track.harm, adjust.wl){
    
    # Read sound files to get sample rate and length
    r <- tuneR::readWave(as.character(X$sound.files[i]), header = TRUE)
    f <- r$sample.rate
    
    #in case bp its higher than can be due to sampling rate
    b <- bp 
    if(!is.null(b)) {if(b[2] > ceiling(f/2000) - 1) b[2] <- ceiling(f/2000) - 1 
    b <- b * 1000}
    
    r <- tuneR::readWave(as.character(X$sound.files[i]), from = X$start[i], to = X$end[i], units = "seconds")
    
    if(frange.dtc){
      frng <- frd_wrblr_int(wave = r, wl = wl.freq, fsmooth = fsmooth, threshold = threshold.freq, wn = wn, flim = c(0, 22), bp = b/ 1000, ovlp = ovlp)
    
    if(!all(is.na(frng$frange))) b <- as.numeric(frng$frange) * 1000 }
    
    # calculate dominant frequency at each time point     
    dfrq1 <- track_harm(wave = r, f = f, wl = wl, plot = FALSE, ovlp = ovlp, bandpass = b, fftw = TRUE,
                             threshold = threshold, dfrq = !track.harm, adjust.wl = adjust.wl)
    
        dfrq <- dfrq1[!is.na(dfrq1[,2]), ]
        if(nrow(dfrq1) == 1 & !is.matrix(dfrq)) dfrq <- as.matrix(t(dfrq))
        
        dfrq[dfrq[,2] < b[1]/1000, ] <- NA
        if(nrow(dfrq1) == 1 & !is.matrix(dfrq)) dfrq <- as.matrix(t(dfrq))
        
    
    if(!raw.contour){ 
     if(nrow(dfrq) < 2) {apdom <- list()
    apdom$x <- dfrq1[, 1]
    apdom$y <- rep(NA, length.out)
    apdom1 <- apdom
    
    } else {
      if(!clip.edges) {        
        apdom <- approx(dfrq[,1], dfrq[,2], xout = seq(from = dfrq1[1, 1], 
                                                                to = dfrq1[nrow(dfrq1), 1], length.out = length.out),
                               method = "linear")
      apdom1 <- apdom
      } else 
        {
        apdom <- approx(dfrq[,1], dfrq[,2], 
                        xout = seq(from = dfrq[1, 1],  to = dfrq[nrow(dfrq), 1], 
                                   length.out = length.out), method = "linear")
        
        #fix for ploting with trackfreqs
        dfrq1[,2][is.na(dfrq1[,2])] <- 0
        
        #calculate time at start and end with no amplitude detected (duration of clipped edges)
        durend1 <- suppressWarnings(diff(range(dfrq1[,1][rev(cumsum(rev(dfrq1[,2])) == 0)])))
        durend <- durend1
        if(is.infinite(durend) | is.na(durend)) durend <- 0
        
        durst1 <- suppressWarnings(diff(range(dfrq1[,1][cumsum(dfrq1[,2]) == 0])))   
        durst <- durst1
        if(is.infinite(durst) | is.na(durst)) durst <- 0
        
        by.dur <- mean(diff(apdom$x))
        clipst <- length(seq(from = 0, to = durst, by = by.dur))
        clipend <- length(seq(from = 0, to = durend, by = by.dur))
        
        apdom1 <- apdom
        apdom1$y <- c(rep(NA, clipst) ,apdom$y, rep(NA, clipend))
        
        if(is.infinite(durst1) | is.na(durst1)) apdom1$y <- apdom1$y[-1]
        if(is.infinite(durend1) | is.na(durend1)) apdom1$y <- apdom1$y[-length(apdom1$y)]
      } 
    }
      cstm.cntr <- data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(apdom1$y))
    } else
    {
      dfrq <- cbind(dfrq, X$start[i] + dfrq[, 1])
      colnames(dfrq) <- c("relative.time", "frequency", "absolute.time")
      dfrq <- dfrq[, c(3, 1, 2)]
      
      cstm.cntr <- list(dfrq)}

    if (img)  
    {
      
      trackfreqs(X[i,], wl = wl, wl.freq = wl.freq, osci = FALSE, leglab = leglab, pb = FALSE, wn = wn, threshold.time = threshold.time, threshold.freq = threshold.freq, bp = bp, 
                 parallel = 1, path = path, img.suffix = img.suffix, ovlp = ovlp,
                 custom.contour = cstm.cntr, xl = ifelse(frange.dtc, 1.8, 1), fsmooth = fsmooth, frange.detec = frange.dtc, ...)
      } 
    if(!raw.contour) return(apdom$y)  else return(dfrq)  
  } 
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  lst <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(i) 
  { 
 dftsFUN(X, i, bp, wl, threshold.time, threshold.freq, fsmooth, wl.freq, frange.dtc = frange.detec, raw.contour, track.harm, adjust.wl)
  }) 
  
  if(!raw.contour)
{  df <- data.frame(sound.files = X$sound.files, selec = X$selec, (as.data.frame(matrix(unlist(lst),nrow = length(X$sound.files), byrow = TRUE))))
    colnames(df)[3:ncol(df)]<-paste("dfreq",1:(ncol(df)-2),sep = "-")
            
    df[ ,3:ncol(df)] <- round(df[ ,3:ncol(df)], 3)
         return(df)}  else
         {
          names(lst) <- paste(X$sound.files, X$selec, sep = "-") 
         return(lst)
          }
    }
