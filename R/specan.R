#' Measure acoustic parameters in batches of sound files
#'
#' \code{specan} measures 22 acoustic parameters on acoustic signals for which the start and end times 
#' are provided. 
#' @usage specan(X, bp = c(0,22), wl = 512, threshold = 15, parallel = 1, fast = TRUE, path = NULL)
#' @param X Data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input data frame.
#' @param bp Numeric vector of length 2 giving the lower and upper limits of the 
#' frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param wl A numeric vector of length 1 specifying the spectrogram window length. Default is 512.
#' @param threshold amplitude threshold (\%) for fundamental frequency and 
#'   dominant frequency detection. Default is 15.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' For windows OS the \code{parallelsugar} package should be installed. 
#' @param fast Logical. If \code{TRUE} (default) then the peakf acoustic parameter (see below) is not computed, which 
#' substantially increases performance (~9 times faster).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @return Data frame with the following acoustic parameters: 
#' \itemize{
#'    \item \code{duration}: length of signal
#'    \item \code{meanfreq}: mean frequency (in kHz)
#'    \item \code{sd}: standard deviation of frequency 
#'    \item \code{median}: median frequency (in kHz) 
#'    \item \code{Q25}: first quantile (in kHz) 
#'    \item \code{Q75}: third quantile (in kHz) 
#'    \item \code{IQR}: interquantile range (in kHz) 
#'    \item \code{skew}: skewness (see note in \code{\link[seewave]{specprop}} description) 
#'    \item \code{kurt}:  kurtosis (see note in \code{\link[seewave]{specprop}} description)
#'    \item \code{sp.ent}: spectral entropy 
#'    \item \code{sfm}: spectral flatness 
#'    \item \code{mode}: mode frequency
#'    \item \code{centroid}: frequency centroid (see \code{\link[seewave]{specprop}})
#'    \item \code{peakf}: peak frequency (frequency with highest energy) 
#'    \item \code{meanfun}: average of fundamental frequency measured across acoustic signal 
#'    \item \code{minfun}: minimum fundamental frequency measured across acoustic signal 
#'    \item \code{maxfun}: maximum fundamental frequency measured across acoustic signal 
#'    \item \code{meandom}: average of dominant frequency measured across acoustic signal 
#'    \item \code{mindom}: minimum of dominant frequency measured across acoustic signal
#'    \item \code{maxdom}: maximum of dominant frequency measured across acoustic signal 
#'    \item \code{dfrange}: range of dominant frequency measured across acoustic signal 
#'    \item \code{modindx}: modulation index. Calculated as the accumulated absolute 
#'      difference between adjacent measurements of fundamental frequencies divided
#'      by the frequency range
#' }
#' @export
#' @name specan
#' @details The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used 
#'  directly without any additional modification. The function measures 22 acoustic parameters on 
#'  each selection in the data frame. Most parameters are produced internally by 
#'  \code{\link[seewave]{specprop}}, \code{\link[seewave]{fpeaks}}, \code{\link[seewave]{fund}},
#'  and \code{\link[seewave]{dfreq}} from the package seewave. 

#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "manualoc.df"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' a <- specan(X = manualoc.df, bp = c(0, 22))
#' 
#' # using a diferent threshold
#' a <- specan(X = manualoc.df, bp = c(0, 22), threshold = 20)
#' # View(a)
#' 
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) and Grace Smith Vidaurre
#last modification on jul-5-2016 (MAS)

specan <- function(X, bp = c(0,22), wl = 512, threshold = 15, parallel = 1, fast = TRUE, path = NULL){
  
  #check path to working directory
  if(!is.null(path))
  {if(class(try(setwd(path), silent = T)) == "try-error") stop("'path' provided does not exist") else setwd(path)} #set working directory
  
  #if X is not a data frame
  if(!class(X) == "data.frame") stop("X is not a data frame")
  
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
  options( show.error.messages = TRUE)
  
  #if bp is not vector or length!=2 stop
  if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}
  
  #return warning if not all sound files were found
  fs <- list.files(pattern = ".wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
    message(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% fs) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
    X <- X[d, ]
  }
  
  # If parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  #create function to run within Xapply functions downstream
  spFUN <- function(i, X, bp, wl, threshold) { 
    r <- tuneR::readWave(as.character(X$sound.files[i]), from = X$start[i], to = X$end[i], units = "seconds") 
    
    b<- bp #in case bp its higher than can be due to sampling rate
    if(b[2] > ceiling(r@samp.rate/2000) - 1) b[2] <- ceiling(r@samp.rate/2000) - 1 
    
    
    #frequency spectrum analysis
    songspec <- seewave::spec(r, f = r@samp.rate, plot = FALSE)
    analysis <- seewave::specprop(songspec, f = r@samp.rate, flim = b, plot = FALSE)
    
    #save parameters
    meanfreq <- analysis$mean/1000
    sd <- analysis$sd/1000
    median <- analysis$median/1000
    Q25 <- analysis$Q25/1000
    Q75 <- analysis$Q75/1000
    IQR <- analysis$IQR/1000
    skew <- analysis$skewness
    kurt <- analysis$kurtosis
    sp.ent <- analysis$sh
    sfm <- analysis$sfm
    mode <- analysis$mode/1000
    centroid <- analysis$cent/1000
    
    #Frequency with amplitude peaks 
    if(!fast) #only if fast is TRUE
      peakf <- seewave::fpeaks(songspec, f = r@samp.rate, wl = 512, nmax = 3, plot = FALSE)[1, 1] else peakf <- NA
    
    #Fundamental frequency parameters
    ff <- seewave::fund(r, f = r@samp.rate, ovlp = 50, threshold = threshold, 
                        fmax = b[2] * 1000, plot = FALSE)[, 2]
    meanfun<-mean(ff, na.rm = TRUE)
    minfun<-min(ff, na.rm = TRUE)
    maxfun<-max(ff, na.rm = TRUE)
    
    #Dominant frecuency parameters
    y <- seewave::dfreq(r, f = r@samp.rate, wl = wl, ovlp = 0, plot = FALSE, threshold = threshold, bandpass = b * 1000, fftw = TRUE)[, 2]
    meandom <- mean(y, na.rm = TRUE)
    mindom <- min(y, na.rm = TRUE)
    maxdom <- max(y, na.rm = TRUE)
    dfrange <- (maxdom - mindom)
    duration <- (X$end[i] - X$start[i])
    
    #modulation index calculation
    changes <- vector()
    for(j in which(!is.na(y))){
      change <- abs(y[j] - y[j + 1])
      changes <- append(changes, change)
    }
    if(mindom==maxdom) modindx<-0 else modindx <- mean(changes, na.rm = TRUE)/dfrange
    
    #save results
    if(fast) return(data.frame(sound.files = X$sound.files[i], selec = X$selec[i], duration, meanfreq, sd, median, Q25, Q75, IQR, skew, kurt, sp.ent, sfm, mode, 
                               centroid, meanfun, minfun, maxfun, meandom, mindom, maxdom, dfrange, modindx)) else
                                 return(data.frame(sound.files = X$sound.files[i], selec = X$selec[i], duration, meanfreq, sd, median, Q25, Q75, IQR, skew, kurt, sp.ent, sfm, mode, 
                                                   centroid, peakf, meanfun, minfun, maxfun, meandom, mindom, maxdom, dfrange, modindx))
  }
  
  # Run parallel in windows
  if(parallel > 1) {if(Sys.info()[1] == "Windows") {
    
    # i <- NULL #only to avoid non-declared objects
    
    cl <- parallel::makeCluster(parallel)
    
    doParallel::registerDoParallel(cl)
    
    sp <- parallel::parLapply(cl, 1:nrow(X), function(i)
    {
      spFUN(X = X, i = i, bp = bp, wl = wl, threshold = threshold)
    })
    
    parallel::stopCluster(cl)
    
    
  } else {    # Run parallel in other operating systems
    
    sp <- parallel::mclapply(1:nrow(X), function (i) {
      spFUN(X = X, i = i, bp = bp, wl = wl, threshold = threshold)
    })
    
  }
  }
  else {sp <- pbapply::pblapply(1:nrow(X), function(i) spFUN(X, i = i, bp = bp, wl = wl, threshold = threshold))             

  }
  sp <- do.call(rbind, sp)
  
  row.names(sp) <- 1:nrow(sp)
  
  return(sp)
}
