#' Measure acoustic parameters in batches of sound files
#'
#' \code{specan} measures 22 acoustic parameters on acoustic signals for which the start and end times 
#' are provided. 
#' @usage specan(X, bp = c(0,22), wl = 512, threshold = 15)
#' @param X data frame with the following columns: 1) "start": start time of 
#'   selections, 2) "end": end time of selections, 3) "rec": name of the .wav 
#'  files, and 4) "sel": number of the selections. The ouptut of \code{\link{manualoc}} can 
#'  be used as the input data frame.
#' @param bp numeric vector of length 2 giving the lower and upper limits of the 
#' frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param wl A numeric vector of length 1 specifying the spectrogram window length. Default is 512.
#' @param threshold \% amplitude threshold for fundamental frequency and dominant frequency detection. 
#' Default is 15.
#' @return Data frame with the following acoustic parameters: 
#' \itemize{
#'    \item \code{duration}: length of signal
#'    \item \code{meanfreq}: mean frequency (in kHz)
#'    \item \code{sd}: standard deviation of frequency 
#'    \item \code{median}: median frequency (in kHz) 
#'    \item \code{Q25}: first quantile (in kHz) 
#'    \item \code{Q75}: third quantile (in kHz) 
#'    \item \code{IQR}: interquantile range (in kHz) 
#'    \item \code{skew}: skewness (see note in \code{\link[seewave]{specprop}} description from seewave package) 
#'    \item \code{kurt}:  kurtosis (see note in \code{\link[seewave]{specprop}} description from seewave package)
#'    \item \code{sp.ent}: spectral entropy 
#'    \item \code{sfm}: spectral flatness 
#'    \item \code{mode}: mode frequency
#'    \item \code{centroid}: centroid
#'    \item \code{peakf}: peak frequency (frequency with highest energy) 
#'    \item \code{meanfun}: average of fundamental frequency measured across acoustic signal 
#'    \item \code{minfun}: minimum fundamental frequency measured across acoustic signal 
#'    \item \code{maxfun}: maximum fundamental frequency measured across acoustic signal 
#'    \item \code{meandom}: average of dominant frequency measured across acoustic signal 
#'    \item \code{mindom}: minimum of dominant frequency measured across acoustic signal
#'    \item \code{maxdom}: maximum of dominant frequency measured across acoustic signal 
#'    \item \code{dfrange}: range of dominant frequency measured across acoustic signal 
#'    \item \code{modindx}: modulation index. Is calculated as the accumulated absolute 
#'      difference between adjacent measurements of fundamental frequencies divided
#'      by the frequency range
#' }
#' @export
#' @name specan
#' @details The output of the manualoc function can be used directly without any
#'   additional modification. The function measures 22 acoustic parameters on 
#'   each selection in the data frame. Most parameters are produced internally by 
#'   the \code{\link[seewave]{specprop}}, \code{\link[seewave]{fpeaks}}, \code{\link[seewave]{fund}},
#'   and \code{\link[seewave]{dfreq}} functions of the seewave package. 

#' @examples
#' \dontrun{
#' #First create empty folder
#' dir.create(file.path(getwd(),"temp"))
#' setwd(file.path(getwd(),"temp"))
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' data(manualoc.df)
#' 
#' a <- specan(X = manualoc.df, bp = c(0, 22))
#' 
#' # using a diferent threshold
#' a <- specan(X = manualoc.df, bp = c(0, 22), threshold = 20)
#' # View(a)
#' 
#' unlink(getwd(),recursive = T)
#' }
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/}), Grace Smith Vidaurre and Hua Zhong

specan <- function(X, bp = c(0,22), wl = 512, threshold = 15){
  if(class(X) == "data.frame") {if(all(c("sound.files", "selec", 
                                        "start", "end") %in% colnames(X))) 
  {
    start <- as.numeric(unlist(X$start))
    end <- as.numeric(unlist(X$end))
    sound.files <- as.character(unlist(X$sound.files))
    selec <- as.character(unlist(X$selec))
  } else stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  } else  stop("X is not a data frame")
  
  #if there are NAs in start or end stop
  if(any(is.na(c(end, start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if(all(class(end) != "numeric" & class(start) != "numeric")) stop("'end' and 'selec' must be numeric")
   
  #if any start higher than end stop
  if(any(end - start<0)) stop(paste("The start is higher than the end in", length(which(end - start<0)), "case(s)"))  
  
  #if any selections longer than 20 secs stop
  if(any(end - start>20)) stop(paste(length(which(end - start>20)), "selection(s) longer than 20 sec"))  
  options( show.error.messages = TRUE)
  
  #if bp is not vector or length!=2 stop
  if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
  if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}
  
  #return warning if not all sound files were found
  fs <- list.files(path = getwd(), pattern = ".wav$", ignore.case = TRUE)
  if(length(unique(sound.files[(sound.files %in% fs)])) != length(unique(sound.files))) 
    message(paste(length(unique(sound.files))-length(unique(sound.files[(sound.files %in% fs)])), 
    ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(sound.files %in% fs) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
    start <- start[d]
    end <- end[d]
    selec <- selec[d]
    sound.files <- sound.files[d]
  }

####
#Hua modified into pbapply, Apr 17, 2015
message("Measuring acoustic parameters:")
x <- as.data.frame(pbapply::pbapply(matrix(c(1:length(start)), ncol=1), 1, function(i) { 
  r <- tuneR::readWave(file.path(getwd(), sound.files[i]), from = start[i], to = end[i], units = "seconds") 
 
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
  peakf <- seewave::fpeaks(songspec, f = r@samp.rate, wl = 512, nmax = 3, plot = FALSE)[1, 1]
  
  #Fundamental frequency parameters
  ff <- seewave::fund(r, f = r@samp.rate, ovlp = 50, threshold = threshold, 
                     fmax = b[2] * 1000, plot = F)[, 2]
  meanfun<-mean(ff, na.rm = T)
  minfun<-min(ff, na.rm = T)
  maxfun<-max(ff, na.rm = T)
  
  #Dominant frecuency parameters
  y <- seewave::dfreq(r, f = r@samp.rate, wl = wl, ovlp = 0, plot = F, threshold = threshold, bandpass = b * 1000, fftw = TRUE)[, 2]
  meandom <- mean(y, na.rm = TRUE)
  mindom <- min(y, na.rm = TRUE)
  maxdom <- max(y, na.rm = TRUE)
  dfrange <- (maxdom - mindom)
  duration <- (end[i] - start[i])
  
  #modulation index calculation
  changes <- vector()
  for(j in which(!is.na(y))){
    change <- abs(y[j] - y[j + 1])
    changes <- append(changes, change)
  }
  if(mindom==maxdom) modindx<-0 else modindx <- mean(changes, na.rm = T)/dfrange
  
  #save results
  return(c(duration, meanfreq, sd, median, Q25, Q75, IQR, skew, kurt, sp.ent, sfm, mode, 
               centroid, peakf, meanfun, minfun, maxfun, meandom, mindom, maxdom, dfrange, modindx))
}))

  #change result names

  rownames(x) <- c("duration", "meanfreq", "sd", "median", "Q25", "Q75", "IQR", "skew", "kurt", "sp.ent", 
        "sfm","mode", "centroid", "peakf", "meanfun", "minfun", "maxfun", "meandom", "mindom", "maxdom", "dfrange", "modindx")
  x <- data.frame(sound.files, selec, as.data.frame(t(x)))
  colnames(x)[1:2] <- c("sound.files", "selec")
  rownames(x) <- c(1:nrow(x))
  
  message("all done!")
  return(x)
}
