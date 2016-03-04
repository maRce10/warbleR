#' Measure signal-to-noise ratio
#' 
#' \code{sig2noise} measures signal-to-noise ratio across multiple files.
#' @usage sig2noise(X, mar, parallel = FALSE)
#' @param X Data frame with results from \code{\link{manualoc}} or any data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). 
#' @param mar numeric vector of length 1. Specifies the margins adjacent to
#'   the start and end points of selection over which to measure noise.
#' @param parallel Either logical or numeric. Controls wehther parallel computing is applied.
#'  If \code{TRUE} 2 cores are employed. If numeric, it specifies the number of cores to be used.
#'  Not available for windows OS.
#' @return Data frame similar to \code{\link{autodetec}} output, but also includes a new variable 
#' with the signal-to-noise values.
#' @export
#' @name sig2noise
#' @details  Signal-to-noise ratio (SNR) is a measure of the level of a desired signal compared to 
#'  background noise. The function divides the mean amplitude of the signal by 
#'   the mean amplitude of the background noise adjacent to the signal. 
#'   A general margin to apply before and after the acoustic signal must 
#'   be specified. Setting margins for individual signals that have been 
#'   previously clipped from larger files may take some optimization, as 
#'   for calls within a larger file that are irregularly separated. When
#'   margins overlap with another acoustic signal nearby, the signal-to-noise 
#'   ratio (SNR) will be inaccurate. Any SNR less than or equal to one suggests 
#'   background noise is equal to or overpowering the acoustic signal.
#'   \code{\link{snrspecs}} can be used to troubleshoot different noise margins.
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#' data(list = c("Phae.long1","manualoc.df"))
#' writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
#' 
#' # specifying the correct margin is important
#' # use snrspecs to troubleshoot margins for sound files
#' sig2noise(manualoc.df[grep("Phae.long1", manualoc.df$sound.files), ], mar = 0.2)
#' 
#' # this smaller margin doesn't overlap neighboring signals
#' sig2noise(manualoc.df[grep("Phae.long1", manualoc.df$sound.files), ], mar = 0.1)
#' }
#' 
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/}) and Grace Smith Vidaurre
#' @source \url{https://en.wikipedia.org/wiki/Signal-to-noise_ratio}

sig2noise <- function(X, mar, parallel = FALSE){
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
  
  #return warning if not all sound files were found
  fs <- list.files(path = getwd(), pattern = ".wav$", ignore.case = T)
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
   
  #if parallel was called
  if(is.logical(parallel)) { if(parallel) lapp <- function(X, FUN) parallel::mclapply(X, 
  FUN, mc.cores = 2) else lapp <- pbapply::pblapply} else   lapp <- function(X, FUN) parallel::mclapply(X, FUN, mc.cores = parallel) 
  
  SNR <- lapp(c(1:length(sound.files)), function(y){
      
    # Read sound files to get sample rate and length
      r <- tuneR::readWave(file.path(getwd(), sound.files[y]), header = TRUE)
      f <- r$sample.rate
    
      #reset coordinates of signals 
      stn <- start[y] - mar
      enn <- end[y] + mar
      mar1 <- mar
      mar2 <- mar1 + end[y] - start[y]
      
      if (stn < 0) { 
      mar1 <- mar1  + stn
      mar2 <- mar2  + stn
      stn <- 0
      }
      
      if(enn > r$samples/f) enn <- r$samples/f
      
      r <- tuneR::readWave(file.path(getwd(), sound.files[y]), from = stn, to = enn, units = "seconds")
      
      # Identify the signal
      signal <- seewave::cutw(r, from =  mar1, to = mar2, f = f)
      
      # Identify areas before and after signal over which to measure noise 
      noise1 <- seewave::cutw(r, from =  0, 
                     to = mar1, f = f)
      
      noise2 <- seewave::cutw(r, from = mar2, to = length(r@left)/r@samp.rate, f = f)
      
      # Calculate mean noise amplitude 
      noisamp <- mean(c(seewave::env(noise1, f = f, envt = "abs", plot = FALSE), 
                        seewave::env(noise2, f = f, envt = "abs", plot = FALSE)))
      
      # Calculate mean signal amplitude 
      sigamp <- mean(seewave::env(signal, f = f, envt = "abs", plot = FALSE))
      
      # Calculate signal-to-noise ratio
      snr <- sigamp / noisamp
    
    return(snr)
    
  })
      
    # Add SNR data to manualoc output
    z <- data.frame(X[d,], SNR = unlist(SNR))
  return(z)
    
}
