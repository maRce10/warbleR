#' Measure signal to noise ratio
#' 
#' \code{sig2noise} measures signal to noise ratio across multiple files.
#' @usage sig2noise(X, mar)
#' @param X data frame output from manualoc().
#' @param mar numeric vector of length one. Specifies the margins to subtract 
#'   from/add to start and end points of manualoc() selection, respectively, 
#'   over which to measure noise.
#' @return manualoc data frame with a new variable for signal to noise
#'   calculations.
#' @export
#' @name sig2noise
#' @details A general margin to apply before and after the acoustic signal must 
#'   be specified. Setting margins for individual calls that have been 
#'   previously clipped from larger files may take some optimization, as will 
#'   margins for calls within a larger file that are irregularly separated. When
#'   margins overlap with another acoustic signal close by, the signal to noise 
#'   ratio (SNR) will be inaccurate. Any SNRs less than or equal to one suggest 
#'   background noise is equal to or overpowering the acoustic signal.
#'   \code{\link{snrspecs}} can be used to troubleshoot different noise margins.
#' @examples
#' data(Arre.aura)
#' data(manualoc.df)
#' writeWave(Arre.aura, "Arre.aura.wav") #save sound files 
#' X <- manualoc.df
#' 
#' # specifying the correct margin is important
#' # use snrspecs to troubleshoot margins for sound files
#' sig2noise(X[grep("Arre", X$sound.files), ], mar = 0.2)
#' 
#' # this smaller margin doesn't overlap neighboring calls
#' sig2noise(X[grep("Arre", X$sound.files), ], mar = 0.1)
#' 
#' # also works
#' sig2noise(X[X$sound.files == "Arre.aura.wav", ], mar = 0.01)
#' @author Marcelo Araya-Salas http://marceloarayasalas.weebly.com/ and Grace Smith Vidaurre

sig2noise <- function(X, mar){
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
   
  #options(show.error.messages = TRUE)
  
  SNR <- pbapply::pbsapply(c(1:length(sound.files)), function(y){
      
      # Read sound file
      r <- tuneR::readWave(file.path(getwd(), sound.files[y]))

      # Set the frequency or sampling rate of the signal 
      f <- r@samp.rate 
      
      # Identify the signal
      signal <- seewave::cutw(r, from = start[y], to = end[y], f = f)
    
      # Identify areas before and after signal over which to measure noise 
      stn <- start[y] - mar
      enn <- end[y] + mar
      if (stn < 0) stn <- 0
      if (enn > length(r@left)/r@samp.rate) enn <- length(r@left)/r@samp.rate
      noise1 <- seewave::cutw(r, from =  stn, 
                     to = start[y], f = f)
      
      noise2 <- seewave::cutw(r, from = end[y], to = enn, f = f)
      
      # Calculate mean noise amplitude 
      noisamp <- mean(c(seewave::env(noise1, f = f, envt = "abs", plot = FALSE), 
                        seewave::env(noise2, f = f, envt = "abs", plot = FALSE)))
      
      # Calculate mean signal amplitude 
      sigamp <- mean(seewave::env(signal, f = f, envt = "abs", plot = FALSE))
      
      # Calculate signal to noise ratio
      snr <- sigamp / noisamp
    
    return(snr)
    
  })
      
    # Add SNR data to manualoc output
    z <- data.frame(X[d,], SNR)
  message("all done!")    
  return(z)
    
}
