#' Measure signal-to-noise ratio
#' 
#' \code{sig2noise} measures signal-to-noise ratio across multiple files.
#' @usage sig2noise(X, mar, parallel = 1, path = NULL, pb = TRUE, type = 1, eq.dur = FALSE)
#' @param X Data frame with results from \code{\link{manualoc}} or any data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). 
#' @param mar numeric vector of length 1. Specifies the margins adjacent to
#'   the start and end points of selection over which to measure noise.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' Not available in Windows OS.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @param type Numeric. Determine the formula to be used to calculate the signal-to-noise ratio (S = signal
#' , N = background noise): 
#' \itemize{
#' \item \code{1}: ratio of S mean amplitude envelope to 
#'   N mean amplitude envelope (\code{mean(env(S))/mean(env(N))})
#' \item \code{2}: ratio of S amplitude envelope quadratic mean  to N amplitude envelope quadratic mean
#'  (\code{rms(env(S))/rms(env(N))})
#' \item \code{3}: ratio of the difference between S amplitude envelope quadratic mean and N amplitude envelope quadratic mean to N amplitude envelope quadratic mean (\code{(rms(env(S)) - rms(env(N)))/rms(env(N))})
#' }
#' @param eq.dur Logical. Controls whether the noise segment that is measured has the same duration 
#' than the signal (if \code{TRUE}, default \code{FALSE}). If \code{TRUE} then mar argument is ignored.  
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
#' data(list = c("Phae.long1","selec.table"))
#' writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
#' 
#' # specifying the correct margin is important
#' # use snrspecs to troubleshoot margins for sound files
#' sig2noise(selec.table[grep("Phae.long1", selec.table$sound.files), ], mar = 0.2)
#' 
#' # this smaller margin doesn't overlap neighboring signals
#' sig2noise(selec.table[grep("Phae.long1", selec.table$sound.files), ], mar = 0.1)
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) and Grace Smith Vidaurre
#' @source \url{https://en.wikipedia.org/wiki/Signal-to-noise_ratio}
#last modification on jul-5-2016 (MAS)

sig2noise <- function(X, mar, parallel = 1, path = NULL, pb = TRUE, type = 1, eq.dur = FALSE){
  
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
  if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
    setwd(path)} #set working directory
  
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
  
  #return warning if not all sound files were found
  fs <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
    message(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% fs) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else X <- X[d, ]
   
  # If parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  #parallel not available on windows
  if(parallel > 1 & Sys.info()[1] == "Windows")
  {message("parallel computing not availabe in Windows OS for this function")
    parallel <- 1}
  
  if(parallel > 1) 
    {if(Sys.info()[1] == "Linux" & pb) lapp <- function(X, FUN) pbmcapply::pbmclapply(X, FUN, mc.cores = parallel) else
    lapp <- function(X, FUN) parallel::mclapply(X, FUN, mc.cores = parallel)} else {
      if(pb) lapp <- pbapply::pblapply else lapp <- lapply}
  
  options(warn = 0)
  
  SNR <- lapp(c(1:nrow(X)), function(y){
      
    # Read sound files to get sample rate and length
      r <- tuneR::readWave(file.path(getwd(), X$sound.files[y]), header = TRUE)
      f <- r$sample.rate
    
      # set margin to half of signal duration
      if(eq.dur) mar <- (X$end[y] - X$start[y])/2
        
      #reset coordinates of signals 
      stn <- X$start[y] - mar
      enn <- X$end[y] + mar
      mar1 <- mar
      
      if (stn < 0) { 
      mar1 <- mar1  + stn
      stn <- 0
      }
      
      mar2 <- mar1 + X$end[y] - X$start[y]
      
      if(enn > r$samples/f) enn <- r$samples/f
      
      r <- tuneR::readWave(file.path(getwd(), X$sound.files[y]), from = stn, to = enn, units = "seconds")
      
      # Identify the signal
      signal <- seewave::cutw(r, from =  mar1, to = mar2, f = f)
      
      # Identify areas before and after signal over which to measure noise 
      noise1 <- seewave::cutw(r, from =  0, 
                     to = mar1, f = f)
      
      noise2 <- seewave::cutw(r, from = mar2, to = length(r@left)/r@samp.rate, f = f)
      
      if(type == 1)
  {    # Calculate mean noise amplitude 
      noisamp <- mean(c(seewave::env(noise1, f = f, envt = "abs", plot = FALSE), 
                        seewave::env(noise2, f = f, envt = "abs", plot = FALSE)))
      
      # Calculate mean signal amplitude 
      sigamp <- mean(seewave::env(signal, f = f, envt = "abs", plot = FALSE))}
      
      if(type == 2)
      {    # Calculate mean noise amplitude 
        noisamp <- seewave::rms(c(seewave::env(noise1, f = f, envt = "abs", plot = FALSE), 
                          seewave::env(noise2, f = f, envt = "abs", plot = FALSE)))
        
        # Calculate mean signal amplitude 
        sigamp <- seewave::rms(seewave::env(signal, f = f, envt = "abs", plot = FALSE))}
      
      if(type == 3)
      {    # Calculate mean noise amplitude 
        noisamp <- seewave::rms(c(seewave::env(noise1, f = f, envt = "abs", plot = FALSE), 
                         seewave::env(noise2, f = f, envt = "abs", plot = FALSE)))
        
        # Calculate mean signal amplitude 
        sigamp <- seewave::rms(seewave::env(signal, f = f, envt = "abs", plot = FALSE))
        sigamp <- sigamp - noisamp
        }
      
      
      
      # Calculate signal-to-noise ratio
      snr <- sigamp / noisamp
    
    return(snr)
    
  })
      
    # Add SNR data to manualoc output
    z <- data.frame(X[d,], SNR = unlist(SNR))
  return(z)
    if(!is.null(path)) setwd(wd)    
}
