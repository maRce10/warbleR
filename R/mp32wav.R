#' Convert .mp3 files to .wav
#' 
#' \code{mp32wav} converts several .mp3 files in working directory to .wav format
#' @usage mp32wav(samp.rate = 44.1, parallel = FALSE)  
#' @param samp.rate Sampling rate at which the .wav files should be written. The maximum permitted is 44.1 kHz (default). Units should be kHz.
#' @param parallel Either logical or numeric. Controls whether parallel computing is applied.
#'  If \code{TRUE} 2 cores are employed. If numeric, it specifies the number of cores to be used.
#'  Not available for windows OS. 
#' @return .wav files saved in the working directory with same name as original mp3 files.
#' @export
#' @name mp32wav
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#'  
#' #Then download mp3 files from xeno-canto
#' querxc(qword = "Phaethornis aethopygus", download = TRUE)
#' 
#' # Convert all files to .wav format
#' mp32wav()
#' 
#' #check this folder!!
#' getwd()
#' }
#' @details convert all .mp3 files in working directory to .wav format. Function used internally to read .mp3 files (\code{\link[tuneR]{readMP3}}) sometimes crashes
#' This should be fixed in the next version of tuneR. 
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/}) and Grace Smith Vidaurre

mp32wav <- function(samp.rate = 44.1, parallel = FALSE) {
  
  if(samp.rate > 44.1) samp.rate <- 44.1
  
  #if parallel was called
  if(is.logical(parallel)) { if(parallel) lapp <- function(X, FUN) parallel::mclapply(X, 
                          FUN, mc.cores = 2) else lapp <- pbapply::pblapply} else   lapp <- function(X, FUN) parallel::mclapply(X, FUN, mc.cores = parallel) 
  
  files <- list.files(path=getwd(), pattern = "mp3$", ignore.case = TRUE) #list .mp3 files in working directory
  if(length(files) == 0) stop("no 'wav' files in working directory")
  message("Start writing wav files:")
  
  a<-lapp(files, function(x) tuneR::writeWave(downsample(tuneR::readMP3(filename =  x), samp.rate = samp.rate * 1000), paste(substr(x, 0, nchar(x) - 4), ".wav", sep="")))
return()  
  }