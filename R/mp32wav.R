#' Convert .mp3 files to .wav
#' 
#' \code{mp32wav} converts several .mp3 files in working directory to .wav format
#' @usage mp32wav(samp.rate = 44.1)  
#' @param samp.rate Sampling rate at which the .wav files should be written. The maximum permitted is 44.1 kHz (default). Units should be kHz.
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


mp32wav <- function(samp.rate = 44.1) {
  
  if(samp.rate > 44.1) samp.rate <- 44.1
  
  files <- list.files(path=getwd(), pattern = "mp3$", ignore.case = TRUE) #list .mp3 files in working directory
  if(length(files) == 0) stop("no 'wav' files in working directory")
  message("Start writing wav files:")
  pbapply::pblapply(files, function(x) tuneR::writeWave(downsample(tuneR::readMP3(filename =  x), samp.rate = samp.rate * 1000), paste(substr(x, 0, nchar(x) - 4), ".wav", sep="")))
  }