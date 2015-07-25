#' Convert .mp3 files to .wav
#' 
#' \code{mp32wav} converts several .mp3 files in working directory to .wav format
#' @usage mp32wav()  
#' @return .wav files saved in the working directory with same name as original mp3 files.
#' @export
#' @name mp32wav
#' @examples
#' \dontrun{
#' # First create empty folder
#' dir.create(file.path(getwd(),"temporal"))
#' setwd(file.path(getwd(),"temporal"))
#' 
#' #Then download mp3 files from xeno-canto
#' querxc(qword = "Phaethornis aethopygus", download = TRUE)
#' 
#' # Convert all files to .wav format
#' mp32wav()
#' 
#' #check this folder!!
#' getwd()
#' 
#' unlink(getwd(),recursive = TRUE)
#' }
#' @details convert all .mp3 files in working directory to .wav format. Function use internally to read .mp3 files (\code{\link[tuneR]{readMP3}}) sometimes crashes
#' Apparently it Will be fixed in the next version of tuneR. 
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/}) and Grace Smith Vidaurre

mp32wav <- function() {
  options( show.error.messages = F)  
  files <- list.files(path=getwd(), pattern = "mp3$", ignore.case = TRUE) #list .mp3 files in working directory
  if(length(files) == 0) stop("no 'wav' files in working directory")
  message("Start writing wav files:")
  invisible(pblapply(files, function(x) tuneR::writeWave(tuneR::readMP3(filename = paste(getwd(), "/", x, sep="")),
                                                paste(getwd(), "/", substr(x, 0, nchar(x) - 4), ".wav", sep=""))))
}