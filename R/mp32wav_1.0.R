#Modified: Hua Mar 6, 2015.
#           i. Add progress bar in searching process using pblapply
#Modified: Hua, Mar 2, 2015. 
#           i. Put the required packages out of the function, R will automatically warn user to install the package.

#' Convert .mp3 files to .wav
#' 
#' \code{mp32wav} converts several .mp3 files in working directory to .wav format
#' @usage mp32wav()  
#' @return .wav files saved in the working directory with same name as original mp3 files.
#' @export
#' @name mp32wav
#' @examples
#' \dontrun{
#' # Need to have at least 1 mp3 file in working directory
#' mp32wav() 
#' }
#' @details Currently only working in windows OS. Function to read mp3 from tuneR package crashes in other operating systems. 
#' @author Marcelo Araya-Salas (http://marceloarayasalas.weebly.com/) and Grace Smith Vidaurre

# require(tuneR)
# require(pbapply)

####

mp32wav <- function() {
  options( show.error.messages = F)  
  files <- list.files(path=getwd(), pattern = "mp3$", ignore.case = TRUE) #list .mp3 files in working directory
  if(length(files) == 0) stop("no 'wav' files in working directory")
  message("Start writing wav files:")
  invisible(pblapply(files, function(x) writeWave(readMP3(filename = paste(getwd(), "/", x, sep="")),
                                                paste(getwd(), "/", substr(x, 0, nchar(x) - 4), ".wav", sep=""))))
}

