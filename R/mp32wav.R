#' Convert .mp3 files to .wav
#' 
#' \code{mp32wav} converts several .mp3 files in working directory to .wav format
#' @usage mp32wav(samp.rate = 44.1, parallel = 1, path = NULL)  
#' @param samp.rate Sampling rate at which the .wav files should be written. The maximum permitted is 44.1 kHz (default). Units should be kHz.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#'  Not availble in Windows OS.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
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
#' @details convert all .mp3 files in working directory to .wav format. Function used internally to read .mp3 files (\code{\link[tuneR]{readMP3}}) sometimes crashes.
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) and Grace Smith Vidaurre
#last modification on jul-5-2016 (MAS)

mp32wav <- function(samp.rate = 44.1, parallel = 1, path = NULL) {
  
  #check path to working directory
  if(!is.null(path))
  {if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else setwd(path)} #set working directory
  
  if(samp.rate > 44.1) samp.rate <- 44.1
  
  #if parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  #parallel not available on windows
  if(parallel > 1 & Sys.info()[1] == "Windows")
  {message("parallel computing not availabe in Windows OS for this function")
    parallel <- 1}
  
  if(parallel > 1) 
    lapp <- function(X, FUN) parallel::mclapply(X, FUN, mc.cores = parallel) else lapp <- pbapply::pblapply
    
          options(warn = 0)
          
  files <- list.files(path=getwd(), pattern = "mp3$", ignore.case = TRUE) #list .mp3 files in working directory
  if(length(files) == 0) stop("no 'wav' files in working directory")
  message("Start writing wav files:")
  
  a<-lapp(files, function(x) tuneR::writeWave(downsample(tuneR::readMP3(filename =  x), samp.rate = samp.rate * 1000), paste(substr(x, 0, nchar(x) - 4), ".wav", sep="")))
return()  
  }
