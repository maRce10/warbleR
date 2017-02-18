#' Measure the duration of sound files
#' 
#' \code{wavdur} measures the duration of sound files in '.wav' format
#' @usage wavdur(files = NULL, path = NULL)
#' @param files Character vector with the names of the sound files to be measured. The sound files should be in the working directory or in the directory provided in 'path'.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @return A data frame with the duration (in seconds) of the sound files.
#' @export
#' @name wavdur
#' @details This function returns the duration (in seconds) of sound files.
#'   
#' @examples
#' \dontrun{
#' # Set temporary working directory
#' setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' 
#' wavdur()
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) 
#last modification on jul-5-2016 (MAS)

wavdur <- function(files = NULL, path = NULL) { 
  
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
  if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
    setwd(path)} #set working directory
  
  #stop if files is not a character vector
  if(!is.null(files) & !is.character(files)) stop("'files' must be a character vector")
  
   if(is.null(files))
  files <- list.files(pattern = "\\.wav$", ignore.case = TRUE) #list .wav files in working director    
  
   #stop if no wav files are found
   if(length(files) == 0) stop("no .wav files in working directory") 
  
  a <- sapply(files, function(x) {
    rec <- tuneR::readWave(as.character(x),header = TRUE)
    return(rec$samples/rec$sample.rate)  
  })
   return(data.frame(sound.files = files, duration = a, row.names = NULL))

    if(!is.null(path)) on.exit(setwd(wd))
  }
