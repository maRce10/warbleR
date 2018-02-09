#' Create 'selection.table' class objects
#' 
#' \code{make.selection.table} converts data frames into an object of class selection.table.
#' @usage make.selection.table(X, max.dur = 10, path = NULL,...)
#' @param X data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. Columns for 'top.freq', 'bottom.freq' and 'channel' are optional.  Alternatively, a 'selection.table' class object can be input to double check selections. 
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input object for other \code{\link{warbleR}} functions.
#' @param max.dur the maximum duration of expected for a selection  (ie. end - start).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param ... Additional arguments to be passed to \code{\link{checksels}} for customizing
#' checking routine.
#' @return An object of class selection.table which includes the original data.frame as well as the 
#' result of the checks as an additional attribute. This are used by downstream warbleR functions to improve efficiency and avoid
#' errors due to missing or mislabeled data, or selection out of the ranges of the original sound files. 
#' @details This function creates and object of class 'selection.table'. The function checks 1) if the selections listed in the data frame correspond to .wav files
#' in the working directory, 2) if the sound files can be read and if so, 3) if the start and end time
#' of the selections are found within the duration of the sound files. Note that the sound files 
#' should be in the working directory (or the directory provided in 'path').
#' This is useful for avoiding errors in dowstream functions (e.g. \code{\link{specan}}, \code{\link{xcorr}}, \code{\link{catalog}}, \code{\link{dfDTW}}). Note that corrupt files can be
#' fixed using \code{\link{fixwavs}}) ('sox' must be installed to be able to run this function).
#' The 'selection.table' class can be input in subsequent functions. 
#' @seealso \code{\link{checkwavs}}
#' @export
#' @name make.selection.table
#' @examples
#' {
#' # First set temporary folder
#' # setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' st <- make.selection.table(X = selec.table)
#' 
#' class(st)
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on nov-10-2017 (MAS)

make.selection.table <- function(X, max.dur = 10, path = NULL, ...)
{
  
  if(is.null(path)) path <- getwd()
  
  
  # cat("Checking selections")
  check.results <- checksels(X, path = path, ...)        
  
  if(any(check.results$check.res != "OK")) stop("Not all selections can be read (use check.res() to locate problematic selections)")
    
  check.results <- check.results[, names(X) %in% c("sound.files", "selec", "check.res", "duration", "min.n.sample", "sample.rate", "channels", "bits")]
  
  class(X) <- append("selection.table", class(X))
  
  attributes(X)$check.results <- check.results

  ## Set the name for the class
  return(X)
}
