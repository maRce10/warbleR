#' Check .wav files
#' 
#' \code{checkwavs} checks whether .wav files can be read by subsequent functions.
#' @usage checkwavs(X = NULL, path = NULL)
#' @param X data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input data frame. If provided the function also returns the
#' smallest number of samples from the listed selections, which limits the minimum window 
#' length (wl argument in other functions) that can be used in batch analyses. 
#' This could be useful for avoiding errors in dowstream functions (e.g. \code{\link{specan}}).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.  
#' @return If all .wav files are ok, returns message "All files are ok!".
#'   Otherwise returns "These file(s) cannot be read" message with names of the
#'   corrupted .wav files.
#' @details This function checks if .wav files in the working directory can be read.
#' Users must set the working directory where they wish to check .wav files beforehand. 
#' If X is provided it also returns the smallest number of samples from
#' the selections listed in X (if all files can be read). 
#' @export
#' @seealso \code{\link{checksels}}
#' @name checkwavs
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' 
#' checkwavs()
#' 
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-5-2016 (MAS)

checkwavs <- function(X = NULL, path = NULL) { 

    #check path to working directory
  if(!is.null(path))
  {if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else setwd(path)} #set working directory
  
    files <- list.files(pattern = "wav$", ignore.case = TRUE) #list .wav files in working director    
  if(length(files) == 0) stop("no .wav files in working directory") 
  a <- sapply(files, function(x) {
    r <- try(suppressWarnings(tuneR::readWave(as.character(x), header = TRUE)), silent = TRUE)
    if(is.list(r) & is.numeric(unlist(r)) & all(unlist(r) > 0))
      return(r$sample.rate) else return (NA)}) 
  
  if(length(files[is.na(a)])>0){
    message("Some file(s) cannot be read ")
    return(files[is.na(a)])
  } else {message("All files are OK!") 
    if(!is.null(X)) {
      X <- X[X$sound.files %in% files,]
      df <- merge(X, data.frame(f = a, sound.files = names(a)), by = "sound.files")
      message("  smallest number of samples: ", floor(min((df$end - df$start)*df$f)), " (sound file:", as.character(X$sound.files[which.min((df$end - df$start)*df$f)]),"; selection: ", X$selec[which.min((df$end - df$start)*df$f)], ")", sep = "")
    }
  }
}
