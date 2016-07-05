#' Check selection data frames
#' 
#' \code{checksels} checks whether selections can be read by subsequent functions.
#' @usage checksels(X, parallel =  1, path = NULL)
#' @param X data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input data frame.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#'  Not available in Windows OS.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @return If all .wav files are ok, returns message "All files are ok!".
#'   Otherwise returns "These file(s) cannot be read" message with names of the
#'   corrupted .wav files.
#' @details This function checks if the selections listed in the data frame correspond to .wav files
#' in the working directory, if the sound files can be read and if so, if the start and end time
#' of the selections are found within the duration of the sound files. Note that the sound files 
#' should be in the working directory (or the directory provided in 'path').
#' This is useful for avoiding errors in dowstream functions (e.g. \code{\link{specan}}).
#' @seealso \code{\link{checkwavs}}
#' @export
#' @name checksels
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "manualoc.df"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' 
#' checksels(X = manualoc.df)
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-5-2016 (MAS)

checksels <- function(X = NULL, parallel =  1, path = NULL){
  
  #check path to working directory
  if(!is.null(path))
  {if(class(try(setwd(path), silent = T)) == "try-error") stop("'path' provided does not exist") else setwd(path)} #set working directory
  
  files <- list.files(pattern = "wav$", ignore.case = T)
  if (length(files) == 0) 
    stop("no .wav files in working directory")
  
  if(!any(X$sound.files %in% files)) stop("Sound files in X aren't found in the working directory")
  
  if (parallel > 1) {
    lapp <- function(X, FUN) parallel::mclapply(X, FUN, mc.cores = parallel)
  }
  else lapp <- pbapply::pblapply
  
  Y <- lapp(unique(X$sound.files), function(x)
    {
    Y <- X[X$sound.files == x, ]
    if(file.exists(as.character(x)))     {
      rec <- try(suppressWarnings(tuneR::readWave(as.character(x), header = T)), silent = T)
      if(is.list(rec) & is.numeric(unlist(rec)) & all(unlist(rec) > 0))
      {maxdur <- rec$samples/rec$sample.rate  
        Y$check.res <- "OK"
        Y$check.res[Y$end > maxdur] <- "exceeds sound file length"} else
          Y$check.res <- "Sound file can't be read"
        } else    Y$check.res <- "sound file not found"
    return(Y)
    })
  return(do.call(rbind, Y))  
}


