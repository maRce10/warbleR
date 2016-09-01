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
  {if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else setwd(path)} #set working directory
  
  #if X is not a data frame
  if(!class(X) == "data.frame") stop("X is not a data frame")
  
  if(!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))

  #check if files are in working directory
  files <- list.files(pattern = "wav$", ignore.case = TRUE)
  if (length(files) == 0) 
    stop("no .wav files in working directory")
  
  if(!any(X$sound.files %in% files)) stop("Sound files in X aren't found in the working directory")
    
  #if there are NAs in start or end stop
  if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if(all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'end' and 'selec' must be numeric")
  
  #if any start higher than end stop
  if(any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))  
  
  #if any selections longer than 20 secs stop
  if(any(X$end - X$start>20)) stop(paste(length(which(X$end - X$start>20)), "selection(s) longer than 20 sec"))  
  options( show.error.messages = TRUE)
  
  
    csFUN <- function(x, X){
    Y <- X[X$sound.files == x, ]
    if(file.exists(as.character(x)))     {
      rec <- try(suppressWarnings(tuneR::readWave(as.character(x), header = TRUE)), silent = TRUE)
      if(is.list(rec) & is.numeric(unlist(rec)) & all(unlist(rec) > 0))
      {maxdur <- rec$samples/rec$sample.rate  
        Y$check.res <- "OK"
        Y$check.res[Y$end > maxdur] <- "exceeds sound file length"} else
          Y$check.res <- "Sound file can't be read"
        } else    Y$check.res <- "sound file not found"
    return(Y)
      }
      #parallel not available on windows
      if(parallel > 1 & Sys.info()[1] == "Windows")
      {message("parallel computing not availabe in Windows OS for this function")
        parallel <- 1}
      
      if(parallel > 1) {
        if(Sys.info()[1] == "Windows") {
          
          x <- NULL #only to avoid non-declared objects
          
          cl <- parallel::makeCluster(parallel)
          
          doParallel::registerDoParallel(cl)
          
          a1 <- foreach::foreach(x = unique(X$sound.files)) %dopar% {
            csFUN(x, X)
          }
          
          parallel::stopCluster(cl)
          
        } 
        
        if(Sys.info()[1] == "Linux"){    # Run parallel in other operating systems
          
          a1 <- parallel::mclapply(unique(X$sound.files), function(x) {
            csFUN(x, X)
          })
          
        }
        if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
        {
          cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))
          
          doParallel::registerDoParallel(cl)
          
          a1 <- foreach::foreach(x = unique(X$sound.files)) %dopar% {
            csFUN(x, X)
          }
          parallel::stopCluster(cl)
        }
        
        
      } else {a1 <- pbapply::pblapply(unique(X$sound.files), function(x) 
      { 
        csFUN(x, X)
      })
      
      }    
      
  return(do.call(rbind, a1))  
}


