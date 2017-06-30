#' Check .wav files
#' 
#' \code{checkwavs} checks whether .wav files can be read by subsequent functions.
#' @usage checkwavs(X = NULL, path = NULL)
#' @param X Optional. Data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input data frame. If provided the function also returns the
#' smallest number of samples from the listed selections, which limits the minimum window 
#' length (wl argument in other functions) that can be used in batch analyses. 
#' This could be useful for avoiding errors in dowstream functions (e.g. \code{\link{specan}}).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.  
#' @return If all .wav files are ok, returns message "All files can be read!".
#'   Otherwise returns the names of the corrupted .wav files.
#' @details This function checks if .wav files in the working directory can be read.
#' Users must set the working directory where they wish to check .wav files beforehand. 
#' If X is provided it also returns the smallest number of samples from
#' the selections listed in X (if all files can be read). Note that corrupt files can be
#' fixed using \code{\link{fixwavs}}) ('sox' must be installed to be able to run this function).
#' The function is intended for a "quick and dirty" check of the .wav files in a selections data
#'  frame. For a more thourough analysis see \code{\link{checksels}}.
#' @export
#' @seealso \code{\link{checksels}} \code{\link{seltailor}}
#' @name checkwavs
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' # without selection data frame
#' checkwavs()
#' 
#' # without selection data frame
#' checkwavs(X = selec.table)
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-5-2016 (MAS)

checkwavs <- function(X = NULL, path = NULL) { 
  
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
  if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
    setwd(path)} #set working directory
  
  #return warning if not all sound files were found
  files <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if(length(files) == 0) stop("no .wav files in working directory") 
  
  
  if(!is.null(X))
  {
    #if X is not a data frame
    if(!class(X) == "data.frame") stop("X is not a data frame")
    
    if(!all(c("sound.files", "selec", 
              "start", "end") %in% colnames(X))) 
      stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                     "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
    
    #if there are NAs in start or end stop
    if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
    
    #if end or start are not numeric stop
    if(all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'end' and 'selec' must be numeric")
    
    #if any start higher than end stop
    if(any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))  
    
    if(length(unique(X$sound.files[(X$sound.files %in% files)])) != length(unique(X$sound.files))) 
      message(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% files)])), 
                    ".wav file(s) not found"))
    
    #count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% files) 
    if(length(d) == 0){
      stop("The .wav files are not in the working directory")
    }  else X <- X[d, ]
    
    files <- files[files %in% X$sound.files]
  }
  
  a <- sapply(files, function(x) {
    r <- try(suppressWarnings(tuneR::readWave(as.character(x), header = TRUE)), silent = TRUE)
    if(class(r) == "try-error") return (NA) else
      return(r$sample.rate)  }) 
  
  if(length(files[is.na(a)])>0){
    message("Some file(s) cannot be read ")
    return(files[is.na(a)])
  } else {message("All files can be read!") 
    if(!is.null(X)) {
      df <- merge(X, data.frame(f = a, sound.files = names(a)), by = "sound.files")
      message("  smallest number of samples: ", floor(min((df$end - df$start)*df$f)), " (sound file:", as.character(df$sound.files[which.min((df$end - df$start)*df$f)]),"; selection label: ", df$selec[which.min((df$end - df$start)*df$f)], ")", sep = "")
    }
  }
  if(!is.null(path)) setwd(wd)
}
