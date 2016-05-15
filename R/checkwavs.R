#' Check .wav files
#' 
#' \code{checkwavs} checks whether .wav files can be read by subsequent functions.
#' @usage checkwavs()
#' @return If all .wav files are ok, returns message "All files are ok!".
#'   Otherwise returns "These file(s) cannot be read" message with names of the
#'   corrupted .wav files.
#' @details This function has no associated arguments, although users must
#'   set the working directory where they wish to check .wav files beforehand.
#' @export
#' @name checkwavs
#' @examples
#' \dontrun{
#' # First create empty folder
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

checkwavs <- function(X = NULL) { 
    files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T) #list .wav files in working director    
    if(length(files) == 0) stop("no .wav files in working directory") 
    a <- sapply(files, function(x) {
      r <- try(suppressWarnings(tuneR::readWave(as.character(x), header = T)), silent = T)
      if(is.list(r) & is.numeric(unlist(r)) & all(unlist(r) > 0))
        return(r$sample.rate) else return (NA)}) 

    if(length(files[is.na(a)])>0){
    cat("Some file(s) cannot be read ")
    return(files[is.na(a)])
  } else {cat("All files are OK!") 
    if(!is.null(X)) {
      X <- X[X$sound.files %in% files,]
      df <- merge(X, data.frame(f = a, sound.files = names(a)), by = "sound.files")
      cat("  smallest number of samples: ", floor(min((df$end - df$start)*df$f)), " (sound file:", as.character(X$sound.files[which.min((df$end - df$start)*df$f)]),"; selection: ", X$selec[which.min((df$end - df$start)*df$f)], ")", sep = "")
      }
      }
}

