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


checkwavs <- function() { 
    files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T) #list .wav files in working director    
    if(length(files) == 0) stop("no .wav files in working directory") 
    a <- unlist(lapply(files, function(x) {
      if(is.numeric(try(tuneR::readWave(as.character(x), header = T)$sample.rate,silent = T)))
      return(1) else return (0)})) 
  if(length(files[a == 0])>0){
    cat("Some file(s) cannot be read:")
    return(files[a == 0])
  } else cat("All files are OK!") 
}

