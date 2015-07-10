#XXX Created XXX XX, 2015 
# Modified: Hua, Mar 2, 2015. 
# i. Put the required packages out of the function, R will automatically 
# warn user to install the package.

# Modified by G.S. Vidaurre 3-May-15
#           iii. added roxygen comments for documentation and namespace

#' Check .wav files
#' 
#' \code{checkwavs} checks whether .wav files can be read by subsequent functions.
#' @usage checkwavs()
#' @return If all .wav files are ok, returns message "All files are ok!".
#'   Otherwise returns "These file(s) cannot be read" message with names of the
#'   corrupted .wav files.
#' @details This function has no associated arguments, although the user must
#'   set the working directory where they wish to check .wav files beforehand.
#' @export
#' @name checkwavs
#' @examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' checkwavs()

# require(tuneR)

checkwavs <- function() { 
    files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T) #list .wav files in working director    
    if(length(files) == 0) stop("no .wav files in working directory") 
    a <- unlist(lapply(files, function(x) {
      if(is.numeric(try(readWave(as.character(x), header = T)$sample.rate,silent = T)))
      return(1) else return (0)})) 
  if(length(files[a == 0])>0){
    message("These file(s) cannot be read:")
    return(files[a == 0])
  } else message("All files are OK!") 
}

