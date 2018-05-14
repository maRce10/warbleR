#' A wrapper for tuneR's readWave that read sound files listed within selection tables
#' 
#' \code{read_wave} A wrapper for tuneR's \code{\link[tuneR]{readWave}} function that read sound files listed within selection tables
#' @usage read_wave(X, index, from = X$start[index], to = X$end[index], header = FALSE, path = NULL) 
#' @param X 'data.frame', 'selection_table' or 'extended_selection_table' containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signals (start and end). 
#' Low and high frequency columns are optional. Default is \code{NULL}.
#' @param index Index of the selection in 'X' that will be read. Ignored if 'X' is \code{NULL}.
#' @param from Where to start reading, in seconds. Default is \code{X$start[index]}.
#' @param to Where to stop reading, in seconds. Default is \code{X$end[index]}.
#' @param header If \code{TRUE}, only the header information of the Wave object is returned, otherwise (the default) the whole Wave object.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @return An object of class "Wave".
#' @export
#' @name read_wave
#' @details The function is a wrapper for \code{\link[tuneR]{readWave}} that read sound files listed within selection tables
#' ignores file extension mismatches, a common mistake when reading wave files. It 
#' is also used internally by warbleR functions to read wave objects from extended selection tables (see \code{\link{selection_table}} for details).
#' @examples
#' {
#' # First set temporary folder
#' # setwd(tempdir())
#' 
#' # write wave files with lower case file extension
#' data(list = c("Phae.long1"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' 
#' read_wave(X = selec.table, index  =  1)
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on may-7-2018 (MAS)

read_wave <- function (X, index, from = X$start[index], to = X$end[index], header = FALSE, path = NULL) 
{
  #check path to working directory
  if(is.null(path)) path <- getwd() else 
      if (!file.exists(path)) stop("'path' provided does not exist") else
      {
        on.exit(setwd(getwd()))
        setwd(path)
        }
    
  filename <- as.character(X$sound.files[index])
  
  # if (ignore.ext.case)
  # {
  #   # get file extension
  #   extnt <- substr(x = filename, start = nchar(filename) - 3, stop = nchar(filename))
  #   
  #   # check if is actually a file extension
  #   if (extnt %in% c(".wav", ".WAV"))  
  #     no.ext.name <- substr(x = filename, start = 0, stop = nchar(filename) - 4) else
  #       no.ext.name <- filename
  #     
  #     # paste file name and file extension
  #     if (file.exists(paste0(no.ext.name, ".wav"))) filename <- paste0(no.ext.name, ".wav")  else
  #       if (file.exists(paste0(no.ext.name, ".WAV"))) filename <- paste0(no.ext.name, ".wav")  
  # }
  
  if (header)
    {
    if(any(is_selection_table(X), is_extended_selection_table(X)))
    object <- list(sample.rate = attr(X, "check.results")$sample.rate[attr(X, "check.results")$sound.files == X$sound.files[index]], channels = 1, bits = attr(X, "check.results")$bits[attr(X, "check.results")$sound.files == X$sound.files[index]], samples = attr(X, "check.results")$n.samples[attr(X, "check.results")$sound.files == X$sound.files[index]]) else 
      object <- readWave(filename = filename, header = TRUE)
    
    if (any(sapply(object, length) > 1)) object <- lapply(object, "[", 1)
    } else 
    {
    if(is_selection_table(X) | is.data.frame(X) & !is_extended_selection_table(X)) # if no extended selection table
    object <- tuneR::readWave(filename = filename, header = FALSE, units = "seconds", from = from, to = to) else {
      object <- attr(X, "wave.objects")[[which(names(attr(X, "wave.objects")) == X$sound.files[index])[1]]]
    
      # if (attr(X, "by.song")$by.song)
        if (attr(X, "check.results")$mar.before[attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$selec == X$selec[index]] != 0 & attr(X, "check.results")$mar.after[attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$selec == X$selec[index]] != 0)  object <- seewave::cutw(object, from = from, to = to, output = "Wave") #else
        
      # if (attr(X, "check.results")$mar.before[attr(X, "check.results")$sound.files == X$sound.files[index]] != 0 & attr(X, "check.results")$mar.after[attr(X, "check.results")$sound.files == X$sound.files[index]] != 0)  object <- seewave::cutw(object, from = from, to = to, output = "Wave")
    }
    }

 return(object)
}
