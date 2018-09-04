#' A wrapper for tuneR's readWave that read sound files listed within selection tables
#' 
#' \code{read_wave} A wrapper for tuneR's \code{\link[tuneR]{readWave}} function that read sound files listed within selection tables
#' @usage read_wave(X, index, from = X$start[index], to = X$end[index], channel = NULL, 
#' header = FALSE, path = NULL) 
#' @param X 'data.frame', 'selection_table' or 'extended_selection_table' containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signals (start and end). 
#' Low and high frequency columns are optional. Default is \code{NULL}.
#' @param index Index of the selection in 'X' that will be read. Ignored if 'X' is \code{NULL}.
#' @param from Where to start reading, in seconds. Default is \code{X$start[index]}.
#' @param to Where to stop reading, in seconds. Default is \code{X$end[index]}.
#' @param channel Channel to be read from sound file (1 = left, 2 = right, or higher number for multichannel waves). If 
#' \code{NULL} or highes than the number of channels in a wave then the first channel is used.
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
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on may-7-2018 (MAS)

read_wave <- function (X, index, from = X$start[index], to = X$end[index], channel = NULL, header = FALSE, path = NULL) 
{
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  # check columns
  if (!all(c("sound.files", 
             "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "start", "end")[!(c("sound.files", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if (all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'start' and 'end' must be numeric")
  
  #if any start higher than end stop
  if (any(X$end - X$start < 0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else 
      if (!dir.exists(path)) stop("'path' provided does not exist") else
      {
        on.exit(setwd(getwd()))
        setwd(path)
        }
    
  filename <- as.character(X$sound.files[index])
  
  if (header)
    {
    if (any(is_selection_table(X), is_extended_selection_table(X)))
    object <- list(sample.rate = attr(X, "check.results")$sample.rate[attr(X, "check.results")$sound.files == X$sound.files[index]][1] * 1000, channels = 1, bits = attr(X, "check.results")$bits[attr(X, "check.results")$sound.files == X$sound.files[index]][1], samples = attr(X, "check.results")$n.samples[attr(X, "check.results")$sound.files == X$sound.files[index]][1]) else 
      object <- tuneR::readWave(filename = filename, header = TRUE)
    
    if (any(sapply(object, length) > 1)) object <- lapply(object, "[", 1)
    } else 
    {
    if (is_selection_table(X) | is.data.frame(X) & !is_extended_selection_table(X)) # if no extended selection table
    {
      object <- tuneR::readWave(filename = filename, header = FALSE, units = "seconds", from = from, to = to, toWaveMC = TRUE)
      #if more than 1 channel
     if (ncol(object) > 1 & length(X[ ,names(X) == "channel"]) > 0) object <- Wave(object[, X$channel[index]]) else
       object <- Wave(object[, 1])
     
    } else {
        
      object <- attr(X, "wave.objects")[[which(names(attr(X, "wave.objects")) == X$sound.files[index])[1]]]

      # if to is inifite then duration of sound file
      if (is.infinite(to)) to <- length(object@left)/object@samp.rate
      
        if (attr(X, "check.results")$mar.before[attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$selec == X$selec[index]] != 0 & attr(X, "check.results")$mar.after[attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$selec == X$selec[index]] != 0 & any(to < length(object@left)/object@samp.rate, from > 0))  object <- seewave::cutw(object, from = from, to = to, output = "Wave") #else
        
    }
    }

 return(object)
}
