#' Export raven selections
#' 
#' \code{exp_raven} exports selection tables as Raven selection data in .txt format.
#' @usage exp_raven(X, path = NULL, file.name = NULL, khz.to.hz = TRUE)
#' @param X Data frame with containing columns for sound file (sound.files), selection (selec), start and end time of signals ('start' and 'end') and low and high frequency ('low.freq' and 'high.freq', optional). See example data 'selec.table'.
#' @param path A character string indicating the path of the directory in which to look for the text files. 
#' If not provided (default) the function saves the file into the current working directory. Default is \code{NULL}).
#' @param file.name Name of the output .txt file.
#' @param khz.to.hz Logical. Controls if frequency variables should be converted from kHz (the unit used by warbleR) to Hz (the unit used by Raven). Default if \code{TRUE}.
#' @return A single data frame with information of the selection files. If all.data argument is set to \code{FALSE}) the data 
#' frame contains the following columns: selec, start, end, and selec.file. If sound.file.col is provided the data frame
#' will also contain a 'sound.files' column. In addition, all rows with duplicated data are removed. This is useful when 
#' both spectrogram and waveform views are included in the Raven selection files. If all.data is set to \code{TRUE} then all 
#' columns in selection files are returned. 
#' @details The function exports selection tables used by warbleR to raven selection data in .txt format.
#' @seealso \code{\link{imp.raven}}; \code{\link{imp.syrinx}} 
#' @export
#' @name exp_raven
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#' # Load data
#' data("selec.table")
#' 
#' # Select data for a single sound file
#' st1 <- selec.table[selec.table$sound.files == "Phae.long1.wav",]
#' 
#' # Export data
#' exp_raven(st1, file.name = "Phaethornis warbleR examples")
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-14-2017 (MAS)
exp_raven <- function(X, path = NULL, file.name = NULL, khz.to.hz = TRUE){
  
  if(is.null(file.name))
  stop("'file.name' is required")
  
  # convert to Hz
  if("low.freq" %in% names(X) & khz.to.hz)
  X$low.freq <- X$low.freq * 1000

  # convert to Hz
  if("high.freq" %in% names(X) & khz.to.hz)
    X$high.freq <- X$high.freq * 1000
  
  # change column names
  rvn.nms <- c("End File", "Selection", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)")
  wblr.nms <- c("sound.files", "selec", "start", "end", "low.freq", "high.freq")
  
  for(i in 1:length(rvn.nms))
  names(X)[names(X) == wblr.nms[i]] <- rvn.nms[i]

  # add View and channel column
  X$View <- "Spectrogram 1"  
  X$Channel <- 1  

  
  # if file name does not contain the extension
  if(substr(file.name, start = nchar(file.name)- 3, nchar(file.name)) != ".txt")
    file.name <- paste0(file.name, ".txt")
  
  if(!is.null(path))  
  file.name <- file.path(path, file.name)
  
 mtch <- match(c( "Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)"), names(X))
  
 X <- X[,c(mtch[!is.na(mtch)], setdiff(1:ncol(X), mtch))]

  write.table(x = X, sep = "\t", file = file.name, row.names = FALSE, quote = FALSE)
  
}

