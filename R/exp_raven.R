#' Export raven selections
#' 
#' \code{exp_raven} exports selection tables as Raven selection data in .txt format.
#' @usage exp_raven(X, path = NULL, file.name = NULL, khz.to.hz = TRUE, 
#' sound.file.path = NULL, single.file = TRUE)
#' @param X Data frame with containing columns for sound file (sound.files), selection (selec), start and end time of signals ('start' and 'end') and low and high frequency ('low.freq' and 'high.freq', optional). See example data 'selec.table'.
#' @param path A character string indicating the path of the directory in which to save the selection files. 
#' If not provided (default) the function saves the file into the current working directory.
#' @param file.name Name of the output .txt file. If \code{NULL} then the sound file names are used instead.
#' @param khz.to.hz Logical. Controls if frequency variables should be converted from kHz (the unit used by warbleR) to Hz (the unit used by Raven). Default is \code{TRUE}.
#' @param sound.file.path A character string indicating the path of the 
#' directory containing the sound file(s). Providing this information allows
#'  to open both sound file and selection table simultaneously. This can be
#'  done by using the "File > Open selection table" option in Raven (or drag/drop the 
#' selection file into Raven). Default is \code{NULL}. This argument is required when
#' exporting selections from multiple sound files. Note that sound files must be found in that directory.
#' @param single.file Logical. Controls whether a single selection file (\code{TRUE})
#' or multiple selection files for each sound files (\code{FALSE}, hence, only applicable
#' when several sound files are included in 'X'). Default is \code{TRUE}. Note that
#' 'sound.file.path' must be provided when exporting several sound files into a single selection file. If \code{FALSE} then the sound file name is used as the selection file name.
#' @return The function saves a selection table in '.txt' format that can be 
#' directly opened in Raven. If several sound files are available users can either 
#' export them as a single selection file or as multiple selection files (one for each sound file). 
#' No objects are returned in the R environment.
#' @details The function exports selection tables used by warbleR to Raven selection files in '.txt' format. This can be useful to obtain additional Raven
#' measurements on existing selections by adding new measurements to the 
#' selection table once in Raven. Note that selection labels must be numeric and unduplicated 
#' when exporting them to Raven. If that is not the case the function will
#' relabeled the selections and the previous selection labels will be retained in a new ('old.selec') column.
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
#' # Export data of a single sound file
#' exp_raven(st1, file.name = "Phaethornis warbleR examples")
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-14-2017 (MAS)
exp_raven <- function(X, path = NULL, file.name = NULL, khz.to.hz = TRUE, sound.file.path = NULL, single.file = TRUE){
  
  #if X is not a data frame
  if(!class(X) == "data.frame") stop("X is not a data frame")
  
  if(!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  #stop if more than 1 sound file is found in X
  if(length(unique(X$sound.files)) > 1 & is.null(sound.file.path)) stop("'sound.file.path' must be provided when including selection from multiple sound files")
  
  
  if(length(unique(X$sound.files)) == 1) single.file <- TRUE
  
  if(!is.null(sound.file.path))
{    
    #count number of sound files in working directory and if 0 stop
    recs.wd <- list.files(path = sound.file.path, pattern = "\\.wav$", ignore.case = TRUE)
  if(!all(unique(X$sound.files) %in% recs.wd)) 
    stop("Some (or all) .wav files are not in the working directory")
  }
  
  # convert to Hz
  if("low.freq" %in% names(X) & khz.to.hz)
  X$low.freq <- X$low.freq * 1000

  # convert to Hz
  if("high.freq" %in% names(X) & khz.to.hz)
    X$high.freq <- X$high.freq * 1000
  
  # change column names
  rvn.nms <- c("Begin File", "Selection", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)")
  wblr.nms <- c("sound.files", "selec", "start", "end", "low.freq", "high.freq")
  
  for(i in 1:length(rvn.nms))
    names(X)[names(X) == wblr.nms[i]] <- rvn.nms[i]
  
  # add View and channel column
  X$View <- "Spectrogram 1"  
  X$Channel <- 1  
  
  
  mtch <- match(c( "Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)"), names(X))
  
  X <- X[,c(mtch[!is.na(mtch)], setdiff(1:ncol(X), mtch))]
  
    
  if(!is.null(sound.file.path))
  {
    X$'Begin Path' <- file.path(sound.file.path, X$'Begin File')
    
    X$'File Offset' <- X$'Begin Time (s)'
    
    
    if(length(unique(X$'Begin File')) > 1 & single.file)
    {
      durs <- wavdur(path = sound.file.path)
    durs$cumdur <- cumsum(durs$duration)
    durs <- durs[durs$sound.files %in% X$'Begin File', ]
    
    out <- lapply(1:nrow(durs), function(x) {
      
      Y <- X[X$`Begin File` == durs$sound.files[x], ]
      Y$'File Offset' <- Y$`Begin Time (s)` 
      
      if(x > 1) {
        Y$`Begin Time (s)` <-  Y$`Begin Time (s)` + durs$cumdur[x - 1]
        Y$`End Time (s)` <-  Y$`End Time (s)` + durs$cumdur[x - 1]}
      
    return(Y)
        })     
      
    X <- do.call(rbind, out)
    } 
  }
  
 
 if(!is.null(sound.file.path))
   if(!is.numeric(X$Selection) | any(duplicated(X$Selection)))
   {
     X$old.selec <- X$Selection 
     X$Selection <- seq_len(nrow(X))
   }
 
if(single.file | nrow(X) == 1)
  row.list <- matrix(c(1, nrow(X)), nrow = 1) else 
  {
    e <- which(!duplicated(X$`Begin File`))  
    e2 <- c(e[2:length(e)] - 1, nrow(X))
    row.list <- data.frame(e, e2, sound.files = X$`Begin File`[!duplicated(X$`Begin File`)])
    }

  
out <-  lapply(seq_len(nrow(row.list)), function(x){
  
  if(is.null(file.name)) file.name <- ""
  
  if(!is.null(path))
    file.name <- file.path(path, file.name)
  
  if(nrow(row.list) > 1)
    file.name <- paste(file.name, row.list$sound.files[x], sep = "-")
  
  # if file name does not contain the extension
  if(substr(file.name, start = nchar(file.name)- 3, nchar(file.name)) != ".txt")
    file.name <- paste0(file.name, ".txt")
  
  write.table(x = X[c(row.list[x, 1] : row.list[x, 2]),], sep = "\t", file = file.name, row.names = FALSE, quote = FALSE)  
})
  
 

  
}

