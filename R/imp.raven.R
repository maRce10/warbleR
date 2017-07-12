#' Import Raven selections
#' 
#' \code{imp.raven} imports Raven selection data from many files simultaneously. Files must be in .txt format.
#' @usage imp.raven(path = NULL, sound.file.col = NULL, all.data = FALSE, recursive = FALSE,
#'  name.from.file = FALSE, ext.case = NULL, freq.cols = TRUE)  
#' @param path A character string indicating the path of the directory in which to look for the text files. 
#' If not provided (default) the function searches into the current working directory. Default is \code{NULL}).
#' @param sound.file.col A character string with the name of the column listing the sound files in 
#' the selection text files. Default is \code{NULL}). If provided, the output data
#' frame will contained all columns needed for subsequent analysis in \code{\link{warbleR}}. 
#' Duplicated rows, as when "waveform" and "spectrogram" information are included for the same selection, will be removed.
#' All selection files must contain "Selection", "Begin.Time" and "End.Time" columns.
#' @param all.data Logical. If \code{TRUE}) all columns in text files are returned, 
#' keeping the name columns as in the raven files (not in "warbleR" format). Default is \code{FALSE}). Columns absent in some selection files will be filled with NA's.
#' @param recursive Logical. If \code{TRUE}) the listing recurse into sub-directories.
#' @param name.from.file Logical. If \code{TRUE}) the sound file names are extracted from the selection text file name. 
#' It asssumes that selections files contained the suffix "Table.1.selections.txt" or "selections.txt". 
#' Note that by default it will assume that the extension file name is ".wav". This can be control using the
#' argumet 'ext.wav'. Default is \code{FALSE}). Ignored if sound.file.col' is provided and/or all.data is \code{TRUE}).
#' @param ext.case Character string of length 1 to specify whether sound file extensions are in upper or lower case. This should match the extension of the
#' of the .wav files from which the selection were made. It must be either 'upper' or 'lower'. Only needed when 'name.from.file' is \code{TRUE}). 
#' Ignored if 'sound.file.col' is provided and/or all.data is \code{TRUE}).
#' @param freq.cols Logical. If \code{TRUE}) 'Low Freq' and 'High Freq' columns are also imported. Ignored if all.data is \code{TRUE}.
#' @return A single data frame with information of the selection files. If all.data argument is set to \code{FALSE}) the data 
#' frame contains the following columns: selec, start, end, and selec.file. If sound.file.col is provided the data frame
#' will also contain a 'sound.files' column. In addition, all rows with duplicated data are removed. This is useful when 
#' both spectrogram and waveform views are included in the Raven selection files. If all.data is set to \code{TRUE} then all 
#' columns in selection files are returned. 
#' @details The function import raven selection data from many files simultaneously. Files must be in .txt format. Note that selection 
#' files including data from mulitple recordings cannot be imported. Make sure that NO OTHER TEXT FILES are found
#' in the working directory, only raven generated selections files.
#' @seealso \code{\link{imp.syrinx}} 
#' @export
#' @name imp.raven
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#' data(selection.files)
#' 
#' write.table(selection.files[[1]],file = "100889-Garrulax monileger.selections.txt",
#' row.names = FALSE, sep= '\t')
#' 
#' write.table(selection.files[[2]],file = "1023-Arremonops rufivirgatus.selections.txt",
#' row.names = FALSE, sep= '\t')
#' 
#' ## MAKE SURE THERE ARE NO OTHER .txt FILES IN THE WORKING DIRECTORY
#'  #providing the name of the column with the sound file names
#' rav.dat<-imp.raven(sound.file.col = "End.File", all.data = FALSE)
#' 
#' View(rav.dat)
#' 
#' #getting all the data
#' rav.dat<-imp.raven(all.data = TRUE)
#' View(rav.dat)
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-5-2017 (MAS)

imp.raven<-function(path = NULL, sound.file.col = NULL, all.data = FALSE, recursive = FALSE, 
                    name.from.file = FALSE, ext.case = NULL, freq.cols = TRUE) 
  {
  
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
  if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
    setwd(path)} #set working directory
  
  if(!is.null(ext.case)) 
    {if(!ext.case %in% c("upper", "lower")) stop("'ext.case' should be either 'upper' or 'lower'") else
    if(ext.case == "upper") ext <- "WAV" else ext <- "wav"}
  
  if(is.null(ext.case) & name.from.file) stop("'ext.case' must be provided when name.from.file is TRUE")
  
   sel.txt <- list.files(pattern = ".txt$", full.names = TRUE, recursive = recursive, ignore.case = TRUE)
  
   sel.txt2 <- list.files(pattern = ".txt$", full.names = FALSE, recursive = recursive, ignore.case = TRUE)
  
   if(length(sel.txt) == 0) stop("No selection .txt files in working directory/'path' provided")
  
   options(warn = -1)
    clist<-lapply(seq_len(length(sel.txt)), function(i)
      {  
      a <- try(read.table(sel.txt[i], header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE), silent = TRUE)
      if(class(a) != "try-error")
 {   if(!all.data) { 
      if(!is.null(sound.file.col)) 
    {  
      if(length(grep(sound.file.col, colnames(a))) == 0) stop(paste0("'",sound.file.col , "' column provided in 'sound.file.col' not found")) 
    c <- try(data.frame(sound.files = a[, grep(sound.file.col, colnames(a), ignore.case = TRUE)], channel = a[, grep("channel", colnames(a), ignore.case = TRUE)],
                                            selec = a[,grep("Selection",colnames(a), ignore.case = TRUE)],
             start = a[,grep("Begin.Time",colnames(a), ignore.case = TRUE)],
             end = a[, grep("End.Time",colnames(a), ignore.case = TRUE)], selec.file = sel.txt2[i], stringsAsFactors = FALSE), silent = TRUE)
    
    try(c$low.freq <- a[, grep("Low.Freq", colnames(a), ignore.case = TRUE)]/ 1000, silent = TRUE)
    try(c$high.freq <- a[, grep("High.Freq", colnames(a), ignore.case = TRUE)]/ 1000, silent = TRUE)
    if(all(c("High.Freq", "Low.Freq") %in% names(c)))
    c <- c[c(1:(ncol(c) - 3), ncol(c):(ncol(c)-1), ncol(c) -2 )]
    } else
           { 
             if(name.from.file) 
              {
               sound.files <- gsub("Table\\.([0-9]+)\\.selections.txt$", ext, sel.txt2[i])
           sound.files <- gsub(".selections.txt$", ext, sound.files)
           
           c <- try(data.frame(sound.files, selec.file = sel.txt2[i], channel = a[, grep("channel", colnames(a), 
                  ignore.case = TRUE)],selec = a[,grep("Selection",colnames(a), ignore.case = TRUE)],
                             start = a[, grep("Begin.Time", colnames(a), ignore.case = TRUE)],
                             end = a[, grep("End.Time", colnames(a), ignore.case = TRUE)], stringsAsFactors = FALSE), silent = TRUE)
           } else
                c <- try(data.frame(selec.file = sel.txt2[i], channel = a[, grep("channel", colnames(a), ignore.case = TRUE)], selec = a[,grep("Selection",colnames(a), ignore.case = TRUE)], start = a[, grep("Begin.Time", colnames(a), ignore.case = TRUE)], end = a[, grep("End.Time", colnames(a), ignore.case = TRUE)], stringsAsFactors = FALSE), silent = TRUE)
           }
      if(freq.cols)    {    
    try(c$low.freq <- a[, grep("Low.Freq", colnames(a), ignore.case = TRUE)]/ 1000, silent = TRUE)
        try(c$high.freq <- a[, grep("High.Freq", colnames(a), ignore.case = TRUE)]/ 1000, silent = TRUE)
    
        if(all(c("High.Freq", "Low.Freq") %in% names(c)))
          c <- c[c(1:(ncol(c) - 3), ncol(c):(ncol(c)-1), ncol(c) -2 )]
      
        }
      
      } else 
      c <- try(data.frame(a, selec.file = sel.txt2[i], stringsAsFactors = FALSE), silent = TRUE) 
      if(class(c) == "try-error") c <- NA
      } else c <- NA
      return(c)
 })

# determine files that could not be read
error.files <- sel.txt2[!sapply(clist, is.data.frame)]    
    
# remove NAs    
clist <- clist[sapply(clist , is.data.frame)]

# determine all column names in all selection tables    
cnms <- unique(unlist(sapply(clist, names)))    

# add columns that are missing to each selection table
clist <- lapply(clist, function(X)
  {
nms <- names(X)
if(length(nms) != length(cnms))  
for(i in cnms[!cnms %in% nms]) {
  X <- data.frame(X,  NA, stringsAsFactors = FALSE)
  names(X)[ncol(X)] <- i
  }

return(X)
})
    
b <- do.call("rbind", clist)

b <- b[!duplicated(b), ]

rownames(b) <- 1:nrow(b)

clm <- match(names(b), c("sound.files", "selec", "start", "end", "low.freq", "high.freq"))
clm <- clm[!is.na(clm)]
b <- b[, c(clm, setdiff(1:ncol(b), clm))]

if(length(error.files) > 0) cat(paste("some files could not be read:",paste(error.files, collapse = "/")))

return(b)

if(!is.null(path)) setwd(wd)
}

