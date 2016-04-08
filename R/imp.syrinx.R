#' Import Syrinx selections
#' 
#' \code{imp.syrinx} Imports Syrinx selection data from many files simultaneously. 
#' All files must be have the same columns.
#' @usage imp.syrinx(path = NULL, all.data = FALSE, recursive = FALSE)  
#' @param path A character string indicating the path of the directory in which to look for the text files. 
#' If not provided (default) the function searches into the current working directory. Default is \code{NULL}).
#' @param all.data Logical. If \code{TRUE}) all columns in text files are returned. Default is \code{FALSE}). Note 
#' that all files should contain exactly the same columns in the same order. 
#' @param recursive Logical. If \code{TRUE}) the listing recurse into sub-directories.
#' @return A single data frame with the information from the selection files. If all.data argument is set to \code{FALSE}) the data 
#' frame contains the following columns: selec, start, end, and selec.file. If sound.file.col is provided the data frame
#' will also contain a sound.file column. In addition, all rows with duplicated data are removed. This is useful when 
#' both spectrogram and waveform views are included in the Syrinx selection files. If all.data is set to \code{TRUE}) then all 
#' columns in selection files are returned.
#' @export
#' @name imp.syrinx
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#' #load data 
#' data(selection.files)
#' 
#' write.table(selection.files[[3]],file = "harpyeagle.wav.txt",row.names = F,
#'  col.names = F, sep= "\t")
#' 
#' write.table(selection.files[[4]],file = "Phae.long4.wav.txt",row.names = F, 
#' col.names = F, sep= "\t")
#' 
#' syr.dat<-imp.syrinx(all.data = FALSE)
#' 
#' View(syr.dat)
#' 
#' #getting all the data
#' syr.dat<-imp.syrinx(all.data = TRUE)
#' 
#' View(syr.dat)
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})

imp.syrinx <- function(path = NULL, all.data = FALSE, recursive = FALSE) 
{ if(is.null(path)) path = getwd()
sel.txt <- list.files(path = path,full.names = T)
sel.txt2 <- list.files(path = path,full.names = F)

sel.txt <- sel.txt[grep(".log$|.txt$",ignore.case = TRUE, sel.txt)]
sel.txt2 <- sel.txt2[grep(".log$|.txt$",ignore.case = TRUE, sel.txt2)]

if(length(sel.txt) == 0) stop("No selection files in path provided")

b<-NULL
if(substring(text = readLines(sel.txt[1])[1], first = 0, last = 9) == "fieldkey:") field <- T else field <- F


clist<-lapply(1:length(sel.txt), function(i)
  {    
  if(field)  {a <- read.table(sel.txt[i], header = T, sep = "\t", fill = TRUE) 

  if(!all.data) { c <- data.frame(selec.file = sel.txt2[i], sound.files = a[, grep("soundfile",colnames(a))],
                                selec = 1,
                                start = a[,grep("lefttimesec",colnames(a))],
                                end = a[,grep("righttimesec",colnames(a))])
  for(i in 2:nrow(c)) if(c$selec.file[i] == c$selec.file[i-1]) c$selec[i]<-c$selec[i-1] + 1
  } else c<-a 
                                b<-rbind(b, c)
                                } else {
          a<-read.table(sel.txt[i],header = F,sep = "\t", fill = TRUE)
            c <- a[, seq(2, ncol(a), by =2)]
           colnames(c) <- gsub(":", "", unlist(a[1, seq(1,ncol(a), by =2)]), fixed = T)
           if(!all.data) {c<-data.frame(sound.files = c[, grep("selected",colnames(c), ignore.case = T)],
                                       selec = 1,
                                       start = c[, grep("lefttime",colnames(c), ignore.case = T)],
                                       end = c[, grep("righttime",colnames(c), ignore.case = T)])
           for(i in 2:nrow(c)) if(c$sound.files[i] == c$sound.files[i-1]) c$selec[i] <- c$selec[i-1] + 1}          
  return(c)
                    }
})

b <- do.call("rbind", clist)
if(!all.data) if(any(is.na(b$start))) warning("NAs found (empty rows)")
return(b[!duplicated(b), ])
}
