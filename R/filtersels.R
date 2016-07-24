#' Filter selection data frames based on filtered image files
#' 
#' \code{filtersels} Filter selection data frames based on image files from spectrogram creating functions that have been manually filtered.
#' @usage filtersels(X, path = NULL, lspec = F)
#' @param X data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input data frame.
#' @param lspec A logical argument indicating if the image files to be use for filtering were produced by the function \code{\link{lspec}}.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @return If all .wav files are ok, returns message "All files are ok!".
#'   Otherwise returns "These file(s) cannot be read" message with names of the
#'   corrupted .wav files.
#' @details This function removes selections listed in a data frame based on the image files
#' from spectrogram creating function (e.g. \code{\link{specreator}}) in the working directory.
#'  This is useful for avoiding including selections from undesired signals. Note that the
#'  image files should be in the working directory (or the directory provided 
#' in 'path').
#' @name filtersels
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "manualoc.df"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' 
#' specreator(manualoc.df, flim = c(0, 11), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), 
#' picsize = 2, res = 300, cexlab = 2, mar = 0.05, wl = 300)
#' 
#' #go to the working directory and delete some images
#' 
#' #filter selection data frame
#' fmloc <- filtersels(X = manualoc.df)
#' 
#' fmloc
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-15-2016 (MAS)


filtersels <- function(X, path = NULL, lspec = F)
  {
  #check path to working directory
  if(!is.null(path))
  {if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else setwd(path)} #set working directory

#if X is not a data frame
if(!class(X) == "data.frame") stop("X is not a data frame")

if(!all(c("sound.files", "selec") %in% colnames(X))) 
  stop(paste(paste(c("sound.files", "selec")[!(c("sound.files", "selec") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))

#check if files are in working directory
files <- list.files(pattern = ".jpeg$|.tiff$", ignore.case = TRUE)
if (length(files) == 0) 
  stop("no image files in working directory")

if(!lspec){
  imgs <- list.files(pattern = ".jpeg$|.tiff$", ignore.case = TRUE)
  
  sfs <- try(sapply(strsplit(as.character(imgs), "-",fixed=TRUE), "[[", 1), silent = TRUE)
  slc <- try(sapply(strsplit(as.character(imgs), "-",fixed=TRUE), "[[", 2), silent = TRUE)
  
  if(class(slc) == "try error"  & class(slc) == "try error") stop("Images don't match selections in data frame")

  if(class(slc) == "try error") stop("Some images don't match selections in data frame (remove any .jpeg or .tiff file that is not a selection image")

    Y <- X[paste(X$sound.files, X$selec) %in% paste(sfs, slc),]
} else {
  imgs <- list.files(pattern = ".jpeg$|.tiff$", ignore.case = TRUE)
  
  sfs <- sapply(strsplit(as.character(imgs), "-",fixed=TRUE), "[[", 1)

  z <- gsub(".wav$", "", X$sound.files, ignore.case = TRUE)
  
  Y <- X[z %in% sfs, ]
  
}
 return(Y)
}