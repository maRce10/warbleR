#' Subset selection data frames based on manually filtered image files
#' 
#' \code{filtersels} subsets selection data frames based on image files that have been manually filtered.
#' @usage filtersels(X, path = NULL, lspec = FALSE, pdf = FALSE)
#' @param X data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input data frame.
#' @param lspec A logical argument indicating if the image files to be use for filtering were produced by the function \code{\link{lspec}}. 
#' All the image files that correspond to a sound file must be deleted in order to be filtered out.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pdf A logical argument indicating if the image files are in .pdf format. Default is \code{FALSE}. Note that
#' pdf files can only be generated using \code{\link{lspec2pdf}} (so they are long spectrograms). 
#' Then, if \code{TRUE}, \code{lspec} argument is ignored.
#' @return If all .wav files are ok, returns message "All files are ok!".
#'   Otherwise returns "These file(s) cannot be read" message with names of the
#'   corrupted .wav files.
#' @details This function subsets selections (or sound files if \code{lspec} is \code{TRUE}) listed in a data frame
#'  based on the image files from spectrogram-creating functions (e.g. \code{\link{specreator}}) in the 
#'  working directory. Only the selections/sound files with and image in the working directory will remain. 
#'  This is useful for excluding selections from undesired signals. Note that the
#'  image files should be in the working directory (or the directory provided in 'path').
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
# fmloc <- filtersels(X = manualoc.df)
#' 
#' #this data frame does not have the selections corresponding to the images that were deleted
#' fmloc
#' 
#' #now using lspec images
#' lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300, ovlp = 10)
#' 
#' #go to the working directory and delete lspec images (the ones with several rows of spectrograms)
#' 
#' #filter selection data frame
# fmloc2 <- filtersels(X = manualoc.df, lspec = TRUE)
#' 
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on nov-13-2016 (MAS)


filtersels <- function(X, path = NULL, lspec = FALSE, pdf = FALSE)
  {
  #check path to working directory
  if(!is.null(path))
  {if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
    {wd <- getwd()
      setwd(path)}} #set working directory

#if X is not a data frame
if(!class(X) == "data.frame") stop("X is not a data frame")

if(!all(c("sound.files", "selec") %in% colnames(X))) 
  stop(paste(paste(c("sound.files", "selec")[!(c("sound.files", "selec") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))

if(!pdf)
{
  #check if files are in working directory
  imgs <- list.files(pattern = ".jpeg$|.tiff$", ignore.case = FALSE)
  if (length(imgs) == 0) 
    stop("No image files in working directory")
  
  if(!lspec){
  
  #remove the ones with no pX.jpeg ending
  imgs <- grep("-\\d+\\.jpeg|-\\d+\\.tiff" ,imgs, value = TRUE)
  if(length(imgs) == 0) stop("Images have not been produced by 'lspec' function")

  pgs <- substr(imgs, regexpr("-\\d+\\.jpeg" ,imgs), nchar(imgs))
  pgs <- as.numeric(gsub("-|.jpeg", "", pgs, ignore.case = TRUE))
  
    #subset selection table
  Y <- X[paste(X$sound.files, X$selec, sep = "-") %in%
           paste(gsub("-\\d+\\.jpeg|-\\d+\\.tiff", "" ,imgs), pgs, sep = "-"),]

  } else {

  imgs <- grep("p\\d+\\.jpeg|p\\d+\\.tiff" ,imgs, value = TRUE)
  if(length(imgs) == 0) stop("Images have not been produced by 'lspec' function")
  
  #subset selection table
  Y <- X[gsub(".wav$", "", X$sound.files, ignore.case = TRUE) %in% gsub("-p\\d+\\.jpeg$|-p\\d+\\.tiff$" , "", imgs), ]
  
  }
} else {
  #check if pdf files are in working directory
  imgs <- list.files(pattern = ".pdf$", ignore.case = FALSE)
  if (length(imgs) == 0) 
    stop("No pdf files in working directory")
  
  # imgs <- grep("p\\d+\\.jpeg|p\\d+\\.tiff" ,imgs, value = TRUE)
  # if(length(imgs) == 0) stop("Images have not been produced by 'lspec' function")
  
  #subset selection table
  Y <- X[gsub(".wav$", "", X$sound.files, ignore.case = TRUE) %in% gsub(".pdf$" , "", imgs), ]
  
}

if(nrow(Y) == 0) stop("Image files in working directory do not match sound file names in X (wrong working directory?)") else
 return(Y)
  if(!is.null(path)) on.exit(setwd(wd))
}
