#' Move/copy image files between directories
#' 
#' \code{move.imgs} moves/copies image files created by \code{\link{warbleR}} between 
#' directories (folders).
#' @usage move.imgs(from = NULL, to = NULL, it = "all", cut = TRUE, 
#' overwrite = FALSE, create.folder = TRUE, folder.name = "image_files", 
#' parallel = 1, pb = TRUE)
#' @param from Directory path where image files to be copied are found. 
#'  If \code{NULL} (default) then the current working directory is used.
#' @param to Directory path where image files will be copied to.
#' @param it A character vector of length 1 giving the image type to be used. "all",
#' "tiff", "jpeg" and "pdf" are admitted ("all" includes all the rest). Default is "all".
#' @param cut Logical. Determines if files are removed from the original location after
#' being copied (cut) or not (just copied). Default is \code{TRUE}.
#' @param overwrite Logical. Determines if files that already exist in the destination directory 
#' should be overwritten. Default is \code{FALSE}.
#' @param create.folder Logical. Determines if files are moved to a new folder (which is named with the
#' "folder.name" argument). Ignored if 'to' is provided. Default is \code{TRUE}.
#' @param folder.name Character string with the name of the new folder where the files will be 
#' copied to. Ignored if 'to' is provided. Default is \code{"image_files"}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @family data manipulation
#' @seealso \code{\link{filtersels}} 
#' @export
#' @name move.imgs
#' @details This function aims to simplify the manipulation of the image files produce by many 
#' of the \code{\link{warbleR}} function. It copies/cuts files between directories.
#' @examples
#' {
#' #Set temporary working directory
#' # setwd(tempdir())
#' 
#' #load data
#' data("Cryp.soui")
#' writeWave(Cryp.soui, "Cryp.soui.wav") #save sound files 
#' 
#' #autodetec location of signals
#' ad <- autodetec(threshold = 6, bp = c(1, 3), mindur = 1.2,
#' maxdur = 3, img = FALSE, ssmooth = 600, wl = 300, flist = "Cryp.soui.wav")
#' 
#' #track dominant frequency graphs with freq reange detection
#' trackfreqs(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, it = "tiff",
#' bp = c(1, 3), contour = "df", wl = 300, frange = TRUE)
#'
#' #copy files
#' move.imgs(cut = FALSE)
#'
#' #cut files
#' move.imgs(cut = TRUE, to = "image_files")
#' 
#'# Check this folder
#' getwd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on feb-09-2017 (MAS)

move.imgs <- function(from = NULL, to = NULL, it = "all", cut = TRUE, overwrite = FALSE, create.folder = TRUE, folder.name = "image_files", parallel = 1, pb = TRUE)
{
  if(is.null(from)) from <- getwd()
  if(is.null(to) & !create.folder) stop("Either 'to' must be provided or 'create.folder' set to TRUE")
  if(is.null(to) & create.folder) {
    to <- file.path(from, folder.name)
    if(dir.exists(to)) stop('Directory with folder name provided already exists')
    dir.create(to)
  }
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  
  
  if(it == "all") pattern <- "\\.jpeg$|\\.tiff$|\\.pdf$"
  if(it == "tiff") pattern <- "\\.tiff$"
  if(it == "jpeg") pattern <- "\\.jpeg$"
  if(it == "pdf") pattern <- "\\.pdf$"
  
  imgs <- list.files(path = from, pattern = pattern, ignore.case = TRUE)
  
  if(length(imgs) == 0) cat(paste("No image files were found in", from))
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
a <- pbapply::pblapply(X = seq(1, length(imgs), by = 10), cl = cl, FUN = function(x) 
  { 
    if(length(imgs) <= x + 9) y <- length(imgs) else y <- x + 9
    
    file.copy(from = file.path(from, imgs[x:y]), to = file.path(to, imgs[x:y]), overwrite = overwrite)  
    }) 
  
  a <- unlist(a)
  
  # a <- file.copy(from = file.path(from, imgs), to = file.path(to, imgs), overwrite = overwrite)
  
  if(all(!a) & !overwrite) cat(paste("All files already existed in", to)) else
    if(any(!a) & !overwrite) cat(paste("Some files already existed in 'to'", to))
  
  if(cut) unlink(file.path(from, imgs)[a])
  
}