#' Move/copy image files between directories
#' 
#' \code{move.imgs} Move/copy image files created by \code{\link{warbleR}} between 
#' directories (folders).
#' @usage move.imgs(from = NULL, to = NULL, it = "all", cut = TRUE, 
#' overwrite = FALSE, create.folder = TRUE, folder.name = "image_files")
#' @param from Directory path where image files to be copied are found. 
#'  If \code{NULL} (default) then the current working directory is used.
#' @param to Directory path where image files will be copied to.
#' @param it A character vector of length 1 giving the image type to be used. "all",
#' "tiff", "jpeg" and "pdf" are admitted ("all" includes all the rest). Default is "all".
#' @param cut Logical. Determines if files are removed from the original location after
#' being copied (cut) or not (just copied). Default is \code{TRUE}.
#' @param overwrite Logical. Determines if files that already exist in the destination directory 
#' should be overwritten. Default is \code{FALSE}.
#' @param create.folder Logical. Determines if are moved to a new folder (which is named with the
#' "folder.name" argument). Ignored if 'to' is provided. Default is \code{TRUE}.
#' @param folder.name Character string with the name of the new folder where the files will be 
#' copied to. Ignored if 'to' is provided. Default is \code{"image_files"}.
#' @family data manipulation
#' @seealso \code{\link{filtersels}} 
#' @export
#' @name move.imgs
#' @details This function aims to simplify the manipulation of the image files produce by many 
#' of the \code{\link{warbleR}} function. It copies/cuts files between directories.
#' @examples
#' \dontrun{
#' #Set temporary working directory
#' setwd(tempdir())
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
#' move.imgs(cut = TRUE)
#' 
#'# Check this folder
#' getwd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on apr-29-2017 (MAS)

move.imgs <- function(from = NULL, to = NULL, it = "all", cut = TRUE, overwrite = FALSE, create.folder = TRUE, folder.name = "image_files")
{
  if(is.null(from)) from <- getwd()
  if(is.null(to) & !create.folder) stop("Either 'to' must be provided or 'create.folder' set to TRUE")
  if(is.null(to) & create.folder) {dir.create(folder.name)
    to <- file.path(from, folder.name)}

  if(it == "all") pattern <- "\\.jpeg$|\\.tiff$|\\.pdf$"
  if(it == "tiff") pattern <- "\\.tiff$"
  if(it == "jpeg") pattern <- "\\.jpeg$"
  if(it == "pdf") pattern <- "\\.pdf$"
  
imgs <- list.files(path = from, pattern = pattern)

if(length(imgs) == 0) stop(paste("No image files were found in", from))

a <- file.copy(from = file.path(from, imgs), to = to, overwrite = overwrite)

if(all(!a) & !overwrite) message(paste("All files already existed in", to)) else
if(any(!a) & !overwrite) message(paste("Some files already existed in 'to'", to))

if(cut) unlink(imgs[a])

}


