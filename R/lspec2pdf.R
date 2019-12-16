# Combine lspec images to single pdf files
#' 
#' \code{lspec2pdf} combines \code{\link{lspec}} images in .jpeg format to a single pdf file. 
#' @usage lspec2pdf(keep.img = TRUE, overwrite = FALSE, parallel = 1, path = NULL, pb = TRUE)
#' @param keep.img Logical argument. Indicates whether jpeg files should be kept (default) or remove.
#'   (including sound file and page number) should be magnified. Default is 1.
#' @param overwrite Logical argument. If \code{TRUE} all jpeg pdf will be produced again 
#'   when code is rerun. If \code{FALSE} only the ones missing will be produced. Default is \code{FALSE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @export
#' @name lspec2pdf
#' @details The function combines  spectrograms for complete sound files from the \code{\link{lspec}} function into
#' a single pdf (for each sound file).
#' @seealso \code{\link{lspec}}, \code{\link{catalog2pdf}}, 
#' \href{https://marce10.github.io/2017/01/07/Create_pdf_files_with_spectrograms_of_full_recordings.html}{blog post on spectrogram pdfs}
#' @examples
#' \dontrun{
#' # save sound file examples
#' data(list = c("Phae.long1", "Phae.long2"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) 
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' 
#' lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300, it = "jpeg", path = tempdir())
#' 
#' #now create single pdf removing jpeg
#' lspec2pdf(keep.img = FALSE, path = tempdir())
#' 
#' # check this floder
#'  tempdir()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on mar-12-2018 (MAS)

lspec2pdf <- function(keep.img = TRUE, overwrite = FALSE, parallel = 1, path = NULL, pb = TRUE)
{
  # error message if jpeg package is not installed
  if (!requireNamespace("jpeg",quietly = TRUE))
    stop("must install 'jpeg' to use this function")
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(specan)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
  # rename path for sound files
  names(opt.argms)[names(opt.argms) == "wav.path"] <- "path"
  
  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]
  
  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]
  
  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]
  
  # set options left
  if (length(opt.argms) > 0)
    for (q in 1:length(opt.argms))
      assign(names(opt.argms)[q], opt.argms[[q]])
  
  
  #check path to working directory
  if (is.null(path)) 
    path <- getwd() else
    if (!dir.exists(path)) stop("'path' provided does not exist")
  
  #list jpeg files
  imgs <- list.files(path = path, pattern = "\\.jpeg$", ignore.case = TRUE)
  if (length(imgs) == 0) stop("No .jpeg files were found in the working directory")
  
  #remove images that don't have the pX.jpeg ending
  imgs <- grep("p\\d+\\.jpeg" ,imgs, value = TRUE)
  
  #remove page info at the end of file names to get sound file names
  or.sf <- gsub("-p\\d+\\.jpeg", "" ,imgs)

  #loop over each sound file name  
  # no.out <- parallel::mclapply(unique(or.sf), mc.cores = parallel, function(x)
    l2pdfFUN <- function(i, overwrite, keep.img)
    {
    if (any(!overwrite & !file.exists(file.path(path, paste0(i, ".pdf"))), overwrite))
{    pdf(file = file.path(path, paste0(i, ".pdf")), width = 8.5, height = 11)
    
    #order imgs so they look order in the pdf
    subimgs <- imgs[or.sf == i]
    if (length(subimgs) > 1){
    pgs <- substr(subimgs, regexpr("p\\d+\\.jpeg" ,subimgs), nchar(subimgs))
    pgs <- as.numeric(gsub("p|.jpeg|.jpg", "", pgs, ignore.case = TRUE))
    subimgs <- subimgs[order(pgs)]
    }
    
    #plot
    img <- jpeg::readJPEG(subimgs[1])
    par(mar = rep(0, 4))
    plot.new()
    mr <- par("usr")
    graphics::rasterImage(img, mr[1], mr[3], mr[2], mr[4])
    
    #loop over the following pages if more than 1 page
    if (length(subimgs) > 1)
      {
      no.out <- lapply(subimgs[-1], function(y) {
        plot.new()
        par(mar = rep(0, 4))
        mr <- par("usr")
        img2 <- jpeg::readJPEG(y)
        graphics::rasterImage(img2, mr[1], mr[3], mr[2], mr[4])
      })
    }
    dev.off()
  if (!keep.img) unlink(subimgs)
    }
    }
    # )
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  lst <- pbapply::pblapply(X = unique(or.sf), cl = cl, FUN = function(i) 
  { 
    l2pdfFUN(i, overwrite, keep.img)
  }) 
}



##############################################################################################################
#' alternative name for \code{\link{lspec2pdf}}
#'
#' @keywords internal
#' @details see \code{\link{lspec2pdf}} for documentation
#' @export

full_spec2pdf <- lspec2pdf
