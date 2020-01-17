#' 
#' \code{catalog2pdf} combines \code{\link{catalog}} images into pdfs 
#' @usage catalog2pdf(keep.img = TRUE, overwrite = FALSE, parallel = 1, path = NULL, 
#' pb = TRUE, by.img.suffix = FALSE, ...)
#' @param keep.img Logical argument. Indicates whether jpeg files should be kept (default) or remove.
#'   (including sound file and page number) should be magnified.
#' @param overwrite Logical argument. If \code{TRUE} all jpeg pdf will be produced again 
#'   when code is rerun. If \code{FALSE} only the ones missing will be produced. Default is \code{FALSE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the catalog image files are located. 
#' If \code{NULL} (default) then the current working directory is used. 
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param by.img.suffix Logical. If \code{TRUE} catalogs with the same image suffix will be 
#' put together in a single pdf (so one pdf per image suffix in the catalog 
#' images). Default is \code{FALSE} (i.e. no suffix).
#' @param ... Additional arguments to be passed to the internal pdf creating function \code{\link[grDevices]{pdf}} for customizing output.
#' @export
#' @name catalog2pdf
#' @details The function combines catalog images in .jpeg format from the \code{\link{catalog}} function into pdfs. Note that using lower resolution and smaller dimension (width and height) when creating catalogs will substantially decrease the size of pdf files (which could be pretty big).
#' @seealso \code{\link{catalog2pdf}}, 
#' \href{https://marce10.github.io/2017/03/17/Creating_song_catalogs.html}{blog post on catalogs}
#' @examples
#' \dontrun{
#' # save sound file examples
#' data(list = c("Phae.long1", "Phae.long2"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) 
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' 
#' catalog(X = lbh_selec_table, nrow = 2, ncol = 4)
#' 
#' # now create single pdf removing jpeg
#' catalog2pdf(keep.img = FALSE, path = tempdir())
#' 
#' # check this floder
#' tempdir()
#' }
#' @references {Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.}
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
# last modification on mar-13-2018 (MAS)

catalog2pdf <- function(keep.img = TRUE, overwrite = FALSE, parallel = 1, path = NULL, 
                        pb = TRUE, by.img.suffix = FALSE, ...)
{
  # error message if jpeg package is not installed
  if (!requireNamespace("jpeg",quietly = TRUE))
    stop("must install 'jpeg' to use this function")
  
  # reset pbapply options
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(catalog2pdf)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
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
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) 
      stop("'path' provided does not exist") 
    
  #list jpeg files
  imgs <- list.files(pattern = "\\.jpeg$|\\.jpeg$", path = path, ignore.case = TRUE)
  if (length(imgs) == 0) stop("No .jpeg files were found in the working directory")
  
  #remove images that don't have the catalog_pX.jpeg ending
  imgs <- grep("Catalog_p\\d+", imgs, value = TRUE)
    
  #remove page info at the end of file names to get sound file names
 if (by.img.suffix)
   or.sf <- gsub("Catalog_p\\d+\\-|\\.jpeg", "" ,imgs) else
  or.sf <- by.img.suffix
  
  pdf.options(useDingbats = TRUE)
  
  #loop over each sound file name  
     cat2pdfFUN <- function(i, overwrite, keep.img)
    {
      if (!is.logical(i)) filnam <- paste0(i, ".pdf") else filnam <- "Catalog.pdf"
   
       if (any(!overwrite & !file.exists(filnam), overwrite))
{ 
    
    #order imgs so they look order in the pdf
    if (!is.logical(i)) subimgs <- imgs[or.sf == i] else subimgs <- imgs
    
    if (length(subimgs) > 1){
      
    pgs <- as.numeric(sapply(strsplit(substr(subimgs, start = regexpr("Catalog_p" ,subimgs) + 9, nchar(subimgs)), "-|.jpeg"), "[", 1))

    if (!is.logical(i))
    pgs <- as.numeric(gsub(paste0("-", i, "|\\.jpeg|\\.jpg"), "", pgs, ignore.case = TRUE)) else
      pgs <- as.numeric(gsub(paste0("|\\.jpeg|\\.jpg"), "", pgs, ignore.case = TRUE))
    
    subimgs <- subimgs[order(pgs)]
    }
    
    # get dimensions
    imgdm <- dim(jpeg::readJPEG(file.path(path, subimgs[1]), native = T))
         
    dimprop <- imgdm[1]/imgdm[2]
    
    #start graphic device     
   if (!is.logical(i))
     grDevices::pdf(file = file.path(path, paste0(i, ".pdf")), width = 10, height = dimprop * 10, ...) else  grDevices::pdf(file = file.path(path, "Catalog.pdf"), width = 10, height = dimprop * 10, ...)     
    #plot
    img <- jpeg::readJPEG(file.path(path, subimgs[1]))
    par(mar = rep(0, 4), oma = rep(0, 4), pty = "m")
    plot.new()
    mr <- par("usr")
    graphics::rasterImage(img, mr[1], mr[3], mr[2], mr[4])
    
    #loop over the following pages if more than 1 page
    if (length(subimgs) > 1)
      {
      no.out <- lapply(subimgs[-1], function(y) {
        plot.new()
        mr <- par("usr")
        img2 <- jpeg::readJPEG(file.path(path, y))
        graphics::rasterImage(img2, mr[1], mr[3], mr[2], mr[4])
      })
    }
    dev.off()
  if (!keep.img) unlink(subimgs)
    }
    }

     # set pb options 
     pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
     # set clusters for windows OS
     if (Sys.info()[1] == "Windows" & parallel > 1)
       cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
     
     # run loop apply function
     a1 <- pbapply::pblapply(X = unique(or.sf), cl = cl, FUN = function(i) 
     { 
       cat2pdfFUN(i, overwrite, keep.img)
     }) 
}
