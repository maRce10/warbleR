#' 
#' \code{catalog2pdf} combines \code{\link{catalog}} images into pdfs 
#' @usage catalog2pdf(keep.jpeg = TRUE, overwrite = FALSE, parallel = 1, path = NULL, 
#' pb = TRUE, by.img.suffix = FALSE, ...)
#' @param keep.jpeg Logical argument. Indicates whether jpeg files should be kept (default) or remove.
#'   (including sound file and page number) should be magnified. Default is 1.
#' @param overwrite Logical argument. If \code{TRUE} all jpeg pdf will be produced again 
#'   when code is rerun. If \code{FALSE} only the ones missing will be produced. Default is \code{FALSE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @param by.img.suffix Logical. If \code{TRUE} catalogs with the same image suffix will be 
#' put together in a single pdf (so one pdf per image suffix in the catalog 
#' images). Default is \code{FALSE} (i.e. no suffix).
#' @param ... Additional arguments to be passed to the internal pdf creating function \code{\link[grDevices]{pdf}} for customizing output.
#' @export
#' @name catalog2pdf
#' @details The function combines catalog images in .jpeg format from the \code{\link{catalog}} function into pdfs. Note that using lower resolution and smaller dimension (width and height) when creating catalogs will substantially decrease the size of pdf files (which could be pretty big).
#' @seealso \code{\link{catalog2pdf}}, 
#' \url{https://marce10.github.io/2017-03-17-Creating_song_catalogs/}
#' @examples
#' \dontrun{
#' # Set temporary working directory
#' setwd(tempdir())
#' 
#' # save sound file examples
#' data(list = c("Phae.long1", "Phae.long2"))
#' writeWave(Phae.long1,"Phae.long1.wav") 
#' writeWave(Phae.long2,"Phae.long2.wav")
#' 
#' catalog(X = selec.table, nrow = 2, ncol = 4)
#' 
#' #now create single pdf removing jpeg
#' catalog2pdf(keep.jpeg = FALSE)
#' 
#' check this floder
#' getwd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on nov-13-2016 (MAS)

catalog2pdf <- function(keep.jpeg = TRUE, overwrite = FALSE, parallel = 1, path = NULL, 
                        pb = TRUE, by.img.suffix = FALSE, ...)
{
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
    if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
  setwd(path)} #set working directory
  
  #list jpeg files
  imgs <- list.files(pattern = "\\.jpeg$", ignore.case = TRUE)
  if(length(imgs) == 0) stop("No .jpeg files were found in the working directory")
  
  #remove images that don't have the catalog_pX.jpeg ending
  imgs <- grep(paste("Catalog_p\\d+\\.jpeg"),imgs, value = TRUE)
    
  #remove page info at the end of file names to get sound file names
 if(by.img.suffix)
   or.sf <- gsub("Catalog_p\\d+\\-|\\.jpeg", "" ,imgs) else
  or.sf <- by.img.suffix
  
  #if parallel and pb in windows
  if(parallel > 1 &  pb & Sys.info()[1] == "Windows") {
    message("parallel with progress bar is currently not available for windows OS")
    message("running parallel without progress bar")
    pb <- FALSE
  } 
  
  pdf.options(useDingbats = TRUE)
  
  #loop over each sound file name  
  # no.out <- parallel::mclapply(unique(or.sf), mc.cores = parallel, function(x)
    cat2pdfFUN <- function(i, overwrite, keep.jpeg)
    {
      if(!is.logical(i)) filnam <- paste0(i, ".pdf") else filnam <- "Catalog.pdf"
   
       if(any(!overwrite & !file.exists(filnam), overwrite))
{ 
    
    #order imgs so they look order in the pdf
         if(!is.logical(i)) subimgs <- imgs[or.sf == i] else subimgs <- imgs
    if(length(subimgs) > 1){
    pgs <- substr(subimgs,attr(regexpr("Catalog_p" ,subimgs), "match.length") + 1, nchar(subimgs))
    if(!is.logical(i))
    pgs <- as.numeric(gsub(paste0("-", i, "|\\.jpeg|\\.jpg"), "", pgs, ignore.case = TRUE)) else
      pgs <- as.numeric(gsub(paste0("|\\.jpeg|\\.jpg"), "", pgs, ignore.case = TRUE))
    
    subimgs <- subimgs[order(pgs)]
    }
    
    # get dimensions
    imgdm <- dim(jpeg::readJPEG(subimgs[1], native = T))
         
    dimprop <- imgdm[1]/imgdm[2]
    
    #start graphic device     
   if(!is.logical(i))
     grDevices::pdf(file = paste0(i, ".pdf"), width = 10, height = dimprop * 10, ...) else  grDevices::pdf(file = "Catalog.pdf", width = 10, height = dimprop * 10, ...)     


    #plot
    img <- jpeg::readJPEG(subimgs[1])
    par(mar = rep(0, 4), oma = rep(0, 4), pty = "m")
    plot.new()
    mr <- par("usr")
    graphics::rasterImage(img, mr[1], mr[3], mr[2], mr[4])
    
    #loop over the following pages if more than 1 page
    if(length(subimgs) > 1)
      {
      no.out <- lapply(subimgs[-1], function(y) {
        plot.new()
        mr <- par("usr")
        img2 <- jpeg::readJPEG(y)
        graphics::rasterImage(img2, mr[1], mr[3], mr[2], mr[4])
      })
    }
    dev.off()
  if(!keep.jpeg) unlink(subimgs)
    }
    }
    # )
  
  
  # Run parallel in windows
  if(parallel > 1) {
    if(Sys.info()[1] == "Windows") {
      
      i <- NULL #only to avoid non-declared objects
      
      cl <- parallel::makeCluster(parallel)
      
      doParallel::registerDoParallel(cl)
      
      lst <- foreach::foreach(i = unique(or.sf)) %dopar% {
        cat2pdfFUN(i, overwrite, keep.jpeg)
      }
      
      parallel::stopCluster(cl)
      
    } 
    if(Sys.info()[1] == "Linux") {    # Run parallel in Linux
      
      if(pb)
      lst <- pbmcapply::pbmclapply(unique(or.sf), mc.cores = parallel,function (i) {
        cat2pdfFUN(i, overwrite, keep.jpeg)
      }) else
      
      lst <- parallel::mclapply(unique(or.sf), mc.cores = parallel, function (i) {
        cat2pdfFUN(i, overwrite, keep.jpeg)
      })
    }
    if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
    {
      cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))
      
      doParallel::registerDoParallel(cl)
      
      lst <- foreach::foreach(i = unique(or.sf)) %dopar% {
        cat2pdfFUN(i, overwrite, keep.jpeg)
      }
      
      parallel::stopCluster(cl)
      
    }
  } else 
    if(pb)
    lst <- pbapply::pblapply(unique(or.sf), function(i) cat2pdfFUN(i, overwrite, keep.jpeg)) else
      lst <- lapply(unique(or.sf), function(i) cat2pdfFUN(i, overwrite, keep.jpeg))
  
  if(!is.null(path)) setwd(wd)
}
