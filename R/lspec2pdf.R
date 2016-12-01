# Combine lspec images to single pdf files
#' 
#' \code{lspec2pdf} combines lspec images in .jpeg format to a single pdf file. 
#' @usage lspec2pdf(keep.jpeg = TRUE, overwrite = FALSE, parallel = 1, path = NULL)
#' @param keep.jpeg Logical argument. Indicates whether jpeg files should be kept (default) or remove.
#'   (including sound file and page number) should be magnified. Default is 1.
#' @param overwrite Logical argument. If \code{TRUE} all jpeg pdf will be produced again 
#'   when code is rerun. If \code{FALSE} only the ones missing will be produced. Default is \code{FALSE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @export
#' @name lspec2pdf
#' @details The function combines  spectrograms for complete sound files from the \code{\link{lspec}} function into
#' a single pdf (for each sound file).
#' @examples
#' \dontrun{
#' # First create empty folder
#' setwd(tempdir())
#' 
#' # save sound file examples
#' data(list = c("Phae.long1", "Phae.long2"))
#' writeWave(Phae.long1,"Phae.long1.wav") 
#' writeWave(Phae.long2,"Phae.long2.wav")
#' 
#' lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300, it = "jpeg")
#' 
#' #now create single pdf removing jpeg
#' lspec2pdf(keep.jpeg = FALSE)
#' 
#' check this floder
#' getwd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on nov-13-2016 (MAS)

lspec2pdf <- function(keep.jpeg = TRUE, overwrite = FALSE, parallel = 1, path = NULL)
{
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
    if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
  setwd(path)} #set working directory
  
  #list jpeg files
  imgs <- list.files(pattern = "\\.jpeg$", ignore.case = TRUE)
  if(length(imgs) == 0) stop("No .jpeg files were found in the working directory")
  
  #remove images that don't have the pX.jpeg ending
  imgs <- grep("p\\d+\\.jpeg" ,imgs, value = TRUE)
  
  #remove page info at the end of file names to get sound file names
  or.sf <- gsub("-p\\d+\\.jpeg", "" ,imgs)

  #loop over each sound file name  
  # no.out <- parallel::mclapply(unique(or.sf), mc.cores = parallel, function(x)
    l2pdfFUN <- function(i, overwrite, keep.jpeg)
    {
    if(any(!overwrite & !file.exists(paste0(i, ".pdf")), overwrite))
{    pdf(file = paste0(i, ".pdf"), width = 8.5, height = 11)
    
    #order imgs so they look order in the pdf
    subimgs <- imgs[or.sf == i]
    if(length(subimgs) > 1){
    pgs <- substr(subimgs, regexpr("p\\d+\\.jpeg" ,subimgs), nchar(subimgs))
    pgs <- as.numeric(gsub("p|.jpeg|.jpg", "", pgs, ignore.case = TRUE))
    subimgs <- subimgs[order(pgs)]
    }
    
    #plot
    plot.new()
    img <- jpeg::readJPEG(subimgs[1])
    par(mar = rep(0, 4))
    mr <- par("usr")
    graphics::rasterImage(img, mr[1]- 0.12, mr[3] - 0.14, mr[2] + 0.07, mr[4] + 0.1)
    
    #loop over the following pages if more than 1 page
    if(length(subimgs) > 1)
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
        l2pdfFUN(i, overwrite, keep.jpeg)
      }
      
      parallel::stopCluster(cl)
      
    } 
    if(Sys.info()[1] == "Linux") {    # Run parallel in Linux
      
      lst <- parallel::mclapply(unique(or.sf), function (i) {
        l2pdfFUN(i, overwrite, keep.jpeg)
      })
    }
    if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
    {
      cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))
      
      doParallel::registerDoParallel(cl)
      
      lst <- foreach::foreach(i = unique(or.sf)) %dopar% {
        l2pdfFUN(i, overwrite, keep.jpeg)
      }
      
      parallel::stopCluster(cl)
      
    }
  } else 
    lst <- pbapply::pblapply(unique(or.sf), function(i) l2pdfFUN(i, overwrite, keep.jpeg))
  
  
  if(!is.null(path)) on.exit(setwd(wd))
}
