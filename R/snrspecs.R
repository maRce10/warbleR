#' Spectrograms with background noise margins
#' 
#' \code{snrspecs} creates spectrograms to visualize margins over which background noise
#' will be measured by \code{\link{sig2noise}}.
#' @usage snrspecs(X, wl = 512, flim = c(0, 22), wn = "hanning", ovlp = 70, 
#' inner.mar = c(5, 4, 4, 2), outer.mar = c(0, 0, 0, 0), picsize = 1, 
#' res = 100, cexlab = 1, title = TRUE, 
#'   propwidth= FALSE, xl=1, osci = FALSE, gr = FALSE, sc = FALSE, mar = 0.2,
#'    snrmar = 0.1, it = "jpeg", parallel = 1, path = NULL, pb = TRUE)
#' @param  X 'selection.table' object or data frame with results from \code{\link{manualoc}} or any data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param flim A numeric vector of length 2 for the frequency limit in kHz of 
#'   the spectrogram, as in \code{\link[seewave]{spectro}}. Default is c(0, 22).
##' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70.
#' @param inner.mar Numeric vector with 4 elements, default is c(5,4,4,2). 
#'   Specifies number of lines in inner plot margins where axis labels fall, 
#'   with form c(bottom, left, top, right). See \code{\link[graphics]{par}}.
#' @param outer.mar Numeric vector with 4 elements, default is c(0,0,0,0). 
#'   Specifies number of lines in outer plot margins beyond axis labels, with 
#'   form c(bottom, left, top, right). See \code{\link[graphics]{par}}.
#' @param picsize Numeric argument of length 1, controls relative size of 
#'   spectrogram. Default is 1.
#' @param res Numeric argument of length 1 that controls image resolution.
#'   Default is 100 (faster) although 300 - 400 is recommended for publication/ 
#'   presentation quality.
#' @param cexlab Numeric vector of length 1 specifying relative size of axis 
#'   labels. See \code{\link[seewave]{spectro}}.
#' @param title Logical argument to add a title to individual spectrograms. 
#'   Default is \code{TRUE}.
#' @param propwidth Logical argument to scale the width of spectrogram 
#'   proportionally to duration of the selected call. Default is \code{FALSE}.
#' @param xl Numeric vector of length 1, a constant by which to scale 
#'   spectrogram width if propwidth = \code{TRUE}. Default is 1.
#' @param osci Logical argument to add an oscillogram underneath spectrogram, as
#'   in \code{\link[seewave]{spectro}}. Default is \code{FALSE}.
#' @param gr Logical argument to add grid to spectrogram. Default is \code{FALSE}.
#' @param sc Logical argument to add amplitude scale to spectrogram, default is 
#'   \code{FALSE}.
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the 
#' start and end points of the selections to define spectrogram limits. Default is 0.2. If snrmar 
#' is larger than mar, then mar is set to be equal to snrmar.
#' @param snrmar Numeric vector of length 1. Specifies the margins adjacent to the start and end
#' points of the selections where noise will be measured. Default is 0.1.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @return Spectrograms per selection marked with margins where background noise will be measured.
#' @family spectrogram creators
#' @seealso \code{\link{trackfreqs}} for creating spectrograms to visualize 
#'   frequency measurements by \code{\link{specan}}, \code{\link{specreator}} for 
#'   creating spectrograms after using \code{\link{manualoc}}
#' @export
#' @name snrspecs
#' @details This function can be used to test different margins to facilitate 
#'   accurate SNR measurements when using \code{\link{sig2noise}} down the line.
#'   Setting margins for individual calls that have been previously clipped from
#'   larger files may take some optimization, as for calls within a
#'   larger file that are irregularly separated. Setting inner.mar to 
#'   c(4,4.5,2,1) and outer.mar to c(4,2,2,1) works well when picsize = 2 or 3. 
#'   Title font size, inner.mar and outer.mar (from \code{mar} and \code{oma} in \code{par}) don't work well
#'   when osci or sc = \code{TRUE}, this may take some optimization by the user.
#' @examples
#' \dontrun{
#' # Set temporary working directory
#' # setwd(tempdir())
#'  
#' data(list = c("Phae.long1", "Phae.long2", "selec.table"))
#' writeWave(Phae.long1, "Phae.long1.wav") #save sound.files
#' writeWave(Phae.long2, "Phae.long2.wav") 
#' 
#' # make Phae.long1 and Phae.long2 spectrograms
#' # snrmar needs to be smaller before moving on to sig2noise()
#' 
#' snrspecs(selec.table, flim = c(0, 14), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), 
#' picsize = 2, res = 300, cexlab = 2, mar = 0.2, snrmar = 0.1, it = "jpeg", wl = 300)
#' 
#' # make only Phae.long1 spectrograms
#' # snrmar now doesn't overlap neighboring signals
#' 
#' snrspecs(selec.table[grepl(c("Phae.long1"), selec.table$sound.files), ], flim = c(3, 14),
#' inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), picsize = 2, res = 300, cexlab = 2,
#' mar = 0.2, snrmar = 0.01, wl = 300)
#' 
#' #check this folder!!
#' getwd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) and Grace Smith Vidaurre
#' @source \url{https://en.wikipedia.org/wiki/Signal-to-noise_ratio}
#last modification on jul-5-2016 (MAS)

snrspecs <- function(X, wl = 512, flim = c(0, 22), wn = "hanning", ovlp = 70,
                     inner.mar = c(5,4,4,2), outer.mar = c(0,0,0,0), picsize = 1, res = 100,
                     cexlab = 1, title = TRUE, propwidth = FALSE, xl = 1, osci = FALSE, 
                     gr = FALSE, sc = FALSE, mar = 0.2, snrmar = 0.1, it = "jpeg", parallel = 1,
                     path = NULL, pb = TRUE){
 
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  #check path to working directory
  if(is.null(path)) path <- getwd() else {if(!file.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  #if X is not a data frame
  if(!class(X) %in% c("data.frame", "selection.table")) stop("X is not of a class 'data.frame' or 'selection table")
  
  
  
  if(!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  #if there are NAs in start or end stop
  if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if(all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'end' and 'selec' must be numeric")
  
  #if any start higher than end stop
  if(any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))  
  
  #if it argument is not "jpeg" or "tiff" 
  if(!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
  #wrap img creating function
  if(it == "jpeg") imgfun <- jpeg else imgfun <- tiff
  
  #if any selections longer than 20 secs stop
  if(any(X$end - X$start>20)) stop(paste(length(which(X$end - X$start>20)), "selection(s) longer than 20 sec"))  
  options( show.error.messages = TRUE)
    
  #return warning if not all sound files were found
  fs <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
    cat(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% fs) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else X <- X[d, ]
  
  # If parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  #if parallel and pb in windows
  if(parallel > 1 &  pb & Sys.info()[1] == "Windows") {
    cat("parallel with progress bar is currently not available for windows OS")
    cat("running parallel without progress bar")
    pb <- FALSE
  } 
  
    snrspeFUN <- function(i, X, wl, flim, ovlp, inner.mar, outer.mar, picsize, res, cexlab, xl, mar, snrmar){
    
    r <- tuneR::readWave(as.character(X$sound.files[i])) 
    f <- r@samp.rate
    
    # Read sound files to get sample rate and length
    r <- tuneR::readWave(as.character(X$sound.files[i]), header = TRUE)
    f <- r$sample.rate
    
    fl<- flim #in case flim its higher than can be due to sampling rate
    if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
    
    
    # Set mar equals to snrmar if is smaller
    if(mar < snrmar) mar <- snrmar
    
    # # Correct start and end time if is smaller than 0 or higher than length of rec
    # st <- X$start[i] - mar
    # en <- X$end[i] + mar
    # mar1 <- mar
    # 
    # if (st < 0)  {
    #   mar1 <- mar1  + st
    #   st <- 0
    #   }
    # 
    # mar2 <- mar1 + X$end[i] - X$start[i]
    # 
    # if(en > r$samples/f) en <- r$samples/f

    #reset coordinates of signals 
    st <- X$start[i] - mar
    en <- X$end[i] + mar
    mar1 <- mar
    
    if (st < 0) { 
      mar1 <- mar1  + st
      st <- 0
    }
    
    mar2 <- mar1 + X$end[i] - X$start[i]
    
    if(en > r$samples/f) en <- r$samples/f
    
    
    r <- tuneR::readWave(as.character(X$sound.files[i]), from = st, to = en, units = "seconds")
    
    
# Spectrogram width can be proportional to signal duration
    if(propwidth) pwc <- (10.16) * ((en-st)/0.27) * xl * picsize else pwc <- (10.16) * xl * picsize
    
          imgfun(filename = paste(X$sound.files[i],"-", X$selec[i], "-", "snr.", it, sep = ""), 
           width = pwc, height = (10.16) * picsize, units = "cm", res = res) 

    # Change relative heights of rows for spectrogram when osci = TRUE
    if(osci == TRUE) hts <- c(3, 2) else hts <- NULL
    
    # Change relative widths of columns for spectrogram when sc = TRUE
    if(sc == TRUE) wts <- c(3, 1) else wts <- NULL

    old.par <- par(no.readonly = TRUE) # par settings which could be changed.
    on.exit(par(old.par)) 
    
    # Change inner and outer plot margins
    par(mar = inner.mar)
    par(oma = outer.mar)

    # Generate spectrogram using seewave
    seewave::spectro(r, f = f, wl = wl, ovlp = ovlp, collevels = seq(-40, 0, 0.5), heights = hts,
            wn = "hanning", widths = wts, palette = seewave::reverse.gray.colors.2, osc = osci, grid = gr, scale = sc, 
            collab = "black", cexlab = cexlab, cex.axis = 0.5*picsize, tlab = "Time (s)", 
            flab = "Frequency (kHz)", flim = fl, alab = "", trel = FALSE)
    
    if(title){
      title(paste(X$sound.files[i], "-", X$selec[i], "-", "snr", sep = ""), cex.main = cexlab)
    }
    
    # Add lines to visualize signal
    abline(v = c(mar1, mar2), col = "red", lwd = 1.5, lty = "dashed") 
    
    # Add lines to visualize where noise will be measured in sig2noise
    abline(v = c(mar1 - snrmar, mar2 + snrmar), col = "red", lwd = 1.5, lty = "dashed") 
    
    #add arrows/text indicating noise position
    arrows(x0 =mar1 - snrmar, y0 = fl[1]+fl[2]/6, x1 = mar1, y1 = fl[1]+fl[2]/6, code = 3, 
           col = "red", lty = "solid", lwd = 2, angle = 60)        
  
    arrows(x0 =mar2 + snrmar, y0 = fl[1]+fl[2]/6, x1 = mar2, y1 = fl[1]+fl[2]/6, code = 3, 
           col = "red", lty = "solid", lwd = 2, angle = 60)        
    
    text(x = mar1 - (snrmar * 0.5), y = fl[1]+fl[2]/4, labels = "Noise", col = "red",pos = 3)
    
    text(x = mar2 + (snrmar * 0.5), y = fl[1]+fl[2]/4, labels = "Noise", col = "red",pos = 3) 
    
    dev.off()  
    return (NULL)
  }
     
      
    # Run parallel in windows
    if(parallel > 1) {
      if(Sys.info()[1] == "Windows") {
        
        i <- NULL #only to avoid non-declared objects
        
        cl <- parallel::makeCluster(parallel)
        
        doParallel::registerDoParallel(cl)
        
        a1 <- foreach::foreach(i = 1:nrow(X)) %dopar% {
          snrspeFUN(X = X, i = i, wl = wl, flim = flim, ovlp = ovlp, inner.mar = inner.mar, outer.mar = outer.mar, picsize = picsize, res = res, cexlab = cexlab, xl = xl, mar = mar, snrmar = snrmar)
        }
        
        parallel::stopCluster(cl)
        
      } 
      
      if(Sys.info()[1] == "Linux") {    # Run parallel in Linux
        if(pb)         a1 <- pbmcapply::pbmclapply(1:nrow(X), mc.cores = parallel, function (i) {
          snrspeFUN(X = X, i = i, wl = wl, flim = flim, ovlp = ovlp, inner.mar = inner.mar, outer.mar = outer.mar, picsize = picsize, res = res, cexlab = cexlab, xl = xl, mar = mar, snrmar = snrmar)
        }) else
        a1 <- parallel::mclapply(1:nrow(X), mc.cores = parallel, function (i) {
          snrspeFUN(X = X, i = i, wl = wl, flim = flim, ovlp = ovlp, inner.mar = inner.mar, outer.mar = outer.mar, picsize = picsize, res = res, cexlab = cexlab, xl = xl, mar = mar, snrmar = snrmar)
        })
      }
      if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
      {
        cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))
        
        doParallel::registerDoParallel(cl)
       
         a1 <- foreach::foreach(i = 1:nrow(X)) %dopar% {
          snrspeFUN(X = X, i = i, wl = wl, flim = flim, ovlp = ovlp, inner.mar = inner.mar, outer.mar = outer.mar, picsize = picsize, res = res, cexlab = cexlab, xl = xl, mar = mar, snrmar = snrmar)
        }
        
        parallel::stopCluster(cl)
        
      }
    }
    else {
      if(pb)
      a1 <- pbapply::pblapply(1:nrow(X), function(i) snrspeFUN(X = X, i = i, wl = wl, flim = flim, ovlp = ovlp, inner.mar = inner.mar, outer.mar = outer.mar, picsize = picsize, res = res, cexlab = cexlab, xl = xl, mar = mar, snrmar = snrmar)) else
        a1 <- lapply(1:nrow(X), function(i) snrspeFUN(X = X, i = i, wl = wl, flim = flim, ovlp = ovlp, inner.mar = inner.mar, outer.mar = outer.mar, picsize = picsize, res = res, cexlab = cexlab, xl = xl, mar = mar, snrmar = snrmar))
    }
    
    }

