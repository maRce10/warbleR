#' Extract the fundamental frequency values as a time series
#' 
#' \code{ffts} extracts the fundamental frequency values as a time series
#' of signals selected by \code{\link{manualoc}} or \code{\link{autodetec}}.
#' @usage ffts(X, wl = 512, flim = c(0, 22), length.out = 20, wn = "hanning", pal =
#'   reverse.gray.colors.2, ovlp = 70, inner.mar = c(5, 4, 4, 2), outer.mar = 
#'   c(0, 0, 0, 0), picsize = 1, res = 100, cexlab = 1, title = TRUE, propwidth = FALSE, 
#'   xl = 1, gr = FALSE, sc = FALSE, bp = c(0, 22), cex = 1, 
#'   threshold = 15, col = "dodgerblue", pch = 16,  mar = 0.05, 
#'   lpos = "topright", it = "jpeg", img = TRUE, parallel = 1)
#' @param  X Data frame with results containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param flim A numeric vector of length 2 for the frequency limit of 
#'   the spectrogram (in kHz), as in \code{\link[seewave]{spectro}}. Default is c(0, 22).
#' @param length.out A character vector of length 1 giving the number of measurements of fundamental 
#' frequency desired (the length of the time series).
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param pal A color palette function to be used to assign colors in the 
#'   plot, as in \code{\link[seewave]{spectro}}. Default is reverse.gray.colors.2.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70. 
#' @param inner.mar Numeric vector with 4 elements, default is c(5,4,4,2). 
#'   Specifies number of lines in inner plot margins where axis labels fall, 
#'   with form c(bottom, left, top, right). See \code{\link[graphics]{par}}.
#' @param outer.mar Numeric vector with 4 elements, default is c(0,0,0,0). 
#'   Specifies number of lines in outer plot margins beyond axis labels, with 
#'   form c(bottom, left, top, right). See \code{\link[graphics]{par}}.
#' @param picsize Numeric argument of length 1. Controls relative size of 
#'   spectrogram. Default is 1.
#' @param res Numeric argument of length 1. Controls image resolution.
#'   Default is 100 (faster) although 300 - 400 is recommended for publication/ 
#'   presentation quality.
#' @param cexlab Numeric vector of length 1 specifying the relative size of axis 
#'   labels. See \code{\link[seewave]{spectro}}.
#' @param title Logical argument to add a title to individual spectrograms. 
#'   Default is \code{TRUE}.
#' @param propwidth Logical argument to scale the width of spectrogram 
#'   proportionally to duration of the selected call. Default is \code{FALSE}.
#' @param xl Numeric vector of length 1. A constant by which to scale 
#'   spectrogram width. Default is 1.
#' @param gr Logical argument to add grid to spectrogram. Default is \code{FALSE}.
#' @param sc Logical argument to add amplitude scale to spectrogram, default is 
#'   \code{FALSE}.
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param cex Numeric vector of length 1, specifies relative size of points 
#'   plotted for frequency measurements and legend font/points, respectively. 
#'   See \code{\link[seewave]{spectro}}.
#' @param threshold amplitude threshold (\%) for fundamental frequency detection. Default is 15.
#' @param col Vector of length 1 specifying colors of points plotted to mark 
#'  fundamental frequency measurements. Default is "dodgerblue".
#' @param pch Numeric vector of length 1 specifying plotting characters for 
#'   the frequency measurements. Default is 16.
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the selections
#'  to set spectrogram limits. Default is 0.05.
#' @param lpos Character vector of length 1 or numeric vector of length 2, 
#'   specifiying position of legend. If the former, any keyword accepted by 
#'   xy.coords can be used (see below). If the latter, the first value will be the x 
#'   coordinate and the second value the y coordinate for the legend's position.
#'   Default is "topright".
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (e.i. no parallel computing).
#'   For windows users the \code{parallelsugar} package should be installed.   
#' @return A data frame with the fundamental frequency values measured across the signals. If img is 
#' \code{FALSE} it also produces image files with the spectrograms of the signals listed in the 
#' input data frame showing the location of the fundamental frequencies.
#' @family spectrogram creators
#' @seealso \code{\link{specreator}} for creating spectrograms from selections,
#'  \code{\link{snrspecs}} for creating spectrograms to 
#'   optimize noise margins used in \code{\link{sig2noise}}
#' @export
#' @name ffts
#' @details This function extracts the fundamental frequency values as a time series. 
#' The function uses the `approx` function to interpolate values between fundamental frequency 
#' measures.
#' @examples
#' \dontrun{
#' # set the temp directory
#' setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2","manualoc.df"))
#' writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
#' writeWave(Phae.long1, "Phae.long1.wav")
#' 
#' # run function 
#' ffts(manualoc.df, length.out = 30, flim = c(1, 12), picsize = 2, res = 100, bp = c(2, 9))
#' 
#' }
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/})


ffts <- function(X, wl = 512, flim = c(0, 22), length.out = 20, wn = "hanning", pal = reverse.gray.colors.2, ovlp = 70, 
                       inner.mar = c(5,4,4,2), outer.mar = c(0,0,0,0), picsize = 1, res = 100, cexlab = 1,
                       title = TRUE, propwidth = FALSE, xl = 1, gr = FALSE, sc = FALSE, 
                       bp = c(0, 22), cex = 1, threshold = 15, col = "dodgerblue",pch = 16,
                       mar = 0.05, lpos = "topright", it = "jpeg", img = TRUE, parallel = 1){     
  
  if(class(X) == "data.frame") {if(all(c("sound.files", "selec", 
                                         "start", "end") %in% colnames(X))) 
  {
    start <- as.numeric(X$start)
    end <- as.numeric(X$end)
    sound.files <- as.character(X$sound.files)
    selec <- as.character(X$selec)
  } else stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                        "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  } else  stop("X is not a data frame")
  
  #if there are NAs in start or end stop
  if(any(is.na(c(end, start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if(all(class(end) != "numeric" & class(start) != "numeric")) stop("'end' and 'selec' must be numeric")
  
  #if any start higher than end stop
  if(any(end - start<0)) stop(paste("The start is higher than the end in", length(which(end - start<0)), "case(s)"))  
  
  #if any selections longer than 20 secs stop
  if(any(end - start>20)) stop(paste(length(which(end - start>20)), "selection(s) longer than 20 sec"))  
  options( show.error.messages = TRUE)
  
  #if bp is not vector or length!=2 stop
  if(!is.null(bp)) {if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}}
  
  #if it argument is not "jpeg" or "tiff" 
  if(!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
  #return warning if not all sound files were found
  recs.wd <- list.files(path = getwd(), pattern = ".wav$", ignore.case = T)
  if(length(unique(sound.files[(sound.files %in% recs.wd)])) != length(unique(sound.files))) 
    message(paste(length(unique(sound.files))-length(unique(sound.files[(sound.files %in% recs.wd)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(sound.files %in% recs.wd) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
    start <- start[d]
    end <- end[d]
    selec <- selec[d]
    sound.files <- sound.files[d]
  }
  
  #if parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  if(parallel > 1)
  { options(warn = -1)
    if(all(Sys.info()[1] == "Windows",requireNamespace("parallelsugar", quietly = TRUE) == TRUE)) 
      lapp <- function(X, FUN) parallelsugar::mclapply(X, FUN, mc.cores = parallel) else
        if(Sys.info()[1] == "Windows"){ 
          message("Windows users need to install the 'parallelsugar' package for parallel computing (you are not doing it now!)")
          lapp <- pbapply::pblapply} else lapp <- function(X, FUN) parallel::mclapply(X, FUN, mc.cores = parallel)} else lapp <- pbapply::pblapply
  
  options(warn = 0)
  
 if(parallel == 1) {if(img) message("Creating spectrograms overlaid with fundamental frequency measurements:") else
    message("Calculating fundamental frequency measurements:")}  
  
  lst<-lapp(1:length(sound.files), function(i){
    
    # Read sound files to get sample rate and length
    r <- tuneR::readWave(file.path(getwd(), sound.files[i]), header = TRUE)
    f <- r$sample.rate
    t <- c(start[i] - mar, end[i] + mar)
    
    #reset coordinates of signals 
    mar1 <- start[i]-t[1]
    mar2 <- mar1 + end[i] - start[i]
    
    if (t[1] < 0) { t[2] <- abs(t[1]) + t[2] 
    mar1 <- mar1  + t[1]
    mar2 <- mar2  + t[1]
    t[1] <- 0
    }
    
    if(t[2] > r$samples/f) t[2] <- r$samples/f
  
    b<- bp 
    if(!is.null(b)) {if(b[2] > ceiling(f/2000) - 1) b[2] <- ceiling(f/2000) - 1 
    b <- b * 1000}
    
    
      r <- tuneR::readWave(as.character(sound.files[i]), from = t[1], to = t[2], units = "seconds")
    
    if(img) {
      #in case bp its higher than can be due to sampling rate
    
    fl<- flim #in case flim its higher than can be due to sampling rate
    if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
    
    # Spectrogram width can be proportional to signal duration
    if(propwidth == TRUE){
      if(it == "tiff") tiff(filename = paste(sound.files[i],"-", selec[i], "-", "ffts", ".tiff", sep = ""), 
                            width = (10.16) * ((t[2]-t[1])/0.27) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res) else
                              jpeg(filename = paste(sound.files[i],"-", selec[i], "-", "ffts", ".jpeg", sep = ""), 
                                   width = (10.16) * ((t[2]-t[1])/0.27) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res)
      
    } else {
      if(it == "tiff") tiff(filename = paste(sound.files[i],"-", selec[i], "-", "ffts", ".tiff", sep = ""), 
                            width = (10.16) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res) else
                              jpeg(filename = paste(sound.files[i],"-", selec[i], "-", "ffts", ".jpeg", sep = ""), 
                                   width = (10.16) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res)
      
    }
    
    # Change relative widths of columns for spectrogram when sc = TRUE
    if(sc == TRUE) wts <- c(3, 1) else wts <- NULL
    
    # Generate spectrogram using seewave
    seewave::spectro(r, f = f, wl = wl, ovlp = ovlp, collevels = seq(-40, 0, 0.5),
                     wn = "hanning", widths = wts, palette = pal, grid = gr, scale = sc, collab = "black", 
                     cexlab = cexlab, cex.axis = 0.5*picsize, flim = fl, tlab = "Time (s)", 
                     flab = "Frequency (kHz)", alab = "")
    
    if(title){
      
      title(paste(sound.files[i], "-", selec[i], "-", "ffts", sep = ""), cex.main = cexlab)
      
    }
    
    # Plot fundamental frequency at each time point     
    ffreq <- seewave::fund(r, from=mar1, to = mar2,  
                           fmax= b[2]*1000, f = f, ovlp = ovlp, threshold = threshold, plot = FALSE) 
    ffreq <- ffreq[!is.na(ffreq[,2]), ]
    ffreq <- ffreq[ffreq[,2] > b[1], ]
    
    apdom <- approx(ffreq[,1], ffreq[,2], n =length.out, method = "linear")
    
    
    points(apdom$x+mar1, apdom$y, col = col, cex = cex, pch = pch) 
    abline(v = c(mar1, mar2), col= "red", lty = "dashed")
    
    # Legend coordinates can be uniquely adjusted 
    legend(lpos, legend = c("Ffreq"),
           pch = pch, col = col, bty = "o", cex = cex)
    
    dev.off()
    } else 
      ffreq <- seewave::fund(r, from=mar1, to = mar2,  
                             fmax= b[2]*1000, f = f, ovlp = ovlp, threshold = threshold, plot = FALSE) 
      ffreq <- ffreq[!is.na(ffreq[,2]), ]
      ffreq <- ffreq[ffreq[,2] > b[1],]
      
      apdom<-approx(ffreq[,1], ffreq[,2], n =length.out, method = "linear")
    return(apdom$y)  
  } )

  df<-data.frame(sound.files, selec, (as.data.frame(matrix(unlist(lst),nrow = length(sound.files), byrow = T))))
    colnames(df)[3:ncol(df)]<-paste("dfreq",1:(ncol(df)-2),sep = "-")
                 return(df)
}

