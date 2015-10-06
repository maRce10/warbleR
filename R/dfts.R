#' Extract the dominant frequency values as a time series
#' 
#' \code{dfts} extract the dominant frequency values as a time series.
#' of signals selected by \code{\link{manualoc}} or \code{\link{autodetec}}.
#' @usage dfts(X, wl = 512, flim = c(0, 22), length.out = 20, wn = "hanning", pal =
#'   reverse.gray.colors.2, ovlp = 70, inner.mar = c(5, 4, 4, 2), outer.mar = 
#'   c(0, 0, 0, 0), picsize = 1, res = 100, cexlab = 1, title = TRUE, propwidth = FALSE, 
#'   xl = 1, gr = FALSE, sc = FALSE, bp = c(0, 22), cex = 1, 
#'   threshold = 15, col = "dodgerblue", pch = 16,  mar = 0.05, 
#'   lpos = "topright", it = "jpeg", img = TRUE)
#' @param  X Data frame with results containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param flim A numeric vector of length 2 for the frequency limit of 
#'   the spectrogram (in kHz), as in \code{\link[seewave]{spectro}}. Default is c(0, 22).
#' @param length.out A character vector of length 1 giving the number of measurements of dominant 
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
#' @param threshold amplitude threshold (\%) for dominant frequency detection. Default is 15.
#' @param col Vector of length 1 specifying colors of points plotted to mark 
#'  dominant frequency measurements. Default is "dodgerblue".
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
#' @return A data frame with the dominant frequency values measured across the signals. If img is 
#' \code{FALSE} it also produces image files with the spectrograms of the signals listed in the 
#' input data frame showing the location of the dominant frequencies.
#' @family spectrogram creators
#' @seealso \code{\link{specreator}} for creating spectrograms from selections,
#'  \code{\link{snrspecs}} for creating spectrograms to 
#'   optimize noise margins used in \code{\link{sig2noise}}
#' @export
#' @name dfts
#' @details This function extracts the dominant frequency values as a time series. 
#' The function uses the `approx` function to interpolate values between dominant frequency 
#' measures.
#' @examples
#' \dontrun{
#' #First create empty folder
#' dir.create(file.path(getwd(),"temp"))
#' setwd(file.path(getwd(),"temp"))
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2","manualoc.df"))
#' writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
#' writeWave(Phae.long1, "Phae.long1.wav")
#' 
#' # make  spectrograms  
#' 
#' dfts(manualoc.df, length.out = 30, flim = c(1, 12), picsize = 2, res = 100, bp = c(2, 9))

#' 
#' # remove example directory
#' unlink(getwd(),recursive = TRUE)
#' }
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/})


dfts <- function(X, wl = 512, flim = c(0, 22), length.out = 20, wn = "hanning", pal = reverse.gray.colors.2, ovlp = 70, 
                       inner.mar = c(5,4,4,2), outer.mar = c(0,0,0,0), picsize = 1, res = 100, cexlab = 1,
                       title = TRUE, propwidth = FALSE, xl = 1, gr = FALSE, sc = FALSE, 
                       bp = c(0, 22), cex = 1, threshold = 15, col = "dodgerblue",
                       pch = 16, mar = 0.05, lpos = "topright", it = "jpeg", img = TRUE){     
  
  if(class(X) == "data.frame") {if(all(c("sound.files", "selec", 
                                         "start", "end") %in% colnames(X))) 
  {
    start <- as.numeric(unlist(X$start))
    end <- as.numeric(unlist(X$end))
    sound.files <- as.character(unlist(X$sound.files))
    selec <- as.character(unlist(X$selec))
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
  if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}
  
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
  
  if(img) message("Creating spectrograms overlaid with dominant frequency measurements:") else
    message("Calculating dominant frequency measurements:")
  lst<-pbapply::pblapply(matrix(c(1:length(sound.files)), ncol=1), function(i){
    
    r <- tuneR::readWave(file.path(getwd(), sound.files[i]))
    
    f <- r@samp.rate
    t <- c(start[i] - mar, end[i] + mar)
    cex <- cex
 
    b<- bp 
    if(b[2] > ceiling(r@samp.rate/2000) - 1) b[2] <- ceiling(r@samp.rate/2000) - 1 
    
    if(img) {
      #in case bp its higher than can be due to sampling rate
    
    fl<- flim #in case flim its higher than can be due to sampling rate
    if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
    
    # Spectrogram width can be proportional to signal duration
    if(propwidth == TRUE){
      if(it == "tiff") tiff(filename = paste(sound.files[i],"-", selec[i], "-", "dfts", ".tiff", sep = ""), 
                            width = (10.16) * ((t[2]-t[1])/0.27) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res) else
                              jpeg(filename = paste(sound.files[i],"-", selec[i], "-", "dfts", ".jpeg", sep = ""), 
                                   width = (10.16) * ((t[2]-t[1])/0.27) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res)
      
    } else {
      if(it == "tiff") tiff(filename = paste(sound.files[i],"-", selec[i], "-", "dfts", ".tiff", sep = ""), 
                            width = (10.16) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res) else
                              jpeg(filename = paste(sound.files[i],"-", selec[i], "-", "dfts", ".jpeg", sep = ""), 
                                   width = (10.16) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res)
      
    }
    
    # Change relative widths of columns for spectrogram when sc = TRUE
    if(sc == TRUE) wts <- c(3, 1) else wts <- NULL
    
    #     old.par <- par(no.readonly = TRUE) # par settings which could be changed.
    #     on.exit(par(old.par)) 
    
    # Change inner and outer plot margins
    par(mar = inner.mar)
    par(oma = outer.mar)
    
    # Generate spectrogram using seewave
    seewave::spectro(r, f = f, wl = wl, ovlp = 70, collevels = seq(-40, 0, 0.5), heights = hts,
                     wn = "hanning", widths = wts, palette = pal, grid = gr, scale = sc, collab = "black", 
                     cexlab = cexlab, cex.axis = 0.5*picsize, tlim = t, flim = fl, tlab = "Time (s)", 
                     flab = "Frequency (kHz)", alab = "")
    
    if(title){
      
      title(paste(sound.files[i], "-", selec[i], "-", "dfts", sep = ""), cex.main = cexlab)
      
    }
    
    # Plot dominant frequency at each time point     
    dfreq <- seewave::dfreq(r, f = f, wl = wl, plot = FALSE, ovlp = ovlp, bandpass = b * 1000, fftw = TRUE, 
                            threshold = threshold, tlim=c(start[i],end[i]))
    
    apdom<-approx(dfreq[,1], dfreq[,2], n =length.out, method = "linear")
    
    
    points(apdom$x+start[i], apdom$y, col = col, cex = cex, pch = pch) 
    abline(v = c(end[i],start[i]), col= "red", lty = "dashed")
    
    # Legend coordinates can be uniquely adjusted 
    legend(lpos, legend = c("Dfreq"),
           pch = pch, col = col, bty = "o", cex = cex)
    
    dev.off()
    } else 
      dfreq <- seewave::dfreq(r, f = f, wl = wl, plot = FALSE, ovlp = 99, bandpass = b * 1000, fftw = TRUE, 
                              threshold = threshold, tlim=c(start[i],end[i]))
    
    apdom<-approx(dfreq[,1], dfreq[,2], n =length.out, method = "linear")
    
    return(apdom$y)  
  } )

  df<-data.frame(sound.files, selec, (as.data.frame(matrix(unlist(lst),nrow = length(sound.files), byrow = T))))
    colnames(df)[3:ncol(df)]<-paste("dfreq",1:(ncol(df)-2),sep = "-")
    
                 message("all done!")
                 return(df)
                 }

