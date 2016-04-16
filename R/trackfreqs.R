#' Spectrograms with frequency measurements
#' 
#' \code{trackfreqs} creates spectrograms to visualize dominant and fundametal frequency measurements (contours)
#' of signals selected by \code{\link{manualoc}} or \code{\link{autodetec}}.
#' @usage trackfreqs(X, wl = 512, flim = c(0, 22), wn = "hanning", pal =
#'   reverse.gray.colors.2, ovlp = 70, inner.mar = c(5, 4, 4, 2), outer.mar = 
#'   c(0, 0, 0, 0), picsize = 1, res = 100, cexlab = 1, title = TRUE, propwidth = FALSE, 
#'   xl = 1, osci = FALSE, gr = FALSE, sc = FALSE, bp = c(0, 22), cex = c(0.8, 1), 
#'   threshold = 15, contour = "both", col = c("chartreuse3", "dodgerblue"),
#'    pch = c(17, 16),  mar = 0.05, lpos = "topright", it = "jpeg", parallel = 1)
#' @param  X Data frame with results containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param flim A numeric vector of length 2 for the frequency limit of 
#'   the spectrogram (in kHz), as in \code{\link[seewave]{spectro}}. Default is c(0, 22).
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
#' @param osci Logical argument to add an oscillogram underneath spectrogram, as
#'   in \code{\link[seewave]{spectro}}. Default is \code{FALSE}.
#' @param gr Logical argument to add grid to spectrogram. Default is \code{FALSE}.
#' @param sc Logical argument to add amplitude scale to spectrogram, default is 
#'   \code{FALSE}.
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param cex Numeric vector of length 1, specifies relative size of points 
#'   plotted for frequency measurements and legend font/points, respectively. 
#'   See \code{\link[seewave]{spectro}}.
#' @param threshold amplitude threshold (\%) for fundamental frequency and 
#'   dominant frequency detection. Default is 15.
#' @param contour Character vector, one of "df", "ff" or "both", specifying whether the
#'  dominant or fundamental frequencies or both should be plotted. Default is "both". 
#' @param col Vector of length 2 specifying colors of points plotted to mark 
#'   fundamental and dominant frequency measurements. Default is c("chartreuse3",
#'   "dodgerblue").
#' @param pch Numeric vector of length 2 specifying plotting characters for 
#'   the frequency measurements. Default is c(17, 16).
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the selections
#'  to set spectrogram limits. Default is 0.05.
#' @param lpos Character vector of length 1 or numeric vector of length 2, 
#'   specifiying position of legend. If the former, any keyword accepted by 
#'   xy.coords can be used (see below). If the latter, the first value will be the x 
#'   coordinate and the second value the y coordinate for the legend's position.
#'   Default is "topright".
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (e.i. no parallel computing).
#' For windows OS the \code{parallelsugar} package should be installed.
#'   Note that creating images is not compatible with parallel computing 
#'   (parallel > 1) in OSX (mac).    
#' @return Spectrograms of the signals listed in the input data frame showing the location of 
#' the dominant and fundamental frequencies.
#' @family spectrogram creators
#' @seealso \code{\link{specreator}} for creating spectrograms from selections,
#'  \code{\link{snrspecs}} for creating spectrograms to 
#'   optimize noise margins used in \code{\link{sig2noise}}
#' @export
#' @name trackfreqs
#' @details This function provides visualization of frequency measurements 
#'   made by \code{\link{specan}}. Arguments that are accepted by xy.coords and can be 
#'   used for lpos are: "bottomright", "bottom", "bottomleft", "left", 
#'   "topleft", "top", "topright", "right" and "center". Setting inner.mar to 
#'   c(4,4.5,2,1) and outer.mar to c(4,2,2,1) works well when picsize = 2 or 3. 
#'   Title font size, inner.mar and outer.mar (from mar and oma) don't work well
#'   when osci or sc = TRUE, this may take some optimization by the user.
#' @examples
#' \dontrun{
#' #Set temporal folder as working directory
#' setwd(tempdir())
#' 
#' #load data
#' data("Cryp.soui")
#' writeWave(Cryp.soui, "Cryp.soui.wav") #save sound files 
#' 
#' #autodetec location of signals
#' ad <- autodetec(threshold = 6, bp = c(1, 3), flim = c(0, 5), mindur = 1.2,
#'  maxdur = 3, sxrow = 5, rows = 3, ssmooth = 600, redo = TRUE)
#' 
#' #track dominant frequency graphs
#' trackfreqs(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, it = "tiff",
#'  bp = c(1, 3), contour = "df")
#'  
#'# Check this folder
#'getwd()
#'
#'#track both frequencies 
#'trackfreqs(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, it = "tiff",
#' bp = c(1, 3), contour = "both")
#' 
#' }
#' @author Grace Smith Vidaurre and Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})

trackfreqs <- function(X, wl = 512, flim = c(0, 22), wn = "hanning", pal = reverse.gray.colors.2, ovlp = 70, 
                       inner.mar = c(5,4,4,2), outer.mar = c(0,0,0,0), picsize = 1, res = 100, cexlab = 1,
                       title = TRUE, propwidth = FALSE, xl = 1, osci = FALSE, gr = FALSE, sc = FALSE, 
                     bp = c(0, 22), cex = c(0.8, 1), threshold = 15, contour = "both", 
                     col = c("chartreuse3", "dodgerblue"),  pch = c(17, 16), 
                     mar = 0.05, lpos = "topright", it = "jpeg", parallel = 1){     

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
    (paste(length(unique(sound.files))-length(unique(sound.files[(sound.files %in% recs.wd)])), 
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
  
  # If parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  #if parallel in OSX
  if(all(parallel > 1, !Sys.info()[1] %in% c("Linux","Windows"))) {
    parallel <- 1
    ("creating images is not compatible with parallel computing (parallel > 1) in OSX (mac)")
  }
  
  # If parallel was called
  if(parallel > 1)
  { options(warn = -1)
    if(all(Sys.info()[1] == "Windows",requireNamespace("parallelsugar", quietly = TRUE) == TRUE)) 
      lapp <- function(X, FUN) parallelsugar::mclapply(X, FUN, mc.cores = parallel) else
        if(Sys.info()[1] == "Windows"){ 
          ("Windows users need to install the 'parallelsugar' package for parallel computing (you are not doing it now!)")
          lapp <- pbapply::pblapply} else lapp <- function(X, FUN) parallel::mclapply(X, FUN, mc.cores = parallel)} else lapp <- pbapply::pblapply
  
  options(warn = 0)
  
  if(parallel == 1) ("Creating spectrograms overlaid with acoustic measurements:")
  invisible(lapp(1:length(sound.files), function(i){
    
    # Read sound files, initialize frequency and time limits for spectrogram
    r <- tuneR::readWave(file.path(getwd(), sound.files[i]), header = T)
    f <- r$sample.rate
    t <- c(start[i] - mar, end[i] + mar)
    if(t[1]<0) t[1]<-0
    if(t[2]>r$samples/f) t[2]<-r$samples/f
    
    
    mar1 <- mar
    mar2 <- mar1 + end[i] - start[i]
    
    if (t[1] < 0) { 
      mar1 <- mar1  + t[1]
      mar2 <- mar2  + t[1]
      t[1] <- 0
    }
    
    if(t[2] > r$samples/f) t[2] <- r$samples/f
    
    # read rec segment
    r <- tuneR::readWave(as.character(sound.files[i]), from = t[1], to = t[2], units = "seconds")
    #in case bp its higher than can be due to sampling rate
    b<- bp 
    if(b[2] > ceiling(r@samp.rate/2000) - 1) b[2] <- ceiling(r@samp.rate/2000) - 1 
    
    fl<- flim #in case flim its higher than can be due to sampling rate
    if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
    
    
    # Spectrogram width can be proportional to signal duration
    if(propwidth == TRUE){
      if(it == "tiff") tiff(filename = paste(sound.files[i],"-", selec[i], "-", "trackfreqs", ".tiff", sep = ""), 
           width = (10.16) * ((t[2]-t[1])/0.27) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res) else
             jpeg(filename = paste(sound.files[i],"-", selec[i], "-", "trackfreqs", ".jpeg", sep = ""), 
                  width = (10.16) * ((t[2]-t[1])/0.27) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res)
      
    } else {
      if(it == "tiff") tiff(filename = paste(sound.files[i],"-", selec[i], "-", "trackfreqs", ".tiff", sep = ""), 
           width = (10.16) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res) else
             jpeg(filename = paste(sound.files[i],"-", selec[i], "-", "trackfreqs", ".jpeg", sep = ""), 
                  width = (10.16) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res)
      
    }
    
    # Change relative heights of rows for spectrogram when osci = TRUE
    if(osci == TRUE) hts <- c(3, 2) else hts <- NULL
    
    # Change relative widths of columns for spectrogram when sc = TRUE
    if(sc == TRUE) wts <- c(3, 1) else wts <- NULL
    
#     old.par <- par(no.readonly = TRUE) # par settings which could be changed.
#     on.exit(par(old.par)) 
    
    # Change inner and outer plot margins
    par(mar = inner.mar)
    par(oma = outer.mar)
    
    
    # Generate spectrogram using seewave
    seewave::spectro(r, f = f, wl = wl, ovlp = 70, collevels = seq(-40, 0, 0.5), heights = hts,
            wn = "hanning", widths = wts, palette = pal, osc = osci, grid = gr, scale = sc, collab = "black", 
            cexlab = cexlab, cex.axis = 0.5*picsize, flim = fl, tlab = "Time (s)", 
            flab = "Frequency (kHz)", alab = "")
    
    if(title){
      
      title(paste(sound.files[i], "-", selec[i], "-", "trackfreqs", sep = ""), cex.main = cexlab)
      
    }
    
    # Calculate fundamental frequencies at each time point
if(contour %in% c("both", "ff"))
{        ffreq <- seewave::fund(r, from=mar1, to = mar2,  
              fmax= b[2]*1000, f = f, ovlp = 70, threshold = threshold, plot = FALSE) 
    ffreq <- ffreq[ffreq[,2] > b[1],]
    
    # Plot all fundamental frequency values
    points(c(ffreq[,1])+mar1, c(ffreq[,2]), col = col[1], cex = cex[1], pch = pch[1])
    
    # Plot extreme values fundamental frequency
    points(c(ffreq[c(which.max(ffreq[,2]),which.min(ffreq[,2])),1])+mar1, c(ffreq[c(which.max(ffreq[,2]),which.min(ffreq[,2])),2]), col = "yellow", cex = cex[1]+1, pch = pch[1]) 
  
    points(c(ffreq[c(which.max(ffreq[,2]),which.min(ffreq[,2])),1])+mar1, c(ffreq[c(which.max(ffreq[,2]),which.min(ffreq[,2])),2]), col = col[1], cex = cex[1], pch = pch[1]) 
}    
    
    # Calculate dominant frequency at each time point     
    if(contour %in% c("both", "df"))
{       dfreq <- seewave::dfreq(r, f = f, wl = wl, ovlp = 70, plot = FALSE, bandpass = b * 1000, fftw = TRUE, 
                   threshold = threshold, tlim = c(mar1, mar2))

    # Plot all dominant frequency values
    points(dfreq[,1] + mar1, dfreq[,2], col = col[2], cex = cex[1], pch = pch[2]) 
    
    # Plot extreme values dominant frequency
    points(c(dfreq[c(which.max(dfreq[,2]),which.min(dfreq[,2])),1])+mar1, c(dfreq[c(which.max(dfreq[,2]),which.min(dfreq[,2])),2]), col = "yellow", cex = cex[1]+1, pch = pch[2]) 
    
    points(c(dfreq[c(which.max(dfreq[,2]),which.min(dfreq[,2])),1])+mar1, c(dfreq[c(which.max(dfreq[,2]),which.min(dfreq[,2])),2]), col = col[2], cex = cex[1], pch = pch[2]) 
        }
    
    abline(v = c(mar1, mar2), col= "red", lty = "dashed")
    
    # Legend coordinates can be uniquely adjusted 
    if(contour == "both")
    legend(lpos, legend = c("Ffreq", "Dfreq"),
           pch = pch, col = col[1:2], bty = "o", cex = cex[2])

    if(contour == "ff")
      legend(lpos, legend = "Ffreq",
             pch = pch, col = col[1], bty = "o", cex = cex[2])

    if(contour == "df")
      legend(lpos, legend = "Dfreq",
             pch = pch, col = col[2], bty = "o", cex = cex[2])
    
    invisible() # execute par(old.par)  
    dev.off()
    return(NULL)
    
  }))

}
