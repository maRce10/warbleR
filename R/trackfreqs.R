#' Spectrograms with frequency measurements
#' 
#' \code{trackfreqs} creates spectrograms to visualize dominant and fundametal frequency measurements (contours)
#' of signals selected by \code{\link{manualoc}} or \code{\link{autodetec}}.
#' @usage trackfreqs(X, wl = 512, flim = c(0, 22), wn = "hanning", pal =
#'   reverse.gray.colors.2, ovlp = 70, inner.mar = c(5, 4, 4, 2), outer.mar = 
#'   c(0, 0, 0, 0), picsize = 1, res = 100, cexlab = 1, title = TRUE, propwidth = FALSE, 
#'   xl = 1, osci = FALSE, gr = FALSE, sc = FALSE, bp = c(0, 22), cex = c(0.6, 1), 
#'   threshold = 15, contour = "both", col = c("skyblue", "red2"),
#'    pch = c(21, 24),  mar = 0.05, lpos = "topright", it = "jpeg", parallel = 1, 
#'    path = NULL, img.suffix = NULL, custom.contour = NULL, pb = TRUE, type = "p", 
#'    leglab = c("Ffreq", "Dfreq"), col.alpha = 0.6, line = TRUE)
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
#'   frequency bandpass filter (in kHz) or "frange" to indicate that values in low.f 
#'   and high.f columns will be used as bandpass limits. Default is c(0, 22).
#' @param cex Numeric vector of length 2, specifies relative size of points 
#'   plotted for frequency measurements and legend font/points, respectively. 
#'   See \code{\link[seewave]{spectro}}.
#' @param threshold amplitude threshold (\%) for fundamental frequency and 
#'   dominant frequency detection. Default is 15.
#' @param contour Character vector, one of "df", "ff" or "both", specifying whether the
#'  dominant or fundamental frequencies or both should be plotted. Default is "both". 
#' @param col Vector of length 1 or 2 specifying colors of points plotted to mark 
#'   fundamental and dominant frequency measurements respetively (if both are plotted). Default is c("skyblue",
#'   "red2").
#' @param pch Numeric vector of length 1 or 2 specifying plotting characters for 
#'   the frequency measurements. Default is c(21, 24).
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
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param img.suffix A character vector of length 1 with a suffix (label) to add at the end of the names of 
#' image files. Default is \code{NULL}.
#' @param custom.contour A data frame with frequency contours for exactly the same sound files and selection as in X. 
#' The frequency values are assumed to be equally spaced in between the start and end of the signal. The 
#' first 2 colums of the data frame should contain the 'sound.files' and 'selec' columns and should be 
#' identical to the corresponding columns in X (same order). 
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @param type A character vector of length 1 indicating the type of frequency contour plot to be drawn. 
#' Possible types are "p" for points, "l" for lines and "b" for both.
#' @param leglab A character vector of length 1 or 2 containing the label(s) of the frequency contour legend 
#' in the output image.
#' @param col.alpha A numeric vector of length 1  within [0,1] indicating how transparent the lines/points should be.
#' @param line Logical argument to add red lines (or box if low.f and high.f columns are provided) at start and end times of selection. Default is \code{TRUE}.
#' @return Spectrograms of the signals listed in the input data frame showing the location of 
#' the dominant and fundamental frequencies.
#' @family spectrogram creators
#' @seealso \code{\link{specreator}} for creating spectrograms from selections,
#'  \code{\link{snrspecs}} for creating spectrograms to 
#'   optimize noise margins used in \code{\link{sig2noise}}
#' @export
#' @name trackfreqs
#' @details This function provides visualization of frequency measurements as the ones 
#'   made by \code{\link{specan}}. Frequency measures can be made by the function or input by the 
#'   user (see 'custom.contour' argument)  Arguments that are accepted by xy.coords and can be 
#'   used for 'lpos' are: "bottomright", "bottom", "bottomleft", "left", 
#'   "topleft", "top", "topright", "right" and "center". Setting inner.mar to 
#'   c(4,4.5,2,1) and outer.mar to c(4,2,2,1) works well when picsize = 2 or 3. 
#'   Title font size, inner.mar and outer.mar (from mar and oma) don't work well
#'   when osci or sc = TRUE, this may take some optimization by the user. Note that if no amplitude was detected
#'   for a particular time bin, then the image will show a dark dot at the bottom of the time bin.
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
#' ad <- autodetec(threshold = 6, bp = c(1, 3), mindur = 1.2,
#' maxdur = 3, img = FALSE, ssmooth = 600, wl = 300, flist = "Cryp.soui.wav")
#' 
#' #track dominant frequency graphs
#' trackfreqs(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, it = "tiff",
#' bp = c(1, 3), contour = "df", wl = 300)
#'
#' #using users frequency data (custom.contour argument) 
#' #first get contours using dfts
#' df <- dfts(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, img = FALSE,
#' bp = c(1, 3),  wl = 300)
#'
#'# now input the dfts output into trackfreqs         
#'trackfreqs(X = ad[!is.na(ad$start),], custom.contour = df ,flim = c(0, 5), ovlp = 90, it = "tiff")
#' 
#'# Check this folder
#' getwd()
#'
#'#track both frequencies 
#'trackfreqs(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, it = "tiff",
#' bp = c(1, 3), contour = "both", wl = 300)
#' 
#' }
#' @author Grace Smith Vidaurre and Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-25-2016 (MAS)

trackfreqs <- function(X, wl = 512, flim = c(0, 22), wn = "hanning", pal = reverse.gray.colors.2, ovlp = 70, 
                       inner.mar = c(5,4,4,2), outer.mar = c(0, 0, 0, 0), picsize = 1, res = 100, cexlab = 1,
                       title = TRUE, propwidth = FALSE, xl = 1, osci = FALSE, gr = FALSE, sc = FALSE, 
                       bp = c(0, 22), cex = c(0.6, 1), threshold = 15, contour = "both", 
                       col = c("skyblue", "red2"),  pch = c(21, 24), mar = 0.05, lpos = "topright", 
                       it = "jpeg", parallel = 1, path = NULL, img.suffix = NULL, custom.contour = NULL, pb = TRUE,
                       type = "p", leglab = c("Ffreq", "Dfreq"), col.alpha = 0.6, line = TRUE){     
  
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
  if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
    setwd(path)} #set working directory
  
  #if X is not a data frame
  if(!class(X) == "data.frame") stop("X is not a data frame")
  
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
  
  #if any selections longer than 20 secs stop
  if(any(X$end - X$start>20)) stop(paste(length(which(X$end - X$start>20)), "selection(s) longer than 20 sec"))  
  
  # bp checking
  if(bp[1] != "frange")
  {if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")} 
  } else
  {if(!any(names(X) == "low.f") & !any(names(X) == "high.f")) stop("'bp' = frange requires low.f and high.f columns in X")
    if(any(is.na(c(X$low.f, X$high.f)))) stop("NAs found in low.f and/or high.f") 
    if(any(c(X$low.f, X$high.f) < 0)) stop("Negative values found in low.f and/or high.f") 
    if(any(X$high.f - X$low.f < 0)) stop("high.f should be higher than low.f")
    }
  
  #if it argument is not "jpeg" or "tiff" 
  if(!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
  #wrap img creating function
  if(it == "jpeg") imgfun <- jpeg else imgfun <- tiff
  
  #if type not l b or p
  if(!any(type %in% c("p", "l", "b"))) stop(paste("Type", type, "not allowed"))  

  #join img.suffix and it
  if(is.null(img.suffix))
    img.suffix2 <- paste("trackfreqs", it, sep = ".") else   img.suffix2 <- paste(img.suffix, it, sep = ".")
  
  #return warning if not all sound files were found
  recs.wd <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files))) 
    message(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
           ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% recs.wd) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else X <- X[d, ]
  
  # If parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # Compare custom.contour to X
  if(!is.null(custom.contour)){
    #check if sound.files and selec columns are present and in the right order
    if(!identical(names(custom.contour)[1:2], c("sound.files", "selec"))) stop("'sound.files' and/or 'selec' columns are not found in custom.contour")

      #check if the info in sound.files and selec columns is the same for X and custom.contour
      #remove custom.contour selections not in X
      custom.contour <- custom.contour[paste(custom.contour[,c("sound.files")], custom.contour[,c("selec")]) %in% paste(X[,c("sound.files")], X[,c("selec")])]

      #stop if not the same number of selections
      if(nrow(X) > nrow(custom.contour)) stop("selection(s) in X but not in custom.contour")
      
      #order custom.contour as in X
      custom.contour <- custom.contour[match(paste(custom.contour[,c("sound.files")], custom.contour[,c("selec")]), paste(X[,c("sound.files")], X[,c("selec")])),]      
    }
  
  # adjust if only 1 pch was specfified
  if(length(pch) == 1) pch <- c(pch, pch)
 
  # adjust if only 1 color was specified
  if(length(col) == 1) col <- c(col, col)
  
  # adjust if only 1 leglab was specified
  if(length(leglab) == 1) leglab <- c(leglab, leglab)
  
  #make colors transparent
  col <- adjustcolor(c(col, "yellow", "black", "white", "red"), alpha.f = col.alpha)
  
          trackfreFUN <- function(X, i, mar, flim, xl, picsize, wl, cexlab, inner.mar, outer.mar, res, bp, cex, threshold, pch, custom.contour){
    
    # Read sound files, initialize frequency and time limits for spectrogram
    r <- tuneR::readWave(as.character(X$sound.files[i]), header = TRUE)
    f <- r$sample.rate
    t <- c(X$start[i] - mar, X$end[i] + mar)
    
    #adjust margins if signal is close to start or end of sound file
    mar1 <- mar
      
    if(t[1] < 0) {
      t[1] <- 0
    mar1 <- X$start[i]
    }

    mar2 <- mar1 + X$end[i] - X$start[i]
    
    if(t[2] > r$samples/f) t[2] <- r$samples/f


    # read rec segment
    r <- tuneR::readWave(as.character(X$sound.files[i]), from = t[1], to = t[2], units = "seconds")
   
    #in case bp its higher than can be due to sampling rate
    if(bp[1] == "frange") bp <- c(X$low.f[i], X$high.f[i])
    b<- bp 

    if(b[2] > ceiling(r@samp.rate/2000) - 1) b[2] <- ceiling(r@samp.rate/2000) - 1 
    
    fl<- flim #in case flim its higher than can be due to sampling rate
    if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
    
    
    # Spectrogram width can be proportional to signal duration
    if(propwidth)
      pwc <- (10.16) * ((t[2]-t[1])/0.27) * xl * picsize else pwc <- (10.16) * xl * picsize

    #call image function
    imgfun(filename = paste0(X$sound.files[i],"-", X$selec[i], "-", img.suffix2), 
           width = pwc, height = (10.16) * picsize, units = "cm", res = res) 
    
    # Change relative heights of rows for spectrogram when osci = TRUE
    if(osci == TRUE) hts <- c(3, 2) else hts <- NULL
    
    # Change relative widths of columns for spectrogram when sc = TRUE
    if(sc == TRUE) wts <- c(3, 1) else wts <- NULL
    
    # Change inner and outer plot margins
    par(mar = inner.mar)
    par(oma = outer.mar)
    
    # Generate spectrogram using seewave
    seewave::spectro(r, f = f, wl = wl, ovlp = 70, collevels = seq(-40, 0, 0.5), heights = hts,
            wn = "hanning", widths = wts, palette = pal, osc = osci, grid = gr, scale = sc, collab = "black", 
            cexlab = cexlab, cex.axis = 0.5*picsize, flim = fl, tlab = "Time (s)", 
            flab = "Frequency (kHz)", alab = "")
    
    if(title){

      if(is.null(img.suffix))
        title(paste(X$sound.files[i], X$selec[i], sep = "-"), cex.main = cexlab) else
          title(paste(X$sound.files[i], X$selec[i], img.suffix, sep = "-"), cex.main = cexlab)  
    
    }
    
    # Calculate fundamental frequencies at each time point
if(contour %in% c("both", "ff") & is.null(custom.contour))
{
  ffreq1 <- seewave::fund(wave = r, wl = wl, from = mar1, to = mar2,
              fmax= b[2]*1000, f = f, ovlp = 70, threshold = threshold, plot = FALSE)
  ffreq <- matrix(ffreq1[!is.na(ffreq1[,2]),], ncol = 2)  
  ffreq <- matrix(ffreq[ffreq[,2] > b[1],], ncol = 2)
    
    # Plot extreme values fundamental frequency
      points(c(ffreq[c(which.max(ffreq[,2]),which.min(ffreq[,2])),1]) + mar1, c(ffreq[c(which.max(ffreq[,2]), 
        which.min(ffreq[,2])),2]), col = col[3], cex = cex[1] * 1.6, pch = pch[1], lwd = 2)  
  
  # Plot all fundamental frequency values
  if(type %in% c("p", "b"))
     points(c(ffreq[,1])+mar1, c(ffreq[,2]), col = col[1], cex = cex[1], pch = pch[1], bg = col[1])
 
  # plot lines      
  if(type %in% c("l", "b"))
    lines(ffreq[,1] + mar1, ffreq[,2], col = col[1], lwd = 3) 
      
         
    # Plot empty points at the bottom for the bins that did not detected any frequencies or out of bp
    if(nrow(ffreq1) > nrow(ffreq))
    points(c(ffreq1[!ffreq1[,1] %in% ffreq[,1], 1]) + mar1, rep(fl[1] + (fl[2] - fl[1]) * 0.04, nrow(ffreq1) - nrow(ffreq)), col = col[4], cex = cex[1] * 0.7, pch = pch[1])
}
     
  
    # Calculate dominant frequency at each time point     
    if(contour %in% c("both", "df") & is.null(custom.contour))
{    
      dfreq1 <- seewave::dfreq(r, f = f, wl = wl, ovlp = 70, plot = FALSE, bandpass = b * 1000, fftw = TRUE, 
                   threshold = threshold, tlim = c(mar1, mar2)) 
      dfreq <- matrix(dfreq1[!is.na(dfreq1[,2]),], ncol = 2)  

    # Plot extreme values dominant frequency
        points(c(dfreq[c(which.max(dfreq[,2]),which.min(dfreq[,2])),1])+mar1, c(dfreq[c(which.max(dfreq[,2]),
        which.min(dfreq[,2])),2]), col = col[3], cex = cex[1] * 1.6, pch = pch[2], lwd = 2) 

   # Plot all dominant frequency values
   if(type %in% c("p", "b"))
     points(dfreq[,1] + mar1, dfreq[,2], col = col[2], cex = cex[1], pch = pch[2], bg = col[2]) 
    
  # plot lines      
    if(type %in% c("l", "b"))
    lines(dfreq[,1] + mar1, dfreq[,2], col = col[2], lwd = 3) 
    
    # Plot empty points at the bottom for the bins that did not detected any frequencies or out of bp
    if(nrow(dfreq1) > nrow(dfreq))
      points(c(dfreq1[!dfreq1[,1] %in% dfreq[,1], 1]) + mar1, rep(fl[1] + (fl[2] - fl[1]) * 0.02, nrow(dfreq1) - nrow(dfreq)), col = col[4], cex = cex[1] * 0.7, pch = pch[2])
        }
    
    # Use freq values provided by user   
    if(!is.null(custom.contour))
    {    
      custom <- custom.contour[i, 3:ncol(custom.contour)]
      timeaxis <- seq(from = 0,  to = X$end[i] - X$start[i], length.out = length(custom))
      freq1 <- cbind(timeaxis, t(custom))
      freq <- freq1[!is.na(freq1[,2]),]  
      
      # Plot extreme values dominant frequency
      points(c(freq[c(which.max(freq[,2]),which.min(freq[,2])),1])+mar1, c(freq[c(which.max(freq[,2]),
                                                                                      which.min(freq[,2])),2]), col = col[3], cex = cex[1] * 1.6, pch = pch[2], lwd = 2) 
      
      # Plot all dominant frequency values
      if(type %in% c("p", "b"))
        points(freq[,1] + mar1, freq[,2], col = col[2], cex = cex[1], pch = pch[2], bg = col[2]) 
      
      
      # plot lines      
      if(type %in% c("l", "b"))
        lines(freq[,1] + mar1, freq[,2], col = col[2], lwd = 3) 
      
      # Plot empty points at the bottom for the bins that did not detected any frequencies or out of bp
      if(nrow(freq1) > nrow(freq))
        points(c(freq1[!freq1[,1] %in% freq[,1], 1]) + mar1, rep(fl[1] + (fl[2] - fl[1]) * 0.02, nrow(freq1) - nrow(freq)), col = col[4], cex = cex[1] * 0.7, pch = pch[2])
      
    }
    
  if(line){  
    if(any(names(X) == "low.f") & any(names(X) == "high.f"))
  {   if(!is.na(X$low.f[i]) & !is.na(X$high.f[i]))
      polygon(x = rep(c(mar1, mar2), each = 2), y = c(X$low.f[i], X$high.f[i], X$high.f[i], X$low.f[i]), lty = 3, border = "blue", lwd = 1.2, col = adjustcolor("blue", alpha.f = 0.05)) else
          abline(v = c(mar1, mar2), col= col[6], lty = "dashed")
    } else abline(v = c(mar1, mar2), col= col[6], lty = "dashed")
    }
    
## legend
      # remove points fo legend
    if(type == "l") pch <- NA
    if(type %in% c("l", "b")) lwd <- 3 else lwd = NA
    
    # Adjust legend coordinates  
    if(is.null(custom.contour))
    {
      if(contour == "both")
    legend(lpos, legend = leglab, bg = col[5],
           pch = pch, col = col[1:2], bty = "o", cex = cex[2], pt.bg = col[1:2], lwd = lwd)

    if(contour == "ff")
      legend(lpos, legend = leglab[1],
             pch = pch[1], col = col[1], bty = "o", cex = cex[2], bg = col[5], pt.bg = col[1], lwd = lwd)

    if(contour == "df")
      legend(lpos, legend = leglab[2],
             pch = pch[2], col = col[2], bty = "o", cex = cex[2], bg = col[5], pt.bg = col[2], lwd = lwd)
      } else
    
{ 
legend(lpos, legend = leglab[1],
             pch = pch[2], col = col[2], bty = "o", cex = cex[2], bg = col[5], pt.bg = col[2], lwd = lwd)}
    
    invisible() 
    dev.off()
    return(NULL)
    
  }

          # Run parallel in windows
          if(parallel > 1) {
            if(Sys.info()[1] == "Windows") {
              
              i <- NULL #only to avoid non-declared objects
              
              cl <- parallel::makeCluster(parallel)
              
              doParallel::registerDoParallel(cl)
              
              sp <- foreach::foreach(i = 1:nrow(X)) %dopar% {
                trackfreFUN(X = X, i = i, mar = mar, flim = flim, xl = xl, picsize = picsize, res = res, wl = wl, cexlab = cexlab, inner.mar = inner.mar, outer.mar = outer.mar, bp = bp, cex = cex, threshold = threshold, pch = pch,
                            custom.contour)
              }
              
              parallel::stopCluster(cl)
              
            } 
            if(Sys.info()[1] == "Linux") {    # Run parallel in Linux
              
              sp <- parallel::mclapply(1:nrow(X), function (i) {
                trackfreFUN(X = X, i = i, mar = mar, flim = flim, xl = xl, picsize = picsize, res = res, wl = wl, cexlab = cexlab, inner.mar = inner.mar, outer.mar = outer.mar, bp = bp, cex = cex, threshold = threshold, pch = pch,
                            custom.contour)
              })
            }
            if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
            {
              cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))
              
              doParallel::registerDoParallel(cl)
              
              sp <- foreach::foreach(i = 1:nrow(X)) %dopar% {
                trackfreFUN(X = X, i = i, mar = mar, flim = flim, xl = xl, picsize = picsize, res = res, wl = wl, cexlab = cexlab, inner.mar = inner.mar, outer.mar = outer.mar, bp = bp, cex = cex, threshold = threshold, pch = pch,
                            custom.contour)
              }
              
              parallel::stopCluster(cl)
              
            }
          }
          else {
            if(pb)
            sp <- pbapply::pblapply(1:nrow(X), function(i) 
              trackfreFUN(X = X, i = i, mar = mar, flim = flim, xl = xl, picsize = picsize, res = res, wl = wl, cexlab = cexlab, inner.mar = inner.mar, outer.mar = outer.mar, bp = bp, cex = cex, threshold = threshold, pch = pch,
                          custom.contour)) else
                  sp <- lapply(1:nrow(X), function(i) 
                    trackfreFUN(X = X, i = i, mar = mar, flim = flim, xl = xl, picsize = picsize, res = res, wl = wl, cexlab = cexlab, inner.mar = inner.mar, outer.mar = outer.mar, bp = bp, cex = cex, threshold = threshold, pch = pch,
                      custom.contour))
            
          }
          
  if(!is.null(path)) on.exit(setwd(wd))
          
}
