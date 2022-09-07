#' Spectrograms with frequency measurements
#' 
#' \code{track_freq_contour} creates spectrograms to visualize dominant and fundamental frequency measurements (contours)
#' @usage track_freq_contour(X, wl = 512, wl.freq = 512, flim = NULL, wn = "hanning", pal =
#'   reverse.gray.colors.2, ovlp = 70, inner.mar = c(5, 4, 4, 2), outer.mar = 
#'   c(0, 0, 0, 0), picsize = 1, res = 100, cexlab = 1, title = TRUE, propwidth = FALSE, 
#'   xl = 1, osci = FALSE, gr = FALSE, sc = FALSE, bp = NULL, cex = c(0.6, 1), 
#'   threshold = 15, threshold.time = NULL, threshold.freq = NULL, contour = "both", 
#'   col = c("#E37222B3", "#07889BB3"), pch = c(21, 24),  mar = 0.05, lpos = "topright", 
#'   it = "jpeg", parallel = 1, path = NULL, img.suffix = NULL, custom.contour = NULL, 
#'   pb = TRUE, type = "p", leglab = c("Ffreq", "Dfreq"), col.alpha = 0.6, line = TRUE, 
#'    fast.spec = FALSE, ff.method = "seewave", frange.detec = FALSE, fsmooth = 0.1, 
#'    widths = c(2, 1), freq.continuity = NULL, clip.edges = 2, track.harm = FALSE, ...)
#' @param  X object of class 'selection_table', 'extended_selection_table' or data frame containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The output \code{\link{auto_detec}} can also be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param wl.freq A numeric vector of length 1 specifying the window length of the spectrogram
#' for measurements on the frequency spectrum. Default is 512. Higher values would provide 
#' more accurate measurements.
#' @param flim A numeric vector of length 2 for the frequency limit of 
#'   the spectrogram (in kHz), as in \code{\link[seewave]{spectro}}. Default is \code{NULL}.
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
#'   frequency bandpass filter (in kHz) or "frange" to indicate that values in bottom.freq 
#'   and top.freq columns will be used as bandpass limits. Default is \code{NULL}.
#' @param cex Numeric vector of length 2, specifies relative size of points 
#'   plotted for frequency measurements and legend font/points, respectively. 
#'   See \code{\link[seewave]{spectro}}.
#' @param threshold amplitude threshold (\%) for fundamental and dominant frequency detection as well as frequency range from the spectrum (see 'frange.detec'). Default is 15. WILL BE DEPRECATED. Use 'threshold.time' and 'threshold.time' instead.
#' @param threshold.time amplitude threshold (\%) for the time domain. Use for fundamental and dominant frequency detection. If \code{NULL} (default) then the 'threshold' value is used.
#' @param threshold.freq amplitude threshold (\%) for the frequency domain. Use for frequency range detection from the spectrum (see 'frange.detec'). If \code{NULL} (default) then the
#'  'threshold' value is used.
#' @param contour Character vector, one of "df", "ff" or "both", specifying whether the
#'  dominant or fundamental frequencies or both should be plotted. Default is "both". 
#' @param col Vector of length 1 or 2 specifying colors of points plotted to mark 
#'   fundamental and dominant frequency measurements respectively (if both are plotted). Default is \code{c("#E37222B3",
#'   "#07889BB3")}. Extreme values (lowest and highest) are highlighted in yellow.
#' @param pch Numeric vector of length 1 or 2 specifying plotting characters for 
#'   the frequency measurements. Default is c(21, 24).
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the selections
#'  to set spectrogram limits. Default is 0.05.
#' @param lpos Character vector of length 1 or numeric vector of length 2, 
#'   specifying position of legend. If the former, any keyword accepted by 
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
#' first 2 columns of the data frame should contain the 'sound.files' and 'selec' columns and should be 
#' identical to the corresponding columns in X (same order). 
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param type A character vector of length 1 indicating the type of frequency contour plot to be drawn. 
#' Possible types are "p" for points, "l" for lines and "b" for both.
#' @param leglab A character vector of length 1 or 2 containing the label(s) of the frequency contour legend 
#' in the output image.
#' @param col.alpha A numeric vector of length 1  within [0,1] indicating how transparent the lines/points should be.
#' @param line Logical argument to add red lines (or box if bottom.freq and top.freq columns are provided) at start and end times of selection. Default is \code{TRUE}.
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR:specCols]{gray.1}}, \code{\link[monitoR:specCols]{gray.2}}, 
#' \code{\link[monitoR:specCols]{gray.3}}, \code{\link[monitoR:specCols]{topo.1}} and \code{\link[monitoR:specCols]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast' spectrograms. Palette colors \code{\link[monitoR:specCols]{gray.1}}, \code{\link[monitoR:specCols]{gray.2}}, 
#' \code{\link[monitoR:specCols]{gray.3}} offer 
#' decreasing darkness levels. 
#' @param ff.method Character. Selects the method used to calculate the fundamental
#' frequency. Either 'tuneR' (using \code{\link[tuneR]{FF}}) or 'seewave' (using 
#' \code{\link[seewave]{fund}}). Default is 'seewave'. 'tuneR' performs faster (and seems to be more accurate) than 'seewave'.
#' @param frange.detec Logical. Controls whether frequency range of signal is automatically 
#' detected  using the \code{\link{freq_range_detec}} function. If so, the range is used as the 
#' bandpass filter (overwriting 'bp' argument). Default is \code{FALSE}.
#' @param fsmooth A numeric vector of length 1 to smooth the frequency spectrum with a mean
#'  sliding window (in kHz) used for frequency range detection (when \code{frange.detec = TRUE}). This help to average amplitude "hills" to minimize the effect of
#'  amplitude modulation. Default is 0.1. 
#' @param widths Numeric vector of length 2 to control the relative widths of the spectro (first element) and spectrum (second element,  (when \code{frange.detec = TRUE})).
#' @param freq.continuity Numeric vector of length 1 to control whether dominant frequency detections
#'  outliers(i.e that differ from the frequency of the detections right before and after) would be removed. Should be given in kHz. Default is \code{NULL}. 
#' @param clip.edges Integer vector of length 1 to control if how many 'frequency-wise discontinuous' detection would be remove at the start and end of signals (see 
#' 'freq.continuity' argument). Default is 2. Ignored if \code{freq.continuity = NULL}. 
#' @param track.harm Logical to control if \code{\link{track_harmonic}} or a modified version of \code{\link[seewave]{dfreq}} is used for dominant frequency detection. Default is \code{FALSE} (use \code{\link[seewave]{dfreq}}).
#' @param ... Additional arguments to be passed to the internal spectrogram creating function for customizing graphical output. The function is a modified version of \code{\link[seewave]{spectro}}, so it takes the same arguments.
#' @return Spectrograms of the signals listed in the input data frame showing the location of 
#' the dominant and fundamental frequencies.
#' @family spectrogram creators
#' @seealso \code{\link{spectrograms}} for creating spectrograms from selections,
#'  \code{\link{snr_spectrograms}} for creating spectrograms to 
#'   optimize noise margins used in \code{\link{sig2noise}}
#' @export
#' @name track_freq_contour
#' @details This function provides visualization of frequency measurements as the ones 
#'   made by \code{\link{spectro_analysis}}, \code{\link{freq_ts}} and \code{\link{freq_DTW}}. Frequency measures can be made by the function or input by the 
#'   user (see 'custom.contour' argument). If \code{frange = TRUE} the function uses \code{\link{freq_range_detec}} to detect the frequency range. In this case the graphical output includes a
#'   frequency spectrum showing the detection threshold. Extreme values (lowest and highest) are highlighted in yellow. 
#'   Note that, unlike other warbleR functions that measure frequency contours, track_freq_contour do not interpolate frequency values.
#' @examples
#' {
#' #load data
#' data("Cryp.soui")
#' writeWave(Cryp.soui, file.path(tempdir(), "Cryp.soui.wav")) #save sound files 
#' 
#' #autodetec location of signals
#' ad <- auto_detec(threshold = 6, bp = c(1, 3), mindur = 1.2, flim = c(0, 5),
#' maxdur = 3, img = FALSE, ssmooth = 600, wl = 300, flist = "Cryp.soui.wav", 
#' path = tempdir())
#' 
#' #track dominant frequency graphs with freq range detection
#' track_freq_contour(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, 
#' it = "tiff", bp = c(1, 3), contour = "df", wl = 300, frange = TRUE, 
#' path = tempdir())
#'
#' #using users frequency data (custom.contour argument) 
#' #first get contours using freq_ts
#' df <- freq_ts(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, img = FALSE,
#' bp = c(1, 3),  wl = 300, path = tempdir())
#'
#'# now input the freq_ts output into track_freq_contour         
#'track_freq_contour(X = ad[!is.na(ad$start),], custom.contour = df ,flim = c(0, 5), ovlp = 90, 
#'it = "tiff", path = tempdir())
#' 
#'# Check this folder
#' tempdir()
#'
#'#track both frequencies 
#'track_freq_contour(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, 
#' it = "tiff", bp = c(1, 3), contour = "both", wl = 300, path = tempdir())
#' 
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Grace Smith Vidaurre and Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on mar-13-2018 (MAS)

track_freq_contour <- function(X, wl = 512, wl.freq = 512, flim = NULL, wn = "hanning", pal = reverse.gray.colors.2, ovlp = 70, 
                               inner.mar = c(5,4,4,2), outer.mar = c(0, 0, 0, 0), picsize = 1, res = 100, cexlab = 1,
                               title = TRUE, propwidth = FALSE, xl = 1, osci = FALSE, gr = FALSE, sc = FALSE, 
                               bp = NULL, cex = c(0.6, 1), threshold = 15, threshold.time = NULL, threshold.freq = NULL, 
                               contour = "both", col = c("#E37222B3", "#07889BB3"),  pch = c(21, 24), mar = 0.05, lpos = "topright", 
                               it = "jpeg", parallel = 1, path = NULL, img.suffix = NULL, custom.contour = NULL, pb = TRUE,
                               type = "p", leglab = c("Ffreq", "Dfreq"), col.alpha = 0.6, line = TRUE, fast.spec = FALSE, 
                               ff.method = "seewave", frange.detec = FALSE, fsmooth = 0.1, widths = c(2, 1), 
                               freq.continuity = NULL, clip.edges = 2, track.harm = FALSE, ...){     
  
  
  
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(track_freq_contour)
  
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
    if (!dir.exists(path)) stop2("'path' provided does not exist") else
      path <- normalizePath(path) 
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop2("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  if (!all(c("sound.files", "selec", 
             "start", "end") %in% colnames(X))) 
    stop2(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop2("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop2("'start' and 'end' must be numeric")
  
  #if any start higher than end stop
  if (any(X$end - X$start <= 0)) stop2(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))  
  
  #if any selections longer than 20 secs stop
  if (any(X$end - X$start>20)) stop2(paste(length(which(X$end - X$start>20)), "selection(s) longer than 20 sec"))  
  
  # bp checking
  if(!is.null(bp))
  if (bp[1] != "frange")
  {if (!is.vector(bp)) stop2("'bp' must be a numeric vector of length 2") else{
    if (!length(bp) == 2) stop2("'bp' must be a numeric vector of length 2")} 
  } else
  {if (!any(names(X) == "bottom.freq") & !any(names(X) == "top.freq")) stop2("'bp' = frange requires bottom.freq and top.freq columns in X")
    if (any(is.na(c(X$bottom.freq, X$top.freq)))) stop2("NAs found in bottom.freq and/or top.freq") 
    if (any(c(X$bottom.freq, X$top.freq) < 0)) stop2("Negative values found in bottom.freq and/or top.freq") 
    if (any(X$top.freq - X$bottom.freq < 0)) stop2("top.freq should be higher than low.f")
  }
  
  #if it argument is not "jpeg" or "tiff" 
  if (!any(it == "jpeg", it == "tiff")) stop2(paste("Image type", it, "not allowed"))  
  #if ff.method argument  
  if (!any(ff.method == "seewave", ff.method == "tuneR")) stop2(paste("ff.method", ff.method, "is not recognized"))  
  
  #if type not l b or p
  if (!any(type %in% c("p", "l", "b"))) stop2(paste("Type", type, "not allowed"))  
  
  # if frange.detec oscillo false
  if (frange.detec) osc <- FALSE
  
  #join img.suffix and it
  if (is.null(img.suffix))
    img.suffix2 <- paste("trackfreqs", it, sep = ".") else   img.suffix2 <- paste(img.suffix, it, sep = ".")
  
  # threshold adjustment
  if (is.null(threshold.time)) threshold.time <- threshold
  if (is.null(threshold.freq)) threshold.freq <- threshold
  
  #return warning if not all sound files were found
  if (!is_extended_selection_table(X))  
  {
    recs.wd <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)
    if (length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files))) 
      cat(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
                "sound file(s) not found"))
    
    #count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% recs.wd) 
    if (length(d) == 0){
      stop2("The sound files are not in the working directory")
    }  else X <- X[d, , drop = FALSE]
  }
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop2("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop2("'parallel' should be a positive integer")
  
  # Compare custom.contour to X
  if (!is.null(custom.contour) & is.data.frame(custom.contour)){
    #check if sound.files and selec columns are present and in the right order
    if (!identical(names(custom.contour)[1:2], c("sound.files", "selec"))) stop2("'sound.files' and/or 'selec' columns are not found in custom.contour")
    
    #check if the info in sound.files and selec columns is the same for X and custom.contour
    #remove custom.contour selections not in X
    custom.contour <- custom.contour[paste(custom.contour[ , "sound.files"], custom.contour[ , "selec"]) %in% paste(as.data.frame(X)[ , "sound.files"], as.data.frame(X)[ , "selec"]), ]
    
    #stop if not the same number of selections
    if (nrow(X) > nrow(custom.contour)) stop2("selection(s) in X but not in custom.contour")
    
    #order custom.contour as in X
    custom.contour <- custom.contour[match(paste(custom.contour[ , "sound.files"], custom.contour[ , "selec"]), paste(as.data.frame(X)[ , "sound.files"], as.data.frame(X)[ , "selec"])), ]      
    
    # frange.detec <- FALSE
  }
  
  # adjust if only 1 pch was specfified
  if (length(pch) == 1) pch <- c(pch, pch)
  
  # adjust if only 1 color was specified
  if (length(col) == 1) col <- c(col, col)
  
  # adjust if only 1 leglab was specified
  if (length(leglab) == 1) leglab <- c(leglab, leglab)
  
  #make colors transparent
  col <- adjustcolor(c(col, "yellow", "black", "white", "red"), alpha.f = col.alpha)
  
  trackfreFUN <- function(X, i, mar, flim, xl, picsize, wl, wl.freq, cexlab, inner.mar, outer.mar, res, bp, cex, threshold.time, threshold.freq, pch, custom.contour){
    
    # Read sound files, initialize frequency and time limits for spectrogram
    r <- warbleR::read_sound_file(X = X, path = path, index = i, header = TRUE)
    f <- r$sample.rate
    t <- c(X$start[i] - mar, X$end[i] + mar)
    
    # adjust margins if signal is close to start or end of sound file
    mar1 <- mar
    
    # adjust margin if negative
    if (t[1] < 0) {
      t[1] <- 0
      mar1 <- X$start[i]
    }
    
    mar2 <- mar1 + X$end[i] - X$start[i]
    
    if (t[2] > r$samples/f) t[2] <- r$samples/f
    
    
    # read rec segment
    r <- warbleR::read_sound_file(X = X, path = path, index = i, from = t[1], to = t[2])
    
    #in case bp its higher than can be due to sampling rate
    if (!is.null(bp)){
      if (bp[1] == "frange") bp <- c(X$bottom.freq[i], X$top.freq[i])
    } 
    
    b <- bp
    
    if (!is.null(b))
    if (b[2] > floor(r@samp.rate / 2000)) b[2] <- floor(r@samp.rate / 2000) 
    
    fl<- flim #in case flim its higher than can be due to sampling rate
    if (!is.null(fl))
    {if (fl[2] > floor(f / 2000)) fl[2] <- floor(f / 2000)} else
      fl <- c(0, floor(f / 2000))
    
    
    # Spectrogram width can be proportional to signal duration
    if (propwidth)
      pwc <- (10.16) * ((t[2]-t[1])/0.27) * xl * picsize else pwc <- (10.16) * xl * picsize
    
    #call image function
    img_wrlbr_int(filename = paste0(X$sound.files[i],"-", X$selec[i], "-", img.suffix2), path = path,
                  width = pwc, height = (10.16) * picsize, units = "cm", res = res) 
    
    # Change relative heights of rows for spectrogram when osci = TRUE
    if (osci == TRUE) hts <- c(3, 2) else hts <- NULL
    
    # Change relative widths of columns for spectrogram when sc = TRUE
    if (sc == TRUE) wts <- c(3, 1) else wts <- NULL
    
    # Change inner and outer plot margins
    par(mar = inner.mar)
    par(oma = outer.mar)
    
    # Generate spectrograms
    if (!frange.detec){
      suppressWarnings(spectro_wrblr_int(r, f = f, wl = wl, ovlp = ovlp, heights = hts,
                                         wn = "hanning", widths = wts, palette = pal, osc = osci, grid = gr, scale = sc, collab = "black",
                                         cexlab = cexlab, cex.axis = 0.5*picsize, flim = fl, tlab = "Time (s)", 
                                         flab = "Frequency (kHz)", alab = "", fast.spec = fast.spec, ...))
      
      if (title){
        
        if (is.null(img.suffix))
          title(paste(X$sound.files[i], X$selec[i], sep = "-"), cex.main = cexlab) else
            title(paste(X$sound.files[i], X$selec[i], img.suffix, sep = "-"), cex.main = cexlab)  
        
      }
    } else {
      frng <- frd_wrblr_int(wave = seewave::cutw(r, from = mar1, to = mar2, output = "Wave"), wl = wl.freq, fsmooth = fsmooth, threshold = threshold.freq, wn = wn, bp = b, ovlp = ovlp)
      
      if (!all(is.na(frng$frange))) b <- as.numeric(frng$frange) 
      
      # set limits for color rectangles down
      if (is.null(bp)) lims <- flim else lims <- bp
      b[is.na(b)] <- lims[is.na(b)]
      b <- sort(b)
      
      # split screen
      m <- rbind(c(0, widths[1]/sum(widths), 0, 0.93), #1
                 c(widths[1]/sum(widths), 1, 0 , 0.93),
                 c(0, 1,  0.93 , 1)) #3
      
      invisible(close.screen(all.screens = TRUE))
      split.screen(m)
      screen(1)
      par(mar = c(3.4, 3.4, 0.5, 0))
      
      # create spectro
      spectro_wrblr_int2(wave = r, f = f, flim = fl, fast.spec = fast.spec, palette = pal, ovlp = ovlp, wl = wl, grid = F, tlab = "", flab = "")
      
      #add green polygon on detected frequency bands
      rect(xleft = 0, ybottom = b[1], xright = seewave::duration(r), ytop = b[2], col = adjustcolor("#07889B", alpha.f = 0.1), border = adjustcolor("gray", 0.1)) 
      
      #add line highlighting freq range
      abline(h = b, col = "#07889B", lty = 3, lwd = 1) 
      
      # add axis labels
      mtext(side = 1, text = "Time (s)", line = 2.3)
      mtext(side = 2, text = "Frequency (kHz)", line = 2.3)
    }
    
    options(warn = -1)
    # Calculate fundamental frequencies at each time point
    if (contour %in% c("both", "ff") & is.null(custom.contour))
    {
      if (ff.method == "seewave")
        ffreq1 <- seewave::fund(wave = r, wl = wl, from = mar1, to = mar2,
                                fmax= if (!is.null(b)) b[2]*1000 else f/2, f = f, ovlp = ovlp, threshold = threshold.time, plot = FALSE) else
                                {ff1 <- tuneR::FF(tuneR::periodogram(seewave::cutw(r, f = f, from = mar1, to = mar2, output = "Wave"), width = wl, overlap = wl*ovlp/100), peakheight = (100 - threshold.time) / 100)/1000
                                ff2 <- seq(0, X$end[i] - X$start[i], length.out = length(ff1))
                                
                                ffreq1 <- cbind(ff2, ff1)}
      
      
      ffreq <- matrix(ffreq1[!is.na(ffreq1[,2]),], ncol = 2)  
      ffreq <- matrix(ffreq[ffreq[,2] > b[1],], ncol = 2)
      
      if (!is.null(freq.continuity)) ffreq <- ffreq[c(0,abs(diff(ffreq[,2]))) <= freq.continuity, ]
      
      # Plot extreme values fundamental frequency
      points(c(ffreq[c(which.max(ffreq[,2]),which.min(ffreq[,2])),1]) + mar1, c(ffreq[c(which.max(ffreq[,2]), 
                                                                                        which.min(ffreq[,2])),2]), col = col[3], cex = cex[1] * 1.6, pch = pch[1], lwd = 2)  
      
      # Plot all fundamental frequency values
      if (type %in% c("p", "b"))
        points(c(ffreq[,1])+mar1, c(ffreq[,2]), col = col[1], cex = cex[1], pch = pch[1], bg = col[1])
      
      # plot lines      
      if (type %in% c("l", "b"))
        lines(ffreq[,1] + mar1, ffreq[,2], col = col[1], lwd = 3) 
      
      
      # Plot empty points at the bottom for the bins that did not detected any frequencies or out of bp
      if (nrow(ffreq1) > nrow(ffreq))
        points(c(ffreq1[!ffreq1[,1] %in% ffreq[,1], 1]) + mar1, rep(fl[1] + (fl[2] - fl[1]) * 0.04, nrow(ffreq1) - nrow(ffreq)), col = col[4], cex = cex[1] * 0.7, pch = pch[1])
    }
    
    # Calculate dominant frequency at each time point     
    if (contour %in% c("both", "df") & is.null(custom.contour))
    {    
      dfreq1 <- track_harmonic(r, f = f, wl = wl, ovlp = 70, plot = FALSE, bandpass = if(!is.null(b)) b * 1000 else b, fftw = TRUE, 
                               threshold = threshold.time, tlim = c(mar1, mar2), dfrq = !track.harm, adjust.wl = TRUE) 
      dfreq <- matrix(dfreq1[!is.na(dfreq1[,2]),], ncol = 2)  
      
      
      #freq continuity
      if (nrow(dfreq > 2) & !is.null(freq.continuity))
      {
        indx <- sapply(1:nrow(dfreq), function(x)
        {
          # if first one
          if (x == 1)
          {if (abs(dfreq[x, 2] - dfreq[x + 1, 2]) > freq.continuity & abs(dfreq[x + 1, 2] - dfreq[x + 2, 2]) < freq.continuity) return(FALSE) else return(TRUE)
            
          } else {
            # if last one
            if (x == nrow(dfreq))  
            {
              if (abs(dfreq[x, 2] - dfreq[x - 1, 2]) > freq.continuity & abs(dfreq[x - 2, 2] - dfreq[x - 1, 2]) < freq.continuity) return(FALSE) else return(TRUE)
              
            } else
            {
              if (abs(dfreq[x, 2] - dfreq[x + 1, 2]) > freq.continuity & abs(dfreq[x, 2] - dfreq[x - 1, 2]) > freq.continuity) return(FALSE) else return(TRUE)
            }
          }
        })
        
        if (nrow(dfreq) > 3 * clip.edges & any(!indx[2:clip.edges])) indx[1:(which(!indx[2:clip.edges]))] <- FALSE
        # turn around  
        indx <- indx[nrow(dfreq):1]
        
        if (nrow(dfreq) > 3 * clip.edges & any(!indx[2:clip.edges])) indx[1:(which(!indx[2:clip.edges]))] <- FALSE
        
        # turn around again
        indx <- indx[nrow(dfreq):1]
        
        dfreq <- dfreq[indx, ]
        
      }
      
      dfreq <- as.matrix(dfreq, nrow = 2)
      
      # Plot extreme values dominant frequency
      points(c(dfreq[c(which.max(dfreq[,2]),which.min(dfreq[,2])),1])+mar1, c(dfreq[c(which.max(dfreq[,2]),
                                                                                      which.min(dfreq[,2])),2]), col = col[3], cex = cex[1] * 1.6, pch = pch[2], lwd = 2) 
      
      # Plot all dominant frequency values
      if (type %in% c("p", "b"))
        points(dfreq[,1] + mar1, dfreq[,2], col = col[2], cex = cex[1], pch = pch[2], bg = col[2]) 
      
      # plot lines      
      if (type %in% c("l", "b"))
        lines(dfreq[,1] + mar1, dfreq[,2], col = col[2], lwd = 3) 
      
      # Plot empty points at the bottom for the bins that did not detected any frequencies or out of bp
      if (nrow(dfreq1) > nrow(dfreq))
        points(c(dfreq1[!dfreq1[,1] %in% dfreq[,1], 1]) + mar1, rep(fl[1] + (fl[2] - fl[1]) * 0.02, nrow(dfreq1) - nrow(dfreq)), col = col[4], cex = cex[1] * 0.7, pch = pch[2])
    }
    
    # Use freq values provided by user   
    if (!is.null(custom.contour))
    { 
      if (!is.data.frame(custom.contour)) {
        custom <- try(custom.contour[[i]], silent = TRUE)
        if (is(custom, "try-error")) {
          custom <- rep(NA, 3)
          freq1 <- matrix(rep(NA, 2), ncol = 2)
        } else {
          freq1 <- try(custom.contour[[i]][ , 2:3], silent = TRUE)
          if (is(freq1, "try-error")) freq1 <- custom.contour[ , 2:3, drop = FALSE]
        }      
        freq <- freq1[!is.na(freq1[,2]), , drop = FALSE]
      } else
      { 
        custom <- custom.contour[i, 3:ncol(custom.contour)]
        timeaxis <- seq(from = 0,  to = X$end[i] - X$start[i], length.out = length(custom))
        freq1 <- cbind(timeaxis, t(custom))
        freq <- freq1[!is.na(freq1[,2]), , drop = FALSE] 
      } 
      
      # Plot extreme values dominant frequency
      points(c(freq[c(which.max(freq[,2]),which.min(freq[,2])),1])+mar1, c(freq[c(which.max(freq[,2]),
                                                                                  which.min(freq[,2])),2]), col = col[3], cex = cex[1] * 1.6, pch = pch[2], lwd = 2) 
      
      # Plot all dominant frequency values
      if (type %in% c("p", "b"))
        points(freq[,1] + mar1, freq[,2], col = col[2], cex = cex[1], pch = pch[2], bg = col[2]) 
      
      # plot lines      
      if (type %in% c("l", "b"))
        lines(freq[,1] + mar1, freq[,2], col = col[2], lwd = 3) 
      
      # Plot empty points at the bottom for the bins that did not detected any frequencies or out of bp
      if (nrow(freq1) > nrow(freq))
        points(c(freq1[!freq1[,1] %in% freq[,1], 1]) + mar1, rep(fl[1] + (fl[2] - fl[1]) * 0.02, nrow(freq1) - nrow(freq)), col = col[4], cex = cex[1] * 0.7, pch = pch[2])
      
    }
    
    if (line){  
      if (any(names(X) == "bottom.freq") & any(names(X) == "top.freq"))
      {   if (!is.na(X$bottom.freq[i]) & !is.na(X$top.freq[i]))
        if (!frange.detec) polygon(x = rep(c(mar1, mar2), each = 2), y = c(X$bottom.freq[i], X$top.freq[i], X$top.freq[i], X$bottom.freq[i]), lty = 3, border = col[6], lwd = 1.2) else
          abline(v = c(mar1, mar2), col= col[6], lty = "dashed")
      } else abline(v = c(mar1, mar2), col= col[6], lty = "dashed")
    }
    
    ## legend
    # remove points for legend
    if (type == "l") pch <- NA
    if (type %in% c("l", "b")) lwd <- 3 else lwd = NA
    
    # Adjust legend coordinates  
    if (is.null(custom.contour))
    {
      if (contour == "both")
        legend(lpos, legend = leglab, bg = col[5],
               pch = pch, col = col[1:2], bty = "o", cex = cex[2], pt.bg = col[1:2], lwd = lwd)
      
      if (contour == "ff")
        legend(lpos, legend = leglab[1],
               pch = pch[1], col = col[1], bty = "o", cex = cex[2], bg = col[5], pt.bg = col[1], lwd = lwd)
      
      if (contour == "df")
        legend(lpos, legend = leglab[2],
               pch = pch[2], col = col[2], bty = "o", cex = cex[2], bg = col[5], pt.bg = col[2], lwd = lwd)
    } else
    { 
      legend(lpos, legend = leglab[1],
             pch = pch[2], col = col[2], bty = "o", cex = cex[2], bg = col[5], pt.bg = col[2], lwd = lwd)}
    
    if (frange.detec)
    {
      #second plot
      screen(2)
      
      z <- frng$af.mat[,1]
      zf <- frng$af.mat[,2]
      
      par(mar = c(3.4, 0, 0.5, 0.8))
      
      plot(z, zf, type = "l", ylim = fl, yaxs = "i", xaxs = "i", yaxt = "n", xlab = "", col = "white", xaxt = "n")
      
      # add axis& labels
      axis(1, at = seq(0.2, 1, by = 0.4))
      mtext(side = 1, text = "Amplitude (%)", line = 2.3)
      
      # fix amplitude values to close polygon (just for ploting)
      z3 <- c(0, z, 0)
      
      if (!is.null(bp)) zf3 <- c(b[1], zf, b[2]) else zf3 <- c(fl[1], zf, fl[2])
      
      #addd  extremes to make polygon close fine
      zf3 <- c(lims[1], zf, lims[2])
      
      # plot amplitude values curve
      polygon(cbind(z3, zf3), col= adjustcolor("#E37222", 0.8))
      
      # add border line
      points(z3, zf3, type = "l", col = adjustcolor("gray", 0.5))
      
      # add background color
      rect(xleft = 0, ybottom = fl[1], xright = 1, ytop = fl[2], col = adjustcolor("#4D69FF", 0.05))
      
      #add green polygon on detected frequency bands
      rect(xleft = 0, ybottom = b[1], xright = 1, ytop = b[2], col = adjustcolor("green3", 0.1), border = adjustcolor("gray", 0.2))
      
      # add gray boxes in filtered out freq bands
      if (!is.null(bp))
      {  rect(xleft = 0, ybottom = bp[2], xright = 1, ytop = fl[2], col = adjustcolor("gray", 0.5)) 
        rect(xleft = 0, ybottom = fl[1], xright = 1, ytop = bp[1], col = adjustcolor("gray", 0.5))
      }
      
      # add line to highligh freq range
      abline(v = threshold.freq/100, col = adjustcolor("blue4", 0.7), lty = 3, lwd = 2.3)
      abline(h = b, col = "#80C3FF", lty = 3, lwd = 1.1)
      
      if (title)
      {
        screen(3)
        par( mar = rep(0, 4))
        plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
        
        if (is.null(img.suffix))
          text(x = 0.5, y = 0.35, labels = paste(X$sound.files[i], X$selec[i], sep = "-"), cex = cexlab, font = 2) else
            text(x = 0.5, y = 0.35, labels = paste(X$sound.files[i], X$selec[i], img.suffix, sep = "-"), cex = cexlab, font = 2)
      }
      
    }
    
    invisible() 
    dev.off()
    return(NULL)
    
  }
  
  
  
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  out <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(X), cl = cl, FUN = function(i) 
  { 
    trackfreFUN(X = X, i = i, mar = mar, flim = flim, xl = xl, picsize = picsize, res = res, wl = wl, wl.freq = wl.freq, cexlab = cexlab, inner.mar = inner.mar, outer.mar = outer.mar, bp = bp, cex = cex, threshold.time = threshold.time, threshold.freq = threshold.freq, pch = pch,
                custom.contour)
  }) 
}



##############################################################################################################
#' alternative name for \code{\link{track_freq_contour}}
#'
#' @keywords internal
#' @details see \code{\link{track_freq_contour}} for documentation. \code{\link{trackfreqs}} will be deprecated in future versions.
#' @export

trackfreqs <- track_freq_contour
