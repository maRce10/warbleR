#' Extract the dominant frequency values as a time series
#' 
#' \code{dfts} extracts the dominant frequency values as a time series.
#' of signals selected by \code{\link{manualoc}} or \code{\link{autodetec}}.
#' @usage dfts(X, wl = 512, wl.freq = 512, length.out = 20, wn = "hanning", ovlp = 70,
#' bp = c(0, 22), threshold = 0, threshold.time = NULL, threshold.freq = NULL, 
#' img = TRUE, parallel = 1, path = NULL, img.suffix = "dfts", pb = TRUE,
#' clip.edges = FALSE, leglab = "dfts", frange.detec = FALSE, fsmooth = 0.1,
#'  raw.contour = FALSE, track.harm = FALSE, adjust.wl = TRUE, ...)
#' @param  X object of class 'selection_table', 'extended_selection_table' or data frame containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param wl.freq A numeric vector of length 1 specifying the window length of the spectrogram
#' for measurements on the frecuency spectrum. Default is 512. Higher values would provide 
#' more accurate measurements.
#' @param length.out A numeric vector of length 1 giving the number of measurements of dominant 
#' frequency desired (the length of the time series).
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70. 
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param threshold amplitude threshold (\%) for dominant frequency detection. Default is 0. Note that amplitude 
#' threshold for time and frequency domains can be defined independently. See "threshold.time" and "threshold.freq" 
#' arguments. 
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
#' @param threshold.time amplitude threshold (\%) for the time domain. Use for dominant frequency detection. If \code{NULL} (default) then the 'threshold' value is used.
#' @param threshold.freq amplitude threshold (\%) for the frequency domain. Use for frequency range detection from the spectrum (see 'frange.detec'). If \code{NULL} (default) then the
#'  'threshold' value is used.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located.
#' @param img.suffix A character vector of length 1 with a sufix (label) to add at the end of the names of 
#' image files.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param clip.edges Logical argument to control whether edges (start or end of signal) in
#' which amplitude values above the threshold were not detected will be removed. If 
#' \code{TRUE} this edges will be excluded and signal contour will be calculated on the
#' remainging values. Default is \code{FALSE}. 
#' @param leglab A character vector of length 1 or 2 containing the label(s) of the frequency contour legend 
#' in the output image.
#' @param frange.detec Logical. Controls whether frequency range of signal is automatically 
#' detected  using the \code{\link{frange.detec}} function. If so, the range is used as the 
#' bandpass filter (overwriting 'bp' argument). Default is \code{FALSE}.
#' @param fsmooth A numeric vector of length 1 to smooth the frequency spectrum with a mean
#'  sliding window (in kHz) used for frequency range detection (when \code{frange.detec = TRUE}). This help to average amplitude "hills" to minimize the effect of
#'  amplitude modulation. Default is 0.1. 
#' @param raw.contour Logical. If \code{TRUE} then a list with the original contours 
#'  (i.e. without interpolating values to make all contours of equal length) is returned (and no images are produced). 
#' @param track.harm Logical. If true warbleR's \code{\link{track_harm}} function is 
#' used to track frequency contours. Otherwise seewave's \code{\link[seewave]{dfreq}} is used by default.
#' @param adjust.wl Logical. If \code{TRUE} 'wl' (window length) is reset to be lower than the 
#' number of samples in a selection if the number of samples is less than 'wl'. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{\link{trackfreqs}}.
#' @return The function returns a data frame with the dominant frequency values measured across the signals.  If \code{raw.contour = TRUE} a list with the raw frequency detections (i.e. without interpolating values to make all contours of equal length) is returned. If img is 
#' \code{TRUE} it also produces image files with the spectrograms of the signals listed in the 
#' input data frame showing the location of the dominant frequencies 
#' (see \code{\link{trackfreqs}} description for more details).
#' @family spectrogram creators
#' @seealso \code{\link{sig2noise}}, \code{\link{trackfreqs}}, \code{\link{sp.en.ts}}, \code{\link{ffts}}, \code{\link{ffDTW}}, \code{\link{dfDTW}}
#' @export
#' @name dfts
#' @details This function extracts the dominant frequency values as a time series. 
#' The function uses the \code{\link[stats]{approx}} function to interpolate values between dominant frequency 
#' measures. If there are no frequencies above the amplitude threshold at the begining or end 
#'  of the signals then NAs will be generated. On the other hand, if there are no frequencies 
#'  above the amplitude threshold in time windows in between the signal in which amplitude was 
#'  detected then the values of the adjacent will be interpolated 
#'  to fill out the missing values (e.g. no NAs in between detected amplitude segments). 
#' 
#' @examples{
#' # set the temp directory
#' # setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
#' writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
#' writeWave(Phae.long1, "Phae.long1.wav")
#' 
#' # run function 
#' dfts(X = lbh_selec_table, length.out = 30, flim = c(1, 12), bp = c(2, 9), wl = 300, pb = FALSE)
#' 
#' # note a NA in the row 4 column 3 (dfreq-1)
#' # this can be removed by clipping edges (removing NAs at the start and/or end 
#' # when no freq was detected) 
#' 
#' dfts(X = lbh_selec_table, length.out = 30, flim = c(1, 12), bp = c(2, 9), wl = 300, pb = FALSE, 
#' clip.edges = TRUE)
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on march-12-2018 (MAS)

dfts <- function(X, wl = 512, wl.freq = 512, length.out = 20, wn = "hanning", ovlp = 70, 
                  bp = c(0, 22), threshold = 0, threshold.time = NULL, threshold.freq = NULL,
                  img = TRUE, parallel = 1,
                  path = NULL, img.suffix = "dfts", pb = TRUE, clip.edges = FALSE, leglab = "dfts", frange.detec = FALSE, fsmooth = 0.1, raw.contour = FALSE, 
                  track.harm = FALSE, adjust.wl = TRUE, ...){     
  
  # reset working directory and default parameters
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  options(digits = 5)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(dfts)
  
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
  if (is.null(path)) path <- getwd() else {if (!dir.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  if (!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if (all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'start' and 'end' must be numeric")
  
  #if any start higher than end stop
  if (any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))  
  
  #if any selections longer than 20 secs stop
  if (any(X$end - X$start>20)) stop(paste(length(which(X$end - X$start>20)), "selection(s) longer than 20 sec"))  
  
  #if bp is not vector or length!=2 stop
  # if (!is.null(bp)) {if (!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
  #   if (!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}}

  # bp checking
  if (bp[1] != "frange")
  {if (!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if (!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")} 
  } else
  {if (!any(names(X) == "bottom.freq") & !any(names(X) == "top.freq")) stop("'bp' = frange requires bottom.freq and top.freq columns in X")
    if (any(is.na(c(X$bottom.freq, X$top.freq)))) stop("NAs found in bottom.freq and/or top.freq") 
    if (any(c(X$bottom.freq, X$top.freq) < 0)) stop("Negative values found in bottom.freq and/or top.freq") 
    if (any(X$top.freq - X$bottom.freq < 0)) stop("top.freq should be higher than low.f")
  }
  
  # If length.out is not numeric
  if (!is.numeric(length.out)) stop("'length.out' must be a numeric vector of length 1") 
  if (any(!(length.out %% 1 == 0),length.out < 1)) stop("'length.out' should be a positive integer")
  
  # threshold adjustment
  if (is.null(threshold.time)) threshold.time <- threshold
  if (is.null(threshold.freq)) threshold.freq <- threshold
  
  #return warning if not all sound files were found
  if (!is_extended_selection_table(X)){
   recs.wd <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if (length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files)) & pb) 
    cat(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% recs.wd) 
  if (length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else X <- X[d, , drop = FALSE]
  }
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # no img if raw.contour
  if (raw.contour) img <- FALSE
  
  if (pb) if (img) cat("Creating spectrograms overlaid with dominant frequency measurements:") else
    cat("measuring dominant frequency:") 
  
  dftsFUN <- function(X, i, bp, wl, threshold.time, threshold.freq, fsmooth, wl.freq, frange.dtc, raw.contour, track.harm, adjust.wl){
    
    # Read sound files to get sample rate and length
    r <- read_wave(X = X, index = i, header = TRUE)
    f <- r$sample.rate
    
    # if bp is frange
    if (bp[1] == "frange") bp <- c(X$bottom.freq[i], X$top.freq[i])
    
    #in case bp its higher than can be due to sampling rate
    b <- bp 
    if (!is.null(b)) {if (b[2] > ceiling(f/2000) - 1) b[2] <- ceiling(f/2000) - 1 
    b <- b * 1000}
    
    r <- read_wave(X = X, index = i)
    
    if (frange.dtc){
      frng <- frd_wrblr_int(wave = r, wl = wl.freq, fsmooth = fsmooth, threshold = threshold.freq, wn = wn, flim = c(0, 22), bp = b/ 1000, ovlp = ovlp)
    
    if (!all(is.na(frng$frange))) b <- as.numeric(frng$frange) * 1000 
    }
    
    # calculate dominant frequency at each time point     
    dfrq1 <- track_harm(wave = r, f = f, wl = wl, plot = FALSE, ovlp = ovlp, bandpass = b, fftw = TRUE,
                             threshold = threshold.time, dfrq = !track.harm, adjust.wl = adjust.wl)
    
        dfrq <- dfrq1[!is.na(dfrq1[,2]), , drop = FALSE]
        # if (nrow(dfrq1) == 1 & !is.matrix(dfrq)) dfrq <- as.matrix(t(dfrq))
        
        #make NA's the ones outside banpass freqs
        dfrq[dfrq[,2] < b[1]/1000, ] <- NA
        # if (nrow(dfrq1) == 1 & !is.matrix(dfrq)) dfrq <- as.matrix(t(dfrq))
        if (any(is.na(dfrq[1, ]))) {
          dfrq <- dfrq[!is.na(dfrq[ , 1]), , drop = FALSE]
          # if (!is.matrix(dfrq)) dfrq <- as.matrix(t(dfrq))
        }
    
        # make a matrix containing results and name/order columns
        dfrq <- data.frame(dfrq, X$start[i] + dfrq[, 1])
        if (!is.data.frame(dfrq)) dfrq <- as.data.frame(t(dfrq))
        colnames(dfrq) <- c("relative.time", "frequency", "absolute.time")
        if (!is.data.frame(dfrq)) dfrq <- as.data.frame(t(dfrq))
        dfrq <- dfrq[, c(3, 1, 2), drop = FALSE]
        
        # remove NAs on edges only if more than 
        if (clip.edges & nrow(dfrq) > 2)
        {
          dfrq <- dfrq[which(as.numeric(is.na(dfrq$frequency)) == 0)[1]:nrow(dfrq), , drop = FALSE]
          
          # clip end edges
          dfrq <- dfrq[1:max(which(as.numeric(is.na(dfrq$frequency)) == 0)), , drop = FALSE]
          }
        
        # interpolate if no raw.contour
        if (!raw.contour)
        {
          #if more than one detection extrapolate else repeat value
        if (nrow(dfrq) > 1 | all(is.na(dfrq[, 3])))
          {
          apdom <- try(approx(x = dfrq$relative.time[!is.na(dfrq$frequency)], y =  dfrq$frequency[!is.na(dfrq$frequency)], 
                               xout = seq(from = min(dfrq$relative.time, na.rm = TRUE),  to = max(dfrq$relative.time, na.rm = TRUE), 
                                          length.out = length.out), method = "linear"), silent = TRUE)
        
      if (class(apdom) == "try-error") apdom <- list(x =  seq(from = 0,  to = X$end[i] - X$start[i], 
                                               length.out = length.out), y = rep(NA, length.out))
          
              } else
                                            # repeat same value length.out times if only 1 detection
                                          { 
                                            apdom <- dfrq[rep(1, length.out), , drop = FALSE]
                                            apdom[, 1] <- seq(from = X$start[i],  to = X$end[i],
                                                              length.out = length.out)
                                          
                                            apdom[, 2] <- apdom[, 1] - X$start[i] 
                                           colnames(apdom)[3] <- "y"
                                             }                          
         
        if (clip.edges & !raw.contour) 
        {
 
          #fix for ploting with trackfreqs
          dfrq1[,2][is.na(dfrq1[,2])] <- 0
          
          #calculate time at start and end with no amplitude detected (duration of clipped edges)
          durend1 <- suppressWarnings(diff(range(dfrq1[,1][rev(cumsum(rev(dfrq1[,2])) == 0)])))
          durend <- durend1
          if (is.infinite(durend) | is.na(durend)) durend <- 0
          
          durst1 <- suppressWarnings(diff(range(dfrq1[,1][cumsum(dfrq1[,2]) == 0])))   
          durst <- durst1
          if (is.infinite(durst) | is.na(durst)) durst <- 0
          
          by.dur <- mean(diff(apdom$x))
          clipst <- length(seq(from = 0, to = durst, by = by.dur))
          clipend <- length(seq(from = 0, to = durend, by = by.dur))
          
          apdom1 <- apdom
          apdom1 <- list(x = apdom$x, y = apdom$y)
          apdom1$y <- c(rep(NA, clipst) ,apdom, rep(NA, clipend))
          
          if (is.infinite(durst1) | is.na(durst1)) apdom1$y <- apdom1$y[-1]
          if (is.infinite(durend1) | is.na(durend1)) apdom1$y <- apdom1$y[-length(apdom1$y)]
       
          cstm.cntr <- data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(apdom1$y))
           }
            
           if (!raw.contour)
             cstm.cntr <- data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(apdom$y)) else
               cstm.cntr <- dfrq
            }        

    if (img)  
    {
      trackfreqs(X = X[i, , drop = FALSE], wl = wl, wl.freq = wl.freq, osci = FALSE, leglab = leglab, pb = FALSE, wn = wn, threshold.time = threshold.time, threshold.freq = threshold.freq, bp = bp, 
                 parallel = 1, path = path, img.suffix = img.suffix, ovlp = ovlp,
                 custom.contour = cstm.cntr, fsmooth = fsmooth, frange.detec = FALSE, ...)
      } 
    if (!raw.contour) return(t(apdom$y))  else return(dfrq)  
  } 
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  lst <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(i) 
  { 
 dftsFUN(X, i, bp, wl, threshold.time, threshold.freq, fsmooth, wl.freq, frange.dtc = frange.detec, raw.contour, track.harm, adjust.wl)
  }) 
  
  if (any(sapply(lst, class) == "try-error") & !adjust.wl) stop("error during frequency detection for at least 1 selection (try 'adjust.wl = TRUE' and/or a lower threshold)") else 
    if (any(sapply(lst, class) == "try-error"))
    stop("error during frequency detection for at least 1 selection (try a lower threshold)")
  
  if (!raw.contour)
{  
    
   df <- data.frame(sound.files = X$sound.files, selec = X$selec, do.call(rbind, lst))
  
    # df <- data.frame(sound.files = X$sound.files, selec = X$selec, (as.data.frame(matrix(unlist(lst),nrow = length(X$sound.files), byrow = TRUE))))
    colnames(df)[3:ncol(df)]<-paste("dfreq",1:(ncol(df)-2),sep = "-")
    
    df[ , which(!sapply(df[, -c(1:2)], function(x) all(is.na(x)))) + 2] <- round(df[ , which(!sapply(df[, -c(1:2)], function(x) all(is.na(x)))) + 2], 3)
    
    return(df)
    }  else
         {
          names(lst) <- paste(X$sound.files, X$selec, sep = "-") 
         return(lst)
          }
    }


##############################################################################################################
#' alternative name for \code{\link{dfts}}
#'
#' @keywords internal
#' @details see \code{\link{dfts}} for documentation. \code{\link{dfts}} will be deprecated in future versions.
#' @export

df_ts <- dfts
