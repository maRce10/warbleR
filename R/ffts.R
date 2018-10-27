#' Extract the fundamental frequency values as a time series
#' 
#' \code{ffts} extracts the fundamental frequency values as a time series
#' of signals selected by \code{\link{manualoc}} or \code{\link{autodetec}}.
#' @usage ffts(X, wl = 512, length.out = 20, wn = "hanning", ovlp = 70, bp = c(0, 22),
#'   threshold = 15, img = TRUE, parallel = 1, path = NULL, img.suffix = "ffts", pb = TRUE, 
#'   clip.edges = FALSE, leglab = "ffts", ff.method = "seewave", ...)
#' @param X object of class 'selection_table', 'extended_selection_table' or data 
#' frame containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param length.out A numeric vector of length 1 giving the number of measurements of fundamental 
#' frequency desired (the length of the time series).
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70. 
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param threshold amplitude threshold (\%) for fundamental frequency detection. Default is 15.
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used. 
#' @param img.suffix A character vector of length 1 with a suffix (label) to add at the end of the names of 
#' image files.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param clip.edges Logical argument to control whether edges (start or end of signal) in
#' which amplitude values above the threshold were not detected will be removed. If 
#' \code{TRUE} this edges will be excluded and signal contour will be calculated on the
#' remainging values. Default is \code{FALSE}. 
#' #' @param leglab A character vector of length 1 or 2 containing the label(s) of the frequency contour legend 
#' in the output image.
#' @param leglab A character vector of length 1 or 2 containing the label(s) of the frequency contour legend 
#' in the output image.
#' @param ff.method Character. Selects the method used to calculate the fundamental
#' frequency. Either 'tuneR' (using \code{\link[tuneR]{FF}}) or 'seewave' (using 
#' \code{\link[seewave]{fund}}). Default is 'seewave'. 'tuneR' performs
#' faster (and seems to be more accurate) than 'seewave'.  
#' @param ... Additional arguments to be passed to \code{\link{trackfreqs}}. for customizing
#' graphical output.
#' @return A data frame with the fundamental frequency values measured across the signals. If img is 
#' \code{TRUE} it also produces image files with the spectrograms of the signals listed in the 
#' input data frame showing the location of the fundamental frequencies 
#' (see \code{\link{trackfreqs}} description for more details).
#' @family spectrogram creators
#' @seealso \code{\link{sig2noise}}, \code{\link{trackfreqs}}, \code{\link{dfts}}, \code{\link{ffDTW}}, \code{\link{dfDTW}}
#' @export
#' @name ffts
#' @details This function extracts the fundamental frequency values as a time series. 
#' The function uses the \code{\link[stats]{approx}} function to interpolate values between fundamental frequency #' measures. If there are no frequencies above the amplitude theshold at the begining or end 
#'  of the signals then NAs will be generated. On the other hand, if there are no frequencies 
#'  above the amplitude theshold in between signal segments in which amplitude was 
#'  detected then the values of this adjacent segments will be interpolated 
#'  to fill out the missing values (e.g. no NAs in between detected amplitude segments). 
#' @examples{
#' # set the temp directory
#' # setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
#' writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
#' writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
#' 
#' # run function 
#' ffts(lbh_selec_table, length.out = 50, flim = c(1, 12), bp = c(2, 9), wl = 300)
#' 
#' # Fundamental frequency is not accurate for noisy signals, works better with pure tones
#' 
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on oct-26-2016 (MAS)

ffts <- function(X, wl = 512, length.out = 20, wn = "hanning", ovlp = 70, 
                 bp = c(0, 22), threshold = 15, img = TRUE, parallel = 1,
                 path = NULL, img.suffix = "ffts", pb = TRUE, clip.edges = FALSE,
                 leglab = "ffts", ff.method = "seewave", ...){     
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(ffts)
  
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
  options( show.error.messages = TRUE)
  
  #if bp is not vector or length!=2 stop
  if (!is.null(bp)) {if (!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if (!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}}
  
  #if ff.method argument  
  if (!any(ff.method == "seewave", ff.method == "tuneR")) stop(paste("ff.method", ff.method, "is not recognized"))  
  
  #return warning if not all sound files were found
  if (!is_extended_selection_table(X))
  {recs.wd <- list.files(path = getwd(), pattern = "\\.wav$", ignore.case = TRUE)
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
  
  if (pb) {if (img) cat("Creating spectrograms overlaid with fundamental frequency measurements:") else
    cat("Measuring fundamental frequency:")}
  
  fftsFUN <- function(X, i, bp, wl, threshold){
    
    # Read sound files to get sample rate and length
    r <- read_wave(X = X, index = i, header = TRUE)
    f <- r$sample.rate
    
    b<- bp 
    if (!is.null(b)) {if (b[2] > ceiling(f/2000) - 1) b[2] <- ceiling(f/2000) - 1 
    b <- b * 1000}
    
    r <- read_wave(X = X, index = i)
    
    # calculate fundamental frequency at each time point     
    if (ff.method == "seewave")
      ffreq1 <- seewave::fund(r, fmax= b[2], f = f, ovlp = ovlp, threshold = threshold, plot = FALSE) else
      {
        if (any(slotNames(r) == "stereo")) if (r@stereo) r <- mono(r, which = "both")
        suppressWarnings(ff1 <- tuneR::FF(tuneR::periodogram(r, width = wl, overlap = wl*ovlp / 100), peakheight = (100 - threshold) / 100)/1000)
        ff2 <- seq(0, X$end[i] - X$start[i], length.out = length(ff1))
        
        ffreq1 <- cbind(ff2, ff1)
      }
    
    ffreq <- matrix(ffreq1[!is.na(ffreq1[,2]),], ncol = 2)  
    ffreq <- matrix(ffreq[ffreq[,2] > b[1]/1000,], ncol = 2)

    if (nrow(ffreq) < 2) {
      apfund <- list()
      apfund$x <- ffreq1[, 1]
      apfund$y <- rep(NA, length.out)
      apfund1 <- apfund
    } else {
      if (!clip.edges)  {
        
        # clip start edges
        ffreq <- ffreq[which(as.numeric(is.na(ffreq[ , 2])) == 0)[1]:nrow(ffreq), ]
        
        # clip end edges
        ffreq <- ffreq[1:max(which(as.numeric(is.na(ffreq[ , 2])) == 0)), ]
        
        # interpolate
        apfund <- approx(ffreq[,1], ffreq[,2], xout = seq(from = ffreq1[1, 1],
                                                          to = ffreq1[nrow(ffreq1), 1], length.out = length.out), 
                         method = "linear") 
        apfund1 <- apfund
      } else {
        apfund <- approx(ffreq[,1], ffreq[,2], 
                         xout = seq(from = ffreq[1, 1],  to = ffreq[nrow(ffreq), 1], 
                                    length.out = length.out), method = "linear")
        
        #fix for ploting with trackfreqs
        #calculate time at start and end with no amplitude detected (duration of clipped edges)
        durend1 <- suppressWarnings(diff(range(ffreq1[,1][rev(cumsum(rev(ffreq1[,2])) == 0)])))
        durend <- durend1
        if (is.infinite(durend) | is.na(durend)) durend <- 0
        
        durst1 <- suppressWarnings(diff(range(ffreq1[,1][cumsum(ffreq1[,2]) == 0])))   
        durst <- durst1
        if (is.infinite(durst) | is.na(durst)) durst <- 0
        
        by.dur <- mean(diff(apfund$x))
        clipst <- length(seq(from = 0, to = durst, by = by.dur))
        clipend <- length(seq(from = 0, to = durend, by = by.dur))
        
        apfund1 <- apfund
        apfund1$y <- c(rep(NA, clipst) ,apfund$y, rep(NA, clipend))
        
        if (is.infinite(durst1) | is.na(durst1)) apfund1$y <- apfund1$y[-1]
        if (is.infinite(durend1) | is.na(durend1)) apfund1$y <- apfund1$y[-length(apfund1$y)]
      }
    }
    
    if (img) 
      trackfreqs(X[i, , drop = FALSE], wl = wl, osci = FALSE, leglab = leglab, pb = FALSE, wn = wn,
                 parallel = 1, path = path, img.suffix =  img.suffix, ovlp = ovlp,
                 custom.contour = data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(apfund$y)), ...)
    
    return(apfund$y)  
  } 
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  lst <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(i) 
  { 
    fftsFUN(X, i, bp, wl, threshold)
  })
  
  df <- data.frame(sound.files = X$sound.files, selec = X$selec, as.data.frame(matrix(unlist(lst),nrow = length(X$sound.files), byrow = TRUE)))
  colnames(df)[3:ncol(df)]<-paste("ffreq",1:(ncol(df)-2),sep = "-")
  df[ ,3:ncol(df)] <- round(df[ ,3:ncol(df)], 3)
   
  return(df)
  
}


##############################################################################################################
#' alternative name for \code{\link{ffts}}
#'
#' @keywords internal
#' @details see \code{\link{ffts}} for documentation. \code{\link{ffts}} will be deprecated in future versions.
#' @export

ff_ts <- ffts
