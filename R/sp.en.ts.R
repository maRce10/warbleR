#' Extract the spectral entropy across signals as a time series
#' 
#' \code{sp.en.ts} spectral entropy across signals as a time series.
#' of signals selected by \code{\link{manualoc}} or \code{\link{sp.en.ts}}.
#' @usage sp.en.ts(X, wl = 512, length.out = 20, wn = "hanning", ovlp = 70, bp = "frange",
#'   threshold = 15, img = TRUE, parallel = 1, path = NULL, img.suffix = "sp.en.ts",
#'    pb = TRUE, clip.edges = FALSE, leglab = "sp.en.ts", sp.en.range = c(2, 10), ...)
#' @param  X object of class 'selection_table', 'extended_selection_table' or data 
#' frame containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can  also be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512. Note that this is particularly important for measuring spectral entropy. Low values (~100) 
#'   generate a very detail contour of the variation in spectral entropy that is probably not useful for 
#'   assesing signal similarity.
#' @param length.out A character vector of length 1 giving the number of measurements of spectral entropy 
#'  desired (the length of the time series).
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70. 
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz). If 'frange' (default) then the
#'   'bottom.freq' and 'top.freq' columns are used bandpass limits.
#' @param threshold amplitude threshold (\%) for dominant frequency detection. Default is 15.
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
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
#' @param sp.en.range Numeric vector of length 2. Range of frequency in which to display the entropy values 
#' on the spectrogram (when img = TRUE). Default is c(2, 10). Negative values can be used in order to stretch more
#' the range. 
#' @param ... Additional arguments to be passed to \code{\link{trackfreqs}} for customizing
#' graphical output.
#' @return A data frame with the dominant frequency values measured across the signals. If img is 
#' \code{TRUE} it also produces image files with the spectrograms of the signals listed in the 
#' input data frame showing the location of the dominant frequencies 
#' (see \code{\link{trackfreqs}} description for more details).
#' @family spectrogram creators
#' @seealso \code{\link{specreator}} for creating spectrograms from selections,
#'  \code{\link{snrspecs}} for creating spectrograms to 
#'   optimize noise margins used in \code{\link{sig2noise}}
#' @export
#' @name sp.en.ts
#' @details This function spectral entropy across signals as a time series. 
#' The function uses the \code{\link[stats]{approx}} function to interpolate values between spectral 
#' entropy measures (calculated with \code{\link[seewave]{csh}}). If there are no frequencies above the amplitude theshold 
#' at the begining or end  of the signals then NAs will be generated. On the other hand, 
#' if there are no frequenciesabove the amplitude theshold in between signal segments in which amplitude was 
#'  detected then the values of this adjacent segments will be interpolated 
#'  to fill out the missing values (e.g. no NAs in between detected amplitude segments). Missing values at the start
#'  of end can be removed with "clip.edges".
#' 
#' @examples
#' {
#' # set the temp directory
#' # setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2",  "Phae.long3",  "Phae.long4","lbh_selec_table"))
#' writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
#' writeWave(Phae.long1, "Phae.long1.wav")
#' writeWave(Phae.long3, "Phae.long3.wav") #save sound files 
#' writeWave(Phae.long4, "Phae.long4.wav")
#' 
#' # without clip edges
#' sp.en.ts(X = lbh_selec_table, threshold = 10, clip.edges = FALSE, length.out = 10,
#'  type = "b", sp.en.range = c(-25, 10))
#' 
#' # with clip edges and length.out 10
#' sp.en.ts(X = lbh_selec_table, threshold = 10, bp = c(2, 12), clip.edges = TRUE, length.out = 10)
#' 
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on mar-13-2018 (MAS)

sp.en.ts <-  function(X, wl = 512, length.out = 20, wn = "hanning", ovlp = 70, 
                  bp = "frange", threshold = 15, img = TRUE, parallel = 1,
                  path = NULL, img.suffix = "sp.en.ts", pb = TRUE, clip.edges = FALSE,
                  leglab = "sp.en.ts", sp.en.range = c(2, 10), ...){     

  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(sp.en.ts)
  
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
  if (any(X$end - X$start <= 0)) stop(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))  
  
  #if any selections longer than 20 secs stop
  if (any(X$end - X$start>20)) stop(paste(length(which(X$end - X$start>20)), "selection(s) longer than 20 sec"))  
  options( show.error.messages = TRUE)
  
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
  
  #if sp.en.range is not vector or length!=2 stop
  if (!is.vector(sp.en.range)) stop("'sp.en.range' must be a numeric vector of length 2") else
    if (!length(sp.en.range) == 2) stop("'sp.en.range' must be a numeric vector of length 2")
  
  # If length.out is not numeric
  if (!is.numeric(length.out)) stop("'length.out' must be a numeric vector of length 1") 
  if (any(!(length.out %% 1 == 0),length.out < 1)) stop("'length.out' should be a positive integer")
  
  #return warning if not all sound files were found
  if (!is_extended_selection_table(X))
  {
  recs.wd <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if (length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files)) & pb) 
    cat(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% recs.wd) 
  if (length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else 
  X <- X[d, ]
  }
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
 if (pb) {if (img) cat("Creating spectrograms overlaid with dominant frequency measurements:") else
    cat("Measuring spectral entropy:")}  
  
  sp.en.tsFUN <- function(X, i, bp, wl, threshold, sp.en.range){
    
    # Read sound files to get sample rate and length
    r <- read_wave(X = X, index = i, header = TRUE)
    f <- r$sample.rate

    # if bp is frange
    if (bp[1] == "frange") bp <- c(X$bottom.freq[i], X$top.freq[i])
    
    #in case bp its higher than can be due to sampling rate
    b<- bp 
    if (!is.null(b)) {if (b[2] > ceiling(f/2000) - 1) b[2] <- ceiling(f/2000) - 1 
    b <- b * 1000}
    
      r <- read_wave(X = X, index = i)
    
      #filter if this was needed
      if (!is.null(bp)) r <- ffilter(wave = r, from = b[1], to = b[2]) 
      
      # measure espectral entropy
      sp.en <- csh(wave = r, f = f, wl = wl, ovlp = ovlp, wn = wn, 
                   threshold = threshold, plot = F)
        
      if (clip.edges) 
      {    #remove initial values with 0
        sp.en1 <- sp.en[cumsum(sp.en[,2]) != 0, ]
        
        #remove end values with 0
        sp.en1 <- sp.en1[rev(cumsum(rev(sp.en1[,2])) != 0),]
        
      } else sp.en1 <- sp.en
      
      apen <- approx(sp.en1[,1], sp.en1[,2], xout = seq(from = sp.en1[1, 1],
                            to = sp.en1[nrow(sp.en1), 1], length.out = length.out),
                     method = "linear")  
      
      #fix for ploting with trackfreqs
      if (clip.edges) 
      { apen1 <- approx(sp.en[,1], sp.en[,2], xout = seq(from = sp.en[1, 1],
                            to = sp.en[nrow(sp.en), 1], length.out = length.out),
                        method = "linear")
      
      #make 0s at start and end NAs so they are plot at the bottom by trackfreqs
      apen1$y[cumsum(apen1$y) == 0] <- NA
      apen1$y[rev(cumsum(rev(apen1$y))) == 0] <- NA
      }  else apen1 <- apen
      
      correc.apen <- sp.en.range[1] + (sp.en.range[2] - sp.en.range[1]) * apen1$y 
      
  if (img) 
      trackfreqs(X[i, , drop = FALSE], wl = wl, osci = FALSE, leglab = leglab, pb = FALSE, wn = wn,
                 parallel = 1, path = path, img.suffix =  img.suffix, ovlp = ovlp,
                 custom.contour = data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(correc.apen)), ...)
      
      
    return(apen$y)  
  } 

  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  lst <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(i) 
  { 
    sp.en.tsFUN(X, i, bp, wl, threshold, sp.en.range)
  }) 
  
  # put data together
  df <- data.frame(sound.files = X$sound.files, selec = X$selec, (as.data.frame(matrix(unlist(lst),nrow = length(X$sound.files), byrow = TRUE))))
    colnames(df)[3:ncol(df)] <- paste("sp.en",1:(ncol(df)-2),sep = "-")
    df[ ,3:ncol(df)] <- round(df[ ,3:ncol(df)], 3)   

  return(df)

}


##############################################################################################################
#' alternative name for \code{\link{sp.en.ts}}
#'
#' @keywords internal
#' @details see \code{\link{sp.en.ts}} for documentation. \code{\link{sp.en.ts}} will be deprecated in future versions.
#' @export

se_ts <- sp.en.ts
