#' Extract the spectral entropy across signals as a time series
#' 
#' \code{sp.en.ts} spectral entropy across signals as a time series.
#' of signals selected by \code{\link{manualoc}} or \code{\link{autodetec}}.
#' @usage sp.en.ts(X, wl = 512, length.out = 20, wn = "hanning", ovlp = 70, bp = NULL,
#'   threshold = 15, img = TRUE, parallel = 1, path = NULL, img.suffix = "sp.en.ts",
#'    pb = TRUE, clip.edges = FALSE, leglab = "sp.en.ts", sp.en.range = c(2, 10), ...)
#' @param  X 'selection.table' object or data frame with results containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
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
#'   frequency bandpass filter (in kHz). Default is \code{NULL}.
#' @param threshold amplitude threshold (\%) for dominant frequency detection. Default is 15.
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#'  Not available in Windows OS.
#' @param path Character string containing the directory path where the sound files are located.
#' @param img.suffix A character vector of length 1 with a sufix (label) to add at the end of the names of 
#' image files.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
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
#' data(list = c("Phae.long1", "Phae.long2",  "Phae.long3",  "Phae.long4","selec.table"))
#' writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
#' writeWave(Phae.long1, "Phae.long1.wav")
#' writeWave(Phae.long3, "Phae.long3.wav") #save sound files 
#' writeWave(Phae.long4, "Phae.long4.wav")
#' 
#' # without clip edges
#' sp.en.ts(X = selec.table, threshold = 10, bp = NULL, clip.edges = FALSE, length.out = 10,
#'  type = "b", sp.en.range = c(-25, 10))
#' 
#' # with clip edges and length.out 10
#' sp.en.ts(X = selec.table, threshold = 10, bp = c(2, 12), clip.edges = TRUE, length.out = 10)
#' 
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on oct-26-2016 (MAS)

sp.en.ts <-  function(X, wl = 512, length.out = 20, wn = "hanning", ovlp = 70, 
                  bp = NULL, threshold = 15, img = TRUE, parallel = 1,
                  path = NULL, img.suffix = "sp.en.ts", pb = TRUE, clip.edges = FALSE,
                  leglab = "sp.en.ts", sp.en.range = c(2, 10), ...){     

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
  
  #if any selections longer than 20 secs stop
  if(any(X$end - X$start>20)) stop(paste(length(which(X$end - X$start>20)), "selection(s) longer than 20 sec"))  
  options( show.error.messages = TRUE)
  
  #if bp is not vector or length!=2 stop
  if(!is.null(bp)) {if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}}
  
  #if sp.en.range is not vector or length!=2 stop
  if(!is.vector(sp.en.range)) stop("'sp.en.range' must be a numeric vector of length 2") else
    if(!length(sp.en.range) == 2) stop("'sp.en.range' must be a numeric vector of length 2")
  
  # If length.out is not numeric
  if(!is.numeric(length.out)) stop("'length.out' must be a numeric vector of length 1") 
  if(any(!(length.out %% 1 == 0),length.out < 1)) stop("'length.out' should be a positive integer")
  
  #return warning if not all sound files were found
  recs.wd <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files)) & pb) 
    message(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% recs.wd) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else 
  X <- X[d, ]
  
  #if parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  #if parallel
  if(all(parallel > 1, img, !Sys.info()[1] %in% c("Linux","Windows"))) {
    parallel <- 1
    message("creating images is not compatible with parallel computing (parallel > 1) in OSX (mac)")
  }
  
  #parallel not available on windows
  if(parallel > 1 & Sys.info()[1] == "Windows")
  {message("parallel computing not availabe in Windows OS for this function")
    parallel <- 1}

 if(pb) {if(img) message("Creating spectrograms overlaid with dominant frequency measurements:") else
    message("Measuring spectral entropy:")}  
  
  sp.en.tsFUN <- function(X, i, bp, wl, threshold, sp.en.range){
    
    # Read sound files to get sample rate and length
    r <- tuneR::readWave(as.character(X$sound.files[i]), header = TRUE)
    f <- r$sample.rate

    #in case bp its higher than can be due to sampling rate
    b<- bp 
    if(!is.null(b)) {if(b[2] > ceiling(f/2000) - 1) b[2] <- ceiling(f/2000) - 1 
    b <- b * 1000}
    
    
      r <- tuneR::readWave(as.character(X$sound.files[i]), from = X$start[i], to = X$end[i], units = "seconds")
    
      #filter if this was needed
      if(!is.null(bp)) r <- ffilter(wave = r, from = b[1], to = b[2]) 
      
      # measure espectral entropy
      sp.en <- csh(wave = r, f = f, wl = wl, ovlp = ovlp, wn = wn, 
                   threshold = threshold, plot = F)
      
      if(clip.edges) 
      {    #remove initial values with 0
        sp.en1 <- sp.en[cumsum(sp.en[,2]) != 0, ]
        
        #remove end values with 0
        sp.en1 <- sp.en1[rev(cumsum(rev(sp.en1[,2])) != 0),]
        
      } else sp.en1 <- sp.en
      
      apen <- approx(sp.en1[,1], sp.en1[,2], xout = seq(from = sp.en1[1, 1],
                            to = sp.en1[nrow(sp.en1), 1], length.out = length.out),
                     method = "linear")  
      
      #fix for ploting with trackfreqs
      if(clip.edges) 
      { apen1 <- approx(sp.en[,1], sp.en[,2], xout = seq(from = sp.en[1, 1],
                            to = sp.en[nrow(sp.en), 1], length.out = length.out),
                        method = "linear")
      
      #make 0s at start and end NAs so they are plot at the bottom by trackfreqs
      apen1$y[cumsum(apen1$y) == 0] <- NA
      apen1$y[rev(cumsum(rev(apen1$y))) == 0] <- NA
      }  else apen1 <- apen
      
      correc.apen <- sp.en.range[1] + (sp.en.range[2] - sp.en.range[1]) * apen1$y 
      
  if(img) 
      trackfreqs(X[i,], wl = wl, osci = FALSE, leglab = leglab, pb = FALSE, wn = wn,
                 parallel = 1, path = path, img.suffix =  img.suffix, ovlp = ovlp,
                 custom.contour = data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(correc.apen)), ...)
      
      
    return(apen$y)  
  } 

  # Run parallel in windows
  if(parallel > 1) {
    if(Sys.info()[1] == "Windows") {
      
      i <- NULL #only to avoid non-declared objects
      
      cl <- parallel::makeCluster(parallel)
      
      doParallel::registerDoParallel(cl)
      
      lst <- foreach::foreach(i = 1:nrow(X)) %dopar% {
        sp.en.tsFUN(X, i, bp, wl, threshold, sp.en.range)
      }
      
      parallel::stopCluster(cl)
      
    } 
    if(Sys.info()[1] == "Linux") {    # Run parallel in Linux
      if(pb)
        lst <- pbmcapply::pbmclapply(1:nrow(X), mc.cores = parallel, function (i) {
          sp.en.tsFUN(X, i, bp, wl, threshold, sp.en.range)
        }) else
      lst <- parallel::mclapply(1:nrow(X), mc.cores = parallel, function (i) {
        sp.en.tsFUN(X, i, bp, wl, threshold, sp.en.range)
      })
    }
    if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
    {
      cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))
      
      doParallel::registerDoParallel(cl)
      
      lst <- foreach::foreach(i = 1:nrow(X)) %dopar% {
        sp.en.tsFUN(X, i, bp, wl, threshold, sp.en.range)
      }
      
      parallel::stopCluster(cl)
      
    }
  } else {
   if(pb)
     lst <- pbapply::pblapply(1:nrow(X), function(i) sp.en.tsFUN(X, i, bp, wl, threshold, sp.en.range)) else
       lst <- lapply(1:nrow(X), function(i) sp.en.tsFUN(X, i, bp, wl, threshold, sp.en.range))
  }
  
  
  df <- data.frame(sound.files = X$sound.files, selec = X$selec, (as.data.frame(matrix(unlist(lst),nrow = length(X$sound.files), byrow = TRUE))))
    colnames(df)[3:ncol(df)]<-paste("sp.en",1:(ncol(df)-2),sep = "-")
    df[ ,3:ncol(df)] <- round(df[ ,3:ncol(df)], 3)   
              return(df)

    }
