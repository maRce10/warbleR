#' Extract the fundamental frequency values as a time series
#' 
#' \code{ffts} extracts the fundamental frequency values as a time series
#' of signals selected by \code{\link{manualoc}} or \code{\link{autodetec}}.
#' @usage ffts(X, wl = 512, length.out = 20, wn = "hanning", ovlp = 70, bp = c(0, 22),
#'   threshold = 15, img = TRUE, parallel = 1, path = NULL, img.suffix = "ffts", pb = TRUE, 
#'   clip.edges = FALSE, leglab = "ffts", ...)
#' @param  X Data frame with results containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param length.out A character vector of length 1 giving the number of measurements of fundamental 
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
#'  Not available in Windows OS.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used. 
#' @param img.suffix A character vector of length 1 with a suffix (label) to add at the end of the names of 
#' image files.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @param clip.edges Logical argument to control whether edges (start or end of signal) in
#' which amplitude values above the threshold were not detected will be removed. If 
#' \code{TRUE} this edges will be excluded and signal contour will be calculated on the
#' remainging values. Default is \code{FALSE}. 
#' #' @param leglab A character vector of length 1 or 2 containing the label(s) of the frequency contour legend 
#' in the output image.
#' @param leglab A character vector of length 1 or 2 containing the label(s) of the frequency contour legend 
#' in the output image.
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
#' @examples
#' \dontrun{
#' # set the temp directory
#' setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2","selec.table"))
#' writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
#' writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
#' 
#' # run function 
#' ffts(selec.table, length.out = 50, flim = c(1, 12), bp = c(2, 9), wl = 300)
#' 
#' Note that fundamental frequency is not accurate for noisy signals, works better with pure tones
#' 
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on oct-26-2016 (MAS)

ffts <- function(X, wl = 512, length.out = 20, wn = "hanning", ovlp = 70, 
                              bp = c(0, 22), threshold = 15, img = TRUE, parallel = 1,
                 path = NULL, img.suffix = "ffts", pb = TRUE, clip.edges = FALSE, leglab = "ffts", ...){     
  
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
  options( show.error.messages = TRUE)
  
  #if bp is not vector or length!=2 stop
  if(!is.null(bp)) {if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}}
  
  #return warning if not all sound files were found
  recs.wd <- list.files(path = getwd(), pattern = "\\.wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files)) & pb) 
    message(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% recs.wd) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else X <- X[d, ]
  
  #if parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")

 if(any(parallel == 1, Sys.info()[1] == "Linux") & pb) {if(img) message("Creating spectrograms overlaid with fundamental frequency measurements:") else
    message("Measuring fundamental frequency:")}
  
        fftsFUN <- function(X, i, bp, wl, threshold){
    
    # Read sound files to get sample rate and length
    r <- tuneR::readWave(file.path(getwd(), X$sound.files[i]), header = TRUE)
    f <- r$sample.rate
  
    b<- bp 
    if(!is.null(b)) {if(b[2] > ceiling(f/2000) - 1) b[2] <- ceiling(f/2000) - 1 
    b <- b * 1000}
    
    
      r <- tuneR::readWave(as.character(X$sound.files[i]), from = X$start[i], to = X$end[i], units = "seconds")
    
      # calculate fundamental frequency at each time point     
      ffreq1 <- seewave::fund(r, fmax= b[2], f = f, ovlp = ovlp, threshold = threshold, plot = FALSE) 
      ffreq <- ffreq1[!is.na(ffreq1[,2]), ]
      ffreq <- ffreq[ffreq[,2] > b[1]/1000, ]
      
      if(nrow(ffreq) < 2) {
        apfund <- list()
      apfund$x <- ffreq1[, 1]
      apfund$y <- rep(NA, length.out)
      apfund1 <- apfund
      } else {
        if(!clip.edges)  {
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
          if(is.infinite(durend) | is.na(durend)) durend <- 0

          durst1 <- suppressWarnings(diff(range(ffreq1[,1][cumsum(ffreq1[,2]) == 0])))   
          durst <- durst1
          if(is.infinite(durst) | is.na(durst)) durst <- 0
          
          by.dur <- mean(diff(apfund$x))
          clipst <- length(seq(from = 0, to = durst, by = by.dur))
          clipend <- length(seq(from = 0, to = durend, by = by.dur))
          
          apfund1 <- apfund
          apfund1$y <- c(rep(NA, clipst) ,apfund$y, rep(NA, clipend))
          
          if(is.infinite(durst1) | is.na(durst1)) apfund1$y <- apfund1$y[-1]
          if(is.infinite(durend1) | is.na(durend1)) apfund1$y <- apfund1$y[-length(apfund1$y)]
          }
      }
      
    if(img) 
      trackfreqs(X[i,], wl = wl, osci = FALSE, leglab = leglab, pb = FALSE, wn = wn,
                 parallel = 1, path = path, img.suffix =  img.suffix, ovlp = ovlp,
                 custom.contour = data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(apfund$y)), ...)
      
    return(apfund$y)  
  } 

        # Run parallel in windows
        if(parallel > 1) {
          if(Sys.info()[1] == "Windows") {
            
            i <- NULL #only to avoid non-declared objects
            
            cl <- parallel::makeCluster(parallel)
            
            doParallel::registerDoParallel(cl)
            
            lst <- foreach::foreach(i = 1:nrow(X)) %dopar% {
              fftsFUN(X, i, bp, wl, threshold)
            }
            
            parallel::stopCluster(cl)
            
          } 
          if(Sys.info()[1] == "Linux") {    # Run parallel in Linux

            if(pb)
            lst <- pbmcapply::pbmclapply(1:nrow(X), mc.cores = parallel, function (i) {
              fftsFUN(X, i, bp, wl, threshold)
            }) else
                        
            lst <- parallel::mclapply(1:nrow(X), mc.cores = parallel, function (i) {
              fftsFUN(X, i, bp, wl, threshold)
            })
          }
          if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
          {
            cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))
            
            doParallel::registerDoParallel(cl)
            
            lst <- foreach::foreach(i = 1:nrow(X)) %dopar% {
              fftsFUN(X, i, bp, wl, threshold)
            }
            
            parallel::stopCluster(cl)
            
          }
        }
        else {
          if(pb)
          lst <- pbapply::pblapply(1:nrow(X), function(i) fftsFUN(X, i, bp, wl, threshold)) else
            lst <- lapply(1:nrow(X), function(i) fftsFUN(X, i, bp, wl, threshold))
        }
        
  df <- data.frame(sound.files = X$sound.files, selec = X$selec, as.data.frame(matrix(unlist(lst),nrow = length(X$sound.files), byrow = TRUE)))
    colnames(df)[3:ncol(df)]<-paste("ffreq",1:(ncol(df)-2),sep = "-")
                 return(df)
    if(!is.null(path)) setwd(wd)
    }
