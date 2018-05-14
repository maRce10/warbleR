#' Check selection data frames
#' 
#' \code{checksels} checks whether selections can be read by subsequent functions.
#' @usage checksels(X, parallel = 1, path = NULL, check.header = FALSE, pb = TRUE,
#' wav.size = FALSE)
#' @param X 'selection_table' object or data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. Alternatively, a 'selection_table' class object can be input to double check selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input data frame.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param check.header Logical. Controls whether sound file headers correspond to the actual file properties 
#' (i.e. if is corrupted). This could significantly affect the performance of the function (much slower) particularly 
#' with long sound files.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param wav.size Logical argument to control if the size of the wave object 
#'  when the selection is imported into R (as when using \code{\link[tuneR]{readWave}}
#'  is calculated and added as a column. Size is return in MB. Default is \code{FALSE}.
#' @return A data frame including the columns in the input data frame (X) and 2 additional columns:
#' "check.res" (check selections), and "min.n.samples" (the smallest number of samples). Note the number of samples available
#' in a selection limits the minimum window length (wl argument in other functions) that can be used in batch analyses.
#' @details This function checks 1) if the selections listed in the data frame correspond to .wav files
#' in the working directory, 2) if the sound files can be read and if so, 3) if the start and end time
#' of the selections are found within the duration of the sound files. Note that the sound files 
#' should be in the working directory (or the directory provided in 'path').
#' This is useful for avoiding errors in dowstream functions (e.g. \code{\link{specan}}, \code{\link{xcorr}}, \code{\link{catalog}}, \code{\link{dfDTW}}). Note that corrupt files can be
#' fixed using \code{\link{fixwavs}}) ('sox' must be installed to be able to run this function).
#' @seealso \code{\link{checkwavs}}
#' @export
#' @name checksels
#' @export
#' @rdname check_sels
#' @examples{
#' # First set temporary folder
#' # setwd(tempdir())
#' 
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' 
#' checksels(X = selec.table)
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-5-2016 (MAS)

checksels <- function(X = NULL, parallel =  1, path = NULL, check.header = FALSE, 
                      pb = TRUE, wav.size = FALSE){
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(checksels)
  
  # get warbleR options
  opt.argms <- .Options$warbleR
  
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
  if(is.null(path)) path <- getwd() else {if(!file.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  #if X is not a data frame
  if(all(!any(is.data.frame(X), is_selection_table(X)))) stop("X is not of a class 'data.frame' or 'selection_table'")
  
  if (is_extended_selection_table(X)) stop("checksels does not work on objects of class 'extended_selection_table'")
  
  if(!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  #if end or start are not numeric stop
  if(all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'end' and 'selec' must be numeric")
  
  #if there are NAs in start or end stop
  if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  if(any(duplicated(paste(X$sound.files, X$selec)))) stop("Duplicated selection labels for one or more sound files")
  
  #if any start higher than end stop
  if(any(X$end - X$start < 0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start < 0)), "case(s)"))  
  
  #check additional columns
  if(!"channel" %in% colnames(X)) 
  {#cat("\n sound file channel for analysis assumed to be 1 (left) for all selections (channel column not found)")
    X$channel <- 1
  } else {
    if(!is.numeric(X$channel)) stop("'channel' must be numeric")
    if(any(is.na(X$channel))) {cat("NAs in 'channel', assumed to be channel 1")
      X$channel[is.na(X$channel)] <- 1   
    }}
  
  #check if files are in working directory
  files <- list.files(pattern = "wav$", ignore.case = TRUE)
  if (length(files) == 0) 
    stop("no .wav files in working directory")
  
  #if any selection labels are repeated within a sound file
  if(length(unique(paste(X$sound.files, X$selec))) != nrow(X))
    stop("Repeated selection labels within (a) sound file(s)")  
  
  # update to new frequency range column names
  if(any(grepl("low.freq|high.freq", names(X)))) { 
    names(X)[names(X) == "low.freq"] <- "bottom.freq"
    names(X)[names(X) == "high.freq"] <- "top.freq"
  }
  
  #check frequency range columns
  if("top.freq" %in% colnames(X)) 
  {
    #if any start higher than end stop
    if(any(X$top.freq - X$bottom.freq < 0)) stop(paste("The bottom frequency is higher than the top frequency in", length(which(X$top.freq - X$bottom.freq < 0)), "case(s)"))  
    if(any(X$bottom.freq < 0)) stop("bottom frequency lower than 0 for some selections")  
  }    
  
  #function to run over each sound file
  csFUN <- function(x, X){
    Y <- X[X$sound.files == x, ]
    
    if(file.exists(as.character(x))){
      rec <- try(suppressWarnings(tuneR::readWave(as.character(x), header = TRUE)), silent = TRUE)
      
      if(!class(rec) == "try-error")
      {
        if(check.header)  
        {
          recfull <- try(suppressWarnings(tuneR::readWave(as.character(x), header = FALSE)), silent = TRUE)
          if(any(methods::slotNames(recfull) == "stereo")) 
          {
            if(rec$channels == 2) channel.check <- ifelse(recfull@stereo, FALSE, TRUE) else
              channel.check <- ifelse(!recfull@stereo, FALSE, TRUE)
            
            samples.check <- ifelse(rec$samples == length(recfull@left), FALSE, TRUE) 
          } else {
            channel.check <- FALSE
            samples.check <- ifelse(rec$samples == length(recfull@.Data), FALSE, TRUE)
          }
          
          
          if(any(rec$sample.rate != recfull@samp.rate, rec$bits != recfull@bit, channel.check, samples.check))
          {
            Y$check.res <- "file header corrupted"
            Y$duration <- NA
            Y$min.n.samples <- NA
            Y$sample.rate <- NA
            Y$channels <- NA
            Y$bits <- NA
            Y$sound.file.samples <- NA
          } else
          { 
            maxdur <- rec$samples/rec$sample.rate  
            Y$check.res <- "OK"
            
            if(any(Y$end > maxdur))  Y$check.res[Y$end > maxdur] <- "exceeds sound file length"
            Y$duration <- Y$end - Y$start
            Y$min.n.samples <- floor(Y$duration * rec$sample.rate)
            Y$sample.rate <- rec$sample.rate
            Y$channels <- rec$channels
            Y$bits <- rec$bits
            Y$sound.file.samples <- rec$samples
          }
          
        } else
        { maxdur <- rec$samples/rec$sample.rate  
        Y$check.res <- "OK"
        
        if(any(Y$end > maxdur))  Y$check.res[Y$end > maxdur] <- "exceeds sound file length"
        Y$duration <- Y$end - Y$start
        Y$min.n.samples <- floor(Y$duration * rec$sample.rate)
        Y$sample.rate <- rec$sample.rate
        Y$channels <- rec$channels
        Y$bits <- rec$bits
        Y$sound.file.samples <- rec$samples
        }
      } else {        
        Y$check.res <- "Sound file can't be read"
        Y$duration <- NA
        Y$min.n.samples <- NA
        Y$sample.rate <- NA
        Y$channels <- NA
        Y$bits <- NA
      }    } else {
        Y$check.res <- "sound file not found"
        Y$duration <- NA
        Y$min.n.samples <- NA
        Y$sample.rate <- NA
        Y$channels <- NA
        Y$bits <- NA
        Y$sound.file.samples <- NA
        }
    return(Y)
  }
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  out <- pbapply::pblapply(X = unique(X$sound.files), cl = cl, FUN = function(x) 
  { 
    csFUN(x, X)
  }) 
  
  res <- do.call(rbind, out)
  res <- res[match(paste(X$sound.files, X$selec), paste(res$sound.files, res$selec)),]
  
  if("top.freq" %in% names(res))
  {   
    try(res$check.res <- ifelse((res$sample.rate/2000) - res$top.freq < 0 & !is.na(res$sample.rate), gsub("OK\\|", "", paste(res$check.res, "'Top.freq' higher than half the sample rate", sep = "|")), res$check.res), silent = TRUE)
    
    if(any((((res$sample.rate[!is.na(res$duration)])/2000) - res$top.freq[!is.na(res$duration)]) < 0)) cat("\n 'top.freq' higher than half the sample rate in some selections")     
  } 
  
  if(any(res$channel[!is.na(res$duration)] > res$channels[!is.na(res$duration)])) {cat("\n some selections listed with channel 2 in sound files with only 1 channel, relabeled as channel 1") 
    res$channel[!is.na(res$duration)][any(res$channel[!is.na(res$duration)] > res$channels[!is.na(res$duration)])] <- 1
  }
  
  if (wav.size) res$wav.size.MB <- round(res$bits  * res$channel * res$sample.rate * res$duration / 4) / 1024

  return(res) 
  
}
