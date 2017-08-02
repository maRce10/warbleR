#' Cut selections into individual sound files
#' 
#' \code{cut_sels} cut selections from a selection table into individual sound files.
#' @export cut_sels
#' @usage cut_sels(X, mar = 0.05, parallel = 1, path = NULL, dest.path = NULL, pb = TRUE,
#' labels = c("sound.files", "selec"), overwrite = FALSE, ...)
#' @param X Data frame with results containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signals (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the start and end points of selections,
#' dealineating spectrogram limits. Default is 0.05.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param dest.path Character string containing the directory path where the cut sound files will be saved.
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @param labels String vector. Provides the column names that will be used as labels to
#'  create sound file names. Note that they should provide unique names (otherwise 
#'  sound files will be overwritten). Default is \code{c("sound.files", "selec")}.  
#' @param overwrite Logical. If \code{TRUE} sound files with the same name will be 
#' overwritten. Default is \code{FALSE}.
#' @param ... Additional arguments to be passed to the internal \code{\link[tuneR]{writeWave}}  function for customizing sound file output (e.g. normalization). 
#' @return Sound files of the signals listed in the input data frame.
#' @family selection manipulation, sound file manipulation
#' @seealso \code{\link{seltailor}} for tailoring selections 
#' @name cut_sels
#' @details This function allow users to produce individual sound files from the selections
#' listed in a selection table as in \code{\link{selec.table}}.
#' @examples
#' \dontrun{ 
#' # First set empty folder
#' setwd(tempdir())
#' 
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' # make spectrograms
#' 
#' cut_sels(selec.table)
#'  
#' cut_sels(selec.table, overwrite = TRUE, labels = c("sound.files", "selec", "sel.comment"))
#'  
#'  #check this folder!!
#' getwd()
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) and Grace Smith Vidaurre
#last modification on jul-5-2016 (MAS)

cut_sels <- function(X, mar = 0.05, parallel = 1, path = NULL, dest.path = NULL, pb = TRUE,
                     labels = c("sound.files", "selec"), overwrite = FALSE, ...){
  
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
  if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
    setwd(path)} #set working directory
  
  #check path to working directory
  if(!is.null(dest.path))
  {if(class(try(setwd(dest.path), silent = TRUE)) == "try-error") stop("'dest.path' provided does not exist")} else dest.path <- getwd()
     #set working directory
  
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
  
  #return warning if not all sound files were found
  recs.wd <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files))) 
    (paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
           ".wav file(s) not found"))
  
  #missing label columns
  if(!all(labels %in% colnames(X)))
    stop(paste(paste(labels[!(labels %in% colnames(X))], collapse=", "), "label column(s) not found in data frame"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% recs.wd) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
    X <- X[d, ]
  }
  
  #convert factors to characters
  X[,sapply(X, is.factor)] <- apply(matrix(X[,sapply(X, is.factor)]), 2, as.character)
  
  #remove .wav from sound file names
  X2 <- X
  X2$sound.files <- gsub("\\.wav$", "", X2$sound.files, ignore.case = TRUE)
  
  # If parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  #if parallel and pb in windows
  if(parallel > 1 &  pb & Sys.info()[1] == "Windows") {
    message("parallel with progress bar is currently not available for windows OS")
    message("running parallel without progress bar")
    pb <- FALSE
  } 
  
  #create function to run within Xapply functions downstream     
  cutFUN <- function(X, i, mar, labels, dest.path){
    
    # Read sound files, initialize frequency and time limits for spectrogram
    r <- tuneR::readWave(as.character(X$sound.files[i]), header = TRUE)
    f <- r$sample.rate
    t <- c(X$start[i] - mar, X$end[i] + mar)
    
    # fix margins if below 0 or length of recordings
    mar1 <- mar
    mar2 <- mar1 + X$end[i] - X$start[i]
    
    if (t[1] < 0)  t[1] <- 0
    if(t[2] > r$samples/f) t[2] <- r$samples/f
    
    # Cut wave
    wvcut <- tuneR::readWave(as.character(X$sound.files[i]), from = t[1], to = t[2], units = "seconds")

    
    # save cut
    if(overwrite) unlink(file.path(dest.path, paste0(paste(X2[i, labels], collapse = "-"), ".wav")))

  
    tuneR::writeWave(object = wvcut, filename = file.path(dest.path, paste0(paste(X2[i, labels], collapse = "-"), ".wav")), ...)
       
  }
  
  # Run parallel in windows
  if(parallel > 1) {
    if(Sys.info()[1] == "Windows") {
      
      i <- NULL #only to avoid non-declared objects
      
      cl <- parallel::makeCluster(parallel)
      
      doParallel::registerDoParallel(cl)
      
      out <- foreach::foreach(i = 1:nrow(X)) %dopar% {
        cutFUN(X = X, i = i, mar = mar, labels = labels, dest.path = dest.path)
      }
      
      parallel::stopCluster(cl)
      
    } 
    if(Sys.info()[1] == "Linux") {    # Run parallel in Linux
      
      if(pb)       
        out <- pbmcapply::pbmclapply(1:nrow(X), mc.cores = parallel, function (i) {
          cutFUN(X = X, i = i, mar = mar, labels = labels, dest.path = dest.path)
        }) else
          out <- parallel::mclapply(1:nrow(X), mc.cores = parallel, function (i) {
            cutFUN(X = X, i = i, mar = mar, labels = labels, dest.path = dest.path)
            
          })
    }
    if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
    {
      cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))
      
      doParallel::registerDoParallel(cl)
      
      out <- foreach::foreach(i = 1:nrow(X)) %dopar% {
        cutFUN(X = X, i = i, mar = mar, labels = labels, dest.path = dest.path)
      }
      
      parallel::stopCluster(cl)
      
    }
  }
  else {
    if(pb)
      out <- pbapply::pblapply(1:nrow(X), function(i) cutFUN(X = X, i = i, mar = mar, labels = labels, dest.path = dest.path)) else 
        out <- lapply(1:nrow(X), function(i) cutFUN(X = X, i = i, mar = mar, labels = labels, dest.path = dest.path))
  }
  
  if(!is.null(path)) setwd(wd)
}
