#' Check selection data frames
#' 
#' \code{check_sels} checks whether selections can be read by subsequent functions.
#' @usage check_sels(X, parallel = 1, path = NULL, check.header = FALSE, pb = TRUE,
#' wav.size = FALSE, verbose = TRUE, fix.selec = FALSE)
#' @param X 'selection_table' object or data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. Alternatively, a 'selection_table' class object can be input to double check selections. The output of \code{\link{auto_detec}} can 
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
#' @param verbose Logical to control whether the summary messages are printed to the console. Defaut is \code{TRUE}.
#' @param fix.selec Logical to control if labels in 'selec' column should be fixed. This column should not be duplicated within a sound file. If that happens and \code{fix.selec = TRUE} duplicated labels will be changed. Default is \code{FALSE}.  
#' @return A data frame including the columns in the input data frame (X) and the following additional columns:
#' \itemize{
#'    \item \code{check.res}: diagnose for each selection 
#'    \item \code{duration}: duration of selection in seconds
#'    \item \code{min.n.samples} number of samples in a selection. Note the number of samples available
#' in a selection limits the minimum window length (wl argument in other functions) that can be used in batch analyses.
#'    \item \code{sample.rate}: sampling rate in kHz
#'    \item \code{channels}: number of channels
#'    \item \code{bits}: bit depth
#'    \item \code{sound.file.samples}: number of samples in the sound file
#'    }
#' @details This function checks the information in a selection data frame or selection table (i.e. data frame with annotations on sound files) 
#' to avoid problems in any warbleR analysis downstream. It specifically checks if:
#' \itemize{
#'    \item 'X' is an object of class 'data.frame' or 'selection_table' (see \code{\link{selection_table}}) and contains 
#'    the required columns to be used on any warbleR function ('sound.files', 'selec', 'start', 'end', if not returns an error) 
#'    \item  'sound.files' in 'X' correspond to sound files in the working directory or in the provided 'path'
#'     (if no file is found returns an error, if some files are not found returns error info in the ouput data frame)
#'    \item time ('start', 'end') and frequency ('bottom.freq', 'top.freq', if provided) limit parameters are numeric and 
#'    don't contain NAs (if not returns an error)
#'    \item there are no duplicated selection labels ('selec') within a sound file (if not returns an error)
#'    \item sound files can be read (error info in the ouput data frame)
#'    \item the start and end time of the selections are found within the duration of the sound files (error info in the ouput data frame)
#'    \item sound files can be read (error info in the ouput data frame)
#'    \item sound files header is not corrupted (only if \code{header = TRUE}, error info in the ouput data frame)    
#'    \item selection time position (start and end) doesn't exceeds sound file length (error info in the ouput data frame)
#'    \item 'top.freq' is lower than half the sample rate (nyquist frequency, error info in the ouput data frame)
#'    \item negative values aren't found in time or frequency limit parameters (error info in the ouput data frame)
#'    \item 'start' higher than 'end' or 'bottom.freq' higher than 'top.freq' (error info in the ouput data frame)
#'    \item 'channel' value is not higher than number of channels in sound files (error info in the ouput data frame)
#' } 
#' The function returns a data frame that includes the information in 'X' plus additional columns about the format of sound
#' files (see 'Value') as well as the result of the checks ('check.res' column, value is 'OK' if everything is fine).
#' Sound files should be in the working directory (or the directory provided in 'path'). Corrupt files can be fixed using 
#' \code{\link{fix_wavs}}.
#' @seealso \code{\link{check_wavs}}
#' @export
#' @name check_sels
#' @export
#' @examples{
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' 
#' check_sels(X = lbh_selec_table, path = tempdir())
#' }
#' @references {Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on jul-5-2016 (MAS)

check_sels <- function(X = NULL, parallel =  1, path = NULL, check.header = FALSE, 
                      pb = TRUE, wav.size = FALSE, verbose = TRUE, fix.selec = FALSE){
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(check_sels)
  
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
    if (!dir.exists(path)) 
      stop("'path' provided does not exist") else
        path <- normalizePath(path)
  
  #if X is not a data frame
  if (all(!any(is.data.frame(X), is_selection_table(X)))) stop("X is not of a class 'data.frame' or 'selection_table'")
  
  if (is_extended_selection_table(X)) stop("check_sels does not work on objects of class 'extended_selection_table'")
  
  if (!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  #if end or start are not numeric stop
  if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop("'start' and 'end' must be numeric")
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  # check for duplicates and if fix.selec = TRUE
  if (any(duplicated(paste(X$sound.files, X$selec)))) 
    if (fix.selec) {
      
      X$selec <- do.call(c, lapply(unique(X$sound.files), function(x) seq_len(sum(X$sound.files == x))))
      
    } else
    stop("Duplicated selection labels ('selec' column) for one or more sound files (can be fixed by setting fix.selec = TRUE)")
  
  #check additional columns
  if (!"channel" %in% colnames(X)) 
  {
    X$channel <- 1
  } else {
    if (!is.numeric(X$channel)) stop("'channel' must be numeric")
    if (any(is.na(X$channel))) {cat("NAs in 'channel', assumed to be channel 1 \n")
      X$channel[is.na(X$channel)] <- 1   
    }}
  
  #check if files are in working directory
  files <- file.exists(file.path(path, unique(X$sound.files)))
  if (all(!files)) 
    stop("no sound files found")
  
  # update to new frequency range column names
  if (any(grepl("low.freq|high.freq", names(X)))) {
    names(X)[names(X) == "low.freq"] <- "bottom.freq"
    names(X)[names(X) == "high.freq"] <- "top.freq"
    cat("'low.freq' and 'high.freq' renamed as 'bottom.freq' and 'top.freq' \n")
    }
  
  # check if freq lim are numeric
  if (any(names(X) == "bottom.freq"))
    if (!is(X$bottom.freq, "numeric")) stop("'bottom.freq' is not numeric")
  
  if (any(names(X) == "top.freq"))
    if (!is(X$top.freq, "numeric")) stop("'top.freq' is not numeric")
  
  # check if NAs in freq limits
  if (any(names(X) %in% c("bottom.freq", "top.freq")))
  if (any(is.na(c(X$bottom.freq, X$top.freq)))) stop("NAs found in 'top.freq' and/or 'bottom.freq' \n")  
  
  # function to run over each sound file
  csFUN <- function(x, X, pth){
    Y <- as.data.frame(X[X$sound.files == x, , drop = FALSE])
    
    if (file.exists(file.path(pth, x))){
      rec <- try(suppressWarnings(read_sound_file(X = x, path = pth, header = TRUE)), silent = TRUE)
      
      # if it was read
      if (!is(rec, "try-error"))
      {
        if (check.header) # look for mismatchs between file header & file content  
        {
          recfull <- try(suppressWarnings(read_sound_file(X = x, path = pth, header = FALSE)), silent = TRUE)
          if (any(methods::slotNames(recfull) == "stereo")) 
          {
            if (rec$channels == 2) channel.check <- ifelse(recfull@stereo, FALSE, TRUE) else
              channel.check <- ifelse(!recfull@stereo, FALSE, TRUE)
            
            samples.check <- ifelse(rec$samples == length(recfull@left), FALSE, TRUE) 
          } else {
            channel.check <- FALSE
            samples.check <- ifelse(rec$samples == length(recfull@.Data), FALSE, TRUE)
          }
          
          if (any(rec$sample.rate != recfull@samp.rate, rec$bits != recfull@bit, channel.check, samples.check))
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
            
            if (any(Y$end > maxdur))  Y$check.res[Y$end > maxdur] <- "exceeds sound file length"
            Y$duration <- Y$end - Y$start
            Y$min.n.samples <- floor(Y$duration * rec$sample.rate)
            Y$sample.rate <- rec$sample.rate / 1000
            Y$channels <- rec$channels
            Y$bits <- rec$bits
            Y$sound.file.samples <- rec$samples
          }
          
        } else
        { 
          maxdur <- rec$samples/rec$sample.rate  
          Y$check.res <- "OK"
        
        if (any(Y$end > maxdur))  Y$check.res[Y$end > maxdur] <- "exceeds sound file length"
        Y$duration <- Y$end - Y$start
        Y$min.n.samples <- floor(Y$duration * rec$sample.rate)
        Y$sample.rate <- rec$sample.rate / 1000
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
        Y$sound.file.samples <- NA
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
  
  
  
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  out <- pblapply_wrblr_int(pbar = pb, X = unique(X$sound.files), cl = cl, FUN = function(x) 
  { 
    csFUN(x, X, pth = path)
  }) 
  
  res <- do.call(rbind, out)
  res <- res[match(paste(X$sound.files, X$selec), paste(res$sound.files, res$selec)), ]
  
  if ("top.freq" %in% names(res))
  {   
    # nyquist frequency
    try(res$check.res <- ifelse((res$sample.rate/2) - res$top.freq < 0 & !is.na(res$sample.rate), gsub("OK\\|", "", paste(res$check.res, "'Top.freq' higher than half the sample rate", sep = "|")), res$check.res), silent = TRUE)
    
    # if bottom.freq is negative
    res$check.res <- ifelse(res$bottom.freq < 0, gsub("OK\\|", "", paste(res$check.res, "Negative values in 'bottom.freq'", sep = "|")), res$check.res)
    
    # if fre range is equal or lower than 0
    res$check.res <- ifelse(res$top.freq - res$bottom.freq <= 0, gsub("OK\\|", "", paste(res$check.res, "'bottom.freq' is equal or higher than the 'top.freq'", sep = "|")), res$check.res)
    } 
  
  # if start higher or equal than end
  res$check.res <- ifelse(res$end - res$start <= 0, gsub("OK\\|", "", paste(res$check.res, "'start' is equal or higher than the 'end'", sep = "|")), res$check.res)
  
  # if start is negative
  res$check.res <- ifelse(res$start < 0, gsub("OK\\|", "", paste(res$check.res, "Negative values in 'start'", sep = "|")), res$check.res)
  
  # if channel number is equal or smaller than the number of channels in the wav file
  if (any(res$channel[!is.na(res$duration)] > res$channels[!is.na(res$duration)])) {cat("\n some selections listed as having more than 1 channel found in sound files with only 1 channel; channel field relabeled as '1' \n") 
    res$channel[!is.na(res$duration)][any(res$channel[!is.na(res$duration)] > res$channels[!is.na(res$duration)])] <- 1
  }
  
  if (wav.size) res$wav.size <- round(res$bits  * res$channel * res$sample.rate * res$duration / 4) / 1024
  
  if(verbose){
  # inform result
  if (all(res$check.res == "OK")) 
  {
    if (any(res$min.n.samples < 20))
      cat("all selections are OK but some have very few samples (less than 20, potentially problematic for some analyses) \nCheck 'min.n.samples' column") else
        cat("all selections are OK \n")   
  }
    else cat(paste(sum(res$check.res != "OK"), "selection(s) are not OK \n"))
}
  # return data frame
  res <- res
}

##############################################################################################################
#' alternative name for \code{\link{check_sels}}
#'
#' @keywords internal
#' @details see \code{\link{check_sels}} for documentation. \code{\link{checksels}} will be deprecated in future versions.
#' @export

checksels <- check_sels


##############################################################################################################
#' alternative name for \code{\link{check_sels}}
#'
#' @keywords internal
#' @details see \code{\link{check_sels}} for documentation. \code{\link{check_sels}} will be deprecated in future versions.
#' @export

check_sels <- check_sels
