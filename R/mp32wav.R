#' Convert .mp3 files to .wav
#' 
#' \code{mp32wav} converts several .mp3 files in working directory to .wav format
#' @usage mp32wav(samp.rate = 44.1, parallel = 1, from = NULL, path = NULL, 
#' to = NULL, dest.path = NULL, normalize = NULL, pb = TRUE, overwrite = FALSE)  
#' @param samp.rate Sampling rate at which the .wav files should be written. The maximum permitted is 44.1 kHz (default). Units should be kHz.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param from Character string containing the directory path where the .mp3 files are located. Will be deprecated in future versions.
#' @param path Character string containing the directory path where the .mp3 files are located.   
#' If \code{NULL} (default) then the current working directory is used. Same as
#' 'from' (will replace it in future versions).
#' @param to Character string containing the directory path where the .wav files will be saved. 
#' If \code{NULL} (default) then the current working directory is used. Will be deprecated in future versions.
#' @param dest.path Character string containing the directory path where the .wav files will be saved. 
#' If \code{NULL} (default) then the current working directory is used. Same as 'to' (will replace it in future versions).
#' @param normalize Character string containing the units to be used for amplitude normalization. Check 
#' (\code{\link[tuneR]{normalize}}) for details. If NULL (default) no normalization is carried out.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param overwrite Logical. Control whether a .wav sound file that is already in the working directory should be 
#' overwritten.
#' @return .wav files saved in the working directory with same name as original mp3 files.
#' @export
#' @name mp32wav
#' @examples
#' \dontrun{
#' # First set temporary folder
#' # setwd(tempdir())
#'  
#' #Then download mp3 files from xeno-canto
#' querxc(qword = "Phaethornis aethopygus", download = TRUE)
#' 
#' # Convert all files to .wav format
#' mp32wav()
#' 
#' #check this folder!!
#' getwd()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @details convert all .mp3 files in working directory to .wav format. Function used internally to read .mp3 files (\code{\link[tuneR]{readMP3}}) sometimes crashes.
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) and Grace Smith Vidaurre
#last modification on mar-13-2018 (MAS)

mp32wav <- function(samp.rate = 44.1, parallel = 1, from = NULL, path = NULL, 
                    to = NULL, dest.path = NULL, normalize = NULL, pb = TRUE, overwrite = FALSE) {
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  if (!is.null(path)) from <- path
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(mp32wav)
  
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
  
  if (!is.null(from))
  {if (!dir.exists(from)) stop("'path' provided does not exist")} else
    from <- wd #set working directory

  setwd(from)
  
  if (!is.null(to))
  {if (!dir.exists(to)) stop("'path' provided does not exist")} else
    to <- wd #set working directory
  
  #normalize
  if (!is.null(normalize))
  {if (length(normalize) >1) stop("'normalize' should have a single value")
    normalize <- as.character(normalize)
    if (!normalize %in% c("1", "8", "16", "24", "32", "64", "0")) stop("'normalize' value not allowed (check the documentation from the normalize function in the tuneR package")
    }
  
  #fix sample rate
  if (samp.rate > 44.1) samp.rate <- 44.1
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  #if parallel and pb in windows
  if (parallel > 1 &  pb & Sys.info()[1] == "Windows") {
    cat("parallel with progress bar is currently not available for windows OS")
    cat("running parallel without progress bar")
    pb <- FALSE
  } 
          
  files <- list.files(path=getwd(), pattern = ".mp3$", ignore.case = TRUE) #list .mp3 files in working directory
  if (length(files) == 0) stop("no 'mp3' files in working directory")
  
  #exclude the ones that already have a .wav version
 
  if (!overwrite) 
    {wavs <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  files <- files[!substr(files, 0, nchar(files) - 4) %in% substr(wavs, 0, nchar(wavs) - 4)]
  if (length(files) == 0) stop("all 'mp3' files have been converted")
  }
  
 # function to convert single mp3  
 mp3_conv_FUN <- function(x, normalize) {
   
   wv <- try(tuneR::readMP3(filename =  x), silent = TRUE)
   
   if(class(wv) == "Wave")
   {
     if (wv@samp.rate != samp.rate * 1000)
   wv <- tuneR::downsample(object = wv, samp.rate = samp.rate * 1000)
   
   if (!is.null(normalize))
   wv <- tuneR::normalize(object = wv, unit = normalize)
   
   wv <- try(tuneR::writeWave(extensible = FALSE, object = wv, filename = file.path(from, paste0(substr(x, 0, nchar(x) - 4), ".wav"))), silent = TRUE)
   }
   return(wv)
   }

 # set pb options 
 pbapply::pboptions(type = ifelse(pb, "timer", "none"))
 
 # set clusters for windows OS
 if (Sys.info()[1] == "Windows" & parallel > 1)
   cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
 
 # run loop apply function
 out <- pbapply::pbsapply(X = files, cl = cl, FUN = function(i) 
 { 
   suppressWarnings(mp3_conv_FUN(x = i, normalize))
 })
 
 if(any(sapply(out, function(x) class(x) == "try-error"))) {
   
   cat(paste("the following file(s) could not be converted:\n", paste(files[sapply(out, function(x) class(x) == "try-error")], collapse = ", ")))
   
   cat(paste("\nErrors found: \n", paste(unique(out[sapply(out, function(x) class(x) == "try-error")]), collapse = ", ")))
   }
 
}
