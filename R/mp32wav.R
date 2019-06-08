#' Convert .mp3 files to .wav
#' 
#' \code{mp32wav} converts several .mp3 files in working directory to .wav format
#' @usage mp32wav(samp.rate = NULL, parallel = 1, path = NULL, 
#' to = NULL, dest.path = NULL, bit.depth = 16, pb = TRUE, overwrite = FALSE)  
#' @param samp.rate Sampling rate in kHz at which the .wav files should be written. If not provided the sample rate of the original .mp3 file is used. Downsampling is done using the
#' \code{\link[bioacoustics]{resample}} function from the \href{https://cran.r-project.org/package=bioacoustics}{bioacoustics package} (which should be installed), which seems to generate aliasing. This can be avoided by downsampling after .mp3's have been converted using the \code{\link{fix_wavs}} function (which uses \href{http://sox.sourceforge.net/sox.html}{SOX} instead). Default is \code{NULL} (e.g. keep original sampling rate).
#' @param parallel Numeric. Controls whether parallel computing is applied. 
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the .mp3 files are located.   
#' If \code{NULL} (default) then the current working directory is used. 
#' @param to Character string containing the directory path where the .wav files will be saved. 
#' If \code{NULL} (default) then the current working directory is used. Will be deprecated in future versions.
#' @param dest.path Character string containing the directory path where the .wav files will be saved. 
#' If \code{NULL} (default) then the current working directory is used. Same as 'to' (will replace it in future versions).
#' @param bit.depth Character string containing the units to be used for amplitude normalization. Check 
#' \code{\link[tuneR]{normalize}} for details. Default is 16.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param overwrite Logical. Control whether a .wav sound file that is already in the working directory should be 
#' overwritten.
#' @return .wav files saved in the working directory with same name as original mp3 files.
#' @export
#' @details The function will convert all mp3 files in  working directory or 'path' supplied to wav format. \href{https://cran.r-project.org/package=bioacoustics}{bioacoustics package} must be installed when changing sampling rates (i.e. if 'samp.rate' is supplied). Note that sound files are normalized using \code{\link[tuneR]{normalize}} so they can be written by \code{\link[tuneR]{writeWave}}.
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

mp32wav <- function(samp.rate = NULL, parallel = 1, path = NULL, 
                    to = NULL, dest.path = NULL, bit.depth = 16, pb = TRUE, overwrite = FALSE) {
  
  # error message if bioacoustics is not installed
  if (!requireNamespace("bioacoustics", quietly = TRUE) & !is.null(samp.rate))
    stop("must install 'bioacoustics' to use mp32wav() for changing sampling rate")
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(mp32wav)
  
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
  
  if (!is.null(path))
  {if (!dir.exists(path)) stop("'path' provided does not exist")} else
    path <- wd #set working directory

  setwd(path)
  
  if (!is.null(to))
  {if (!dir.exists(to)) stop("'path' provided does not exist")} else
    to <- wd #set working directory
  
  #normalize
  if (length(bit.depth) > 1) stop("'bit.depth' should have a single value")
    bit.depth <- as.character(bit.depth)
  
  if (!bit.depth %in% c("1", "8", "16", "24", "32", "64", "0")) stop('only this "bit.depth" values allowed c("1", "8", "16", "24", "32", "64", "0") \n see ?tuneR::normalize')
    
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
          
  files <- list.files(path=getwd(), pattern = ".mp3$", ignore.case = TRUE) #list .mp3 files in working directory
  if (length(files) == 0) stop("no 'mp3' files in working directory")
  
  #exclude the ones that already have a .wav version
  if (!overwrite) {
    wavs <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  files <- files[!substr(files, 0, nchar(files) - 4) %in% substr(wavs, 0, nchar(wavs) - 4)]
  if (length(files) == 0) stop("all 'mp3' files have been converted")
  }
  
 # function to convert single mp3  
 mp3_conv_FUN <- function(x, bit.depth) {
   
   # read mp3
   wv <- try(tuneR::readMP3(filename =  x), silent = TRUE)
   
   # downsample and filter if samp.rate different than mp3
   if(class(wv) == "Wave" & !is.null(samp.rate))
   {
     if (wv@samp.rate != samp.rate * 1000) {
      
      # filter first to avoid aliasing 
       if (wv@samp.rate > samp.rate * 1000)
      wv <- seewave::fir(wave = wv , f = wv@samp.rate, from = 0, to = samp.rate * 1000 / 2, bandpass = TRUE, output = "Wave")

      #downsample
      wv <- bioacoustics::resample(wave = wv, to = samp.rate * 1000)
      }

    # normalize 
       wv <- tuneR::normalize(object = wv, unit = as.character(bit.depth))
     
     }
   
   wv <- try(tuneR::writeWave(extensible = FALSE, object = wv, filename = file.path(path, paste0(substr(x, 0, nchar(x) - 4), ".wav"))), silent = TRUE)
   
   return(NULL)
   }

 # set pb options 
 pbapply::pboptions(type = ifelse(pb, "timer", "none"))
 
 # set clusters for windows OS
 if (Sys.info()[1] == "Windows" & parallel > 1)
   cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
 
 # run loop apply function
 out <- pbapply::pbsapply(X = files, cl = cl, FUN = function(i) 
 { 
   suppressWarnings(mp3_conv_FUN(x = i, bit.depth))
 })
 
 if(any(sapply(out, function(x) class(x) == "try-error"))) {
   
   cat(paste("the following file(s) could not be converted:\n", paste(files[sapply(out, function(x) class(x) == "try-error")], collapse = ", ")))
   
   cat(paste("\nErrors found: \n", paste(unique(out[sapply(out, function(x) class(x) == "try-error")]), collapse = ", ")))
   }
 
}
