#' Fix .wav files to allow importing them into R
#' 
#' \code{fixwavs} fixes sound files in .wav format so they can be imported into R.
#' @usage fixwavs(checksels = NULL, files = NULL, samp.rate = NULL, bit.depth = NULL,
#'  path = NULL, mono = FALSE, sox = FALSE)
#' @param checksels Data frame with results from \code{\link{checksels}}. Default is \code{NULL}. If  both 'checksels' and 'files'  are \code{NULL}
#' then all files in 'path' are converted. 
#' @param files Character vector with the names of the wav files to fix. Default is \code{NULL}. If  both 'checksels' and 'files'  are \code{NULL}
#' then all files in 'path' are converted. 
#' @param samp.rate Numeric vector of length 1 with the sampling rate (in kHz) for output files. Default is \code{NULL}. 
#' (remain unchanged).
#' @param bit.depth Numeric vector of length 1 with the dynamic interval (i.e. bit depth) for output files.
#' Default is \code{NULL} (remain unchanged).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param mono Logical indicating if stereo (2 channel) files should be converted to mono (1 channel). Default is \code{NULL} (remain unchanged).
#' @param sox Logical indicating if \href{http://sox.sourceforge.net/sox.html}{SOX} should be used for resampling. If \code{TRUE} SOX must be installed. Default is \code{FALSE}. 
#' @return  A folder inside the working directory (or path provided) all 'converted_sound_files', containing 
#' sound files in a format that can be imported in R. 
#' @export
#' @name fixwavs
#' @details This function aims to simplify the process of converting sound files that cannot be imported into R and/or homogenizing sound files. Problematic files can be determined using \code{\link{check_wavs}} or \code{\link{check_sels}}. The  
#' \code{\link{check_sels}} output can be directly input using the argument 'checksels'. Alternatively a vector of file 
#' names to be "fixed" can be provided (argument 'files'). If neither of those 2 are provided the function will convert
#' all sound files in the working directory to the specified sample rate/bit depth. Files are saved in a new directory
#' ('converted_sound_files'). Internally the function calls \href{http://sox.sourceforge.net/sox.html}{SOX}. \href{http://sox.sourceforge.net/sox.html}{SOX} must be installed to be able to run this function. If  both 'checksels' and 'files'  are \code{NULL}
#' then all files in 'path' are converted. 
#'   
#' @examples
#' \dontrun{
#' # Save to temporary working directory
#' 
#' 
# data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
# writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
# writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
# writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
# writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
# 
# fixwavs(files = lbh_selec_table$sound.files, path = tempdir())
#' 
#' #check this folder
#' getwd()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
# last modification on oct-22-2018 (MAS)

fixwavs <- function(checksels = NULL, files = NULL, samp.rate = NULL, bit.depth = NULL, path = NULL, mono = FALSE, sox = FALSE)
{

  # error message if bioacoustics is not installed
  if (!requireNamespace("bioacoustics", quietly = TRUE) & !is.null(samp.rate))
    stop("must install 'bioacoustics' to use mp32wav() for changing sampling rate")
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(fixwavs)
  
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
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) stop("'path' provided does not exist") 
  
  #  If  both 'checksels' and 'files'  are NULL
  if (is.null(checksels) & is.null(files)) files <- list.files(path = path, pattern = ".wav$", ignore.case = TRUE)  

    if (!is.null(checksels))
  {fls <-unique(checksels$sound.files[checksels$check.res == "Sound file can't be read" | checksels$check.res == "file header corrupted"])
  if (length(fls) == 0) stop("All files were OK according tochecksels")

    #if X is not a data frame
  if (!class(checksels) == "data.frame") stop("checksels is not a data frame")
  
  if (!all(c("sound.files", "check.res") %in% colnames(checksels))) 
    stop(paste(paste(c("sound.files", "check.res")[!(c("sound.files", "check.res") %in% colnames(checksels))], collapse=", "), "column(s) not found in data frame (does not seem to be the output of checksels)"))
  } else fls <- unique(files)

  
  if (length(list.files(pattern = "\\.wav$", ignore.case = TRUE)) == 0) if (is.null(path)) stop("No .wav files in working directory") else stop("No .wav files in 'path' provided") 
  
  if (!is.null(samp.rate)) 
    if (!is.vector(samp.rate)) stop("'samp.rate' must be a numeric vector of length 1") else 
      if (!length(samp.rate) == 1) stop("'samp.rate' must be a numeric vector of length 1")
  
  if (!is.null(bit.depth)) 
    if (!is.vector(bit.depth)) stop("'bit.depth' must be a numeric vector of length 1") else 
      if (!length(bit.depth) == 1) stop("'bit.depth' must be a numeric vector of length 1")
  
    
if (!is.null(samp.rate) & is.null(bit.depth)) bit.depth <- 16

dir.create(file.path(path, "converted_sound_files"), showWarnings = FALSE)
  

fix_bio_FUN <- function(x) {

    # read waves
    wv <- try(warbleR::read_wave(X = x, path = path), silent = TRUE)

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

    wv <- try(tuneR::writeWave(extensible = FALSE, object = wv, filename = file.path(path, "converted_sound_files", paste0(substr(x, 0, nchar(x) - 4), ".wav"))), silent = TRUE)
    
    return(NULL)
}


fix_sox_FUN <- function(x)
{
  
  #name  and path of original file
  cll <- paste0("sox '", x, "' -t wavpcm")
  
  if (!is.null(bit.depth))
    cll <- paste(cll, paste("-b", bit.depth))
  
  cll <- paste0(cll, " converted_sound_files/'", x, "'")
  
  if (!is.null(samp.rate))
    cll <- paste(cll, "rate", samp.rate * 1000)
  
  if (!is.null(mono))
    cll <- paste(cll, "remix 1")
  
  if (!is.null(bit.depth))
    cll <- paste(cll, "dither -s")
  
  if (Sys.info()[1] == "Windows")
    cll <- gsub("'", "\"", cll)
  
  out <- system(cll, ignore.stdout = FALSE, intern = TRUE) 
}

fix_FUN <- if (sox)  fix_sox_FUN else fix_bio_FUN 

  out <- pbapply::pblapply(fls, fix_FUN)
  }


##############################################################################################################
#' alternative name for \code{\link{fixwavs}}
#'
#' @keywords internal
#' @details see \code{\link{fixwavs}} for documentation. \code{\link{fixwavs}} will be deprecated in future versions.
#' @export

fix_wavs <- fixwavs
