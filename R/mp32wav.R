#' Convert .mp3 files to .wav
#'
#' \code{mp32wav} converts several .mp3 files in working directory to .wav format
#' @usage mp32wav(samp.rate = NULL, parallel = 1, path = NULL,
#'  dest.path = NULL, bit.depth = 16, pb = TRUE, overwrite = FALSE)
#' @param samp.rate Sampling rate in kHz at which the .wav files should be written. If not provided the sample rate of the original .mp3 file is used. THIS FEATURE IS CURRENTLY NOT AVAILABLE. However, downsampling can be done after .mp3's have been converted using the \code{\link{fix_wavs}} function (which uses \href{https://sox.sourceforge.net/sox.html}{SOX} instead). Default is \code{NULL} (e.g. keep original sampling rate).
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the .mp3 files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param dest.path Character string containing the directory path where the .wav files will be saved.
#' If \code{NULL} (default) then the folder containing the sound files will be used.
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
#' # download mp3 files from xeno-canto
#' query_xc(qword = "Phaethornis aethopygus", download = TRUE, path = tempdir())
#'
#' # Convert all files to .wav format
#' mp32wav(path = tempdir(), dest.path = tempdir())
#'
#' # check this folder!!
#' tempdir()
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @details convert all .mp3 files in working directory to .wav format. Function used internally to read .mp3 files (\code{\link[tuneR]{readMP3}}) sometimes crashes.
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) and Grace Smith Vidaurre
# last modification on mar-13-2018 (MAS)

mp32wav <- function(samp.rate = NULL, parallel = 1, path = NULL,
                    dest.path = NULL, bit.depth = 16, pb = TRUE, overwrite = FALSE) {
  # error message if bioacoustics is not installed
  if (!requireNamespace("bioacoustics", quietly = TRUE) & !is.null(samp.rate)) {
    stop2("must install 'bioacoustics' to use mp32wav() for changing sampling rate")
  }

  # error message if sox is not installed
  sox_installed <-
    system(paste("which sox", sep = " "),
      ignore.stderr = TRUE,
      intern = FALSE,
      ignore.stdout = TRUE
    )

  if (sox_installed == 1) {
    stop2("Sox is not installed or is not available in path")
  }

  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(mp32wav)

  # get warbleR options
  opt.argms <- if (!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0

  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]

  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]

  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]

  # set options left
  if (length(opt.argms) > 0) {
    for (q in 1:length(opt.argms)) {
      assign(names(opt.argms)[q], opt.argms[[q]])
    }
  }

  # check path to working directory
  if (is.null(path)) {
    path <- getwd()
  } else if (!dir.exists(path)) {
    stop2("'path' provided does not exist")
  } else {
    path <- normalizePath(path)
  }

  # check dest.path to working directory
  if (is.null(dest.path)) {
    dest.path <- getwd()
  } else if (!dir.exists(dest.path)) {
    stop2("'dest.path' provided does not exist")
  } else {
    dest.path <- normalizePath(dest.path)
  }

  # normalize
  if (length(bit.depth) > 1) stop2("'bit.depth' should have a single value")
  bit.depth <- as.character(bit.depth)

  if (!bit.depth %in% c("1", "8", "16", "24", "32", "64", "0")) stop2('only this "bit.depth" values allowed c("1", "8", "16", "24", "32", "64", "0") \n see ?tuneR::normalize')

  # if parallel is not numeric
  if (!is.numeric(parallel)) stop2("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop2("'parallel' should be a positive integer")

  files <- list.files(path = path, pattern = ".mp3$", ignore.case = TRUE) # list .mp3 files in working directory
  if (length(files) == 0) stop2("no 'mp3' files in working directory")

  # exclude the ones that already have a .wav version
  if (!overwrite) {
    wavs <- list.files(path = dest.path, pattern = "\\.wav$", ignore.case = TRUE)
    files <- files[!substr(files, 0, nchar(files) - 4) %in% substr(wavs, 0, nchar(wavs) - 4)]
    if (length(files) == 0) stop2("all 'mp3' files have been converted")
  }

  # function to convert single mp3
  mp3_conv_FUN <- function(x, bit.depth) {
    print(x)
    # read mp3
    wv <- try(tuneR::readMP3(filename = file.path(path, x)), silent = TRUE)

    # downsample and filter if samp.rate different than mp3
    if (is(wv, "Wave") & !is.null(samp.rate)) {
      if (wv@samp.rate != samp.rate * 1000) {
        message2("'samp.rate' modification currently not available")

        # # filter first to avoid aliasing
        #  if (wv@samp.rate > samp.rate * 1000)
        # wv <- seewave::fir(wave = wv , f = wv@samp.rate, from = 0, to = samp.rate * 1000 / 2, bandpass = TRUE, output = "Wave")
        #
        # #downsample
        # wv <- warbleR::resample(wave = wv, to = samp.rate * 1000)
      }

      # normalize
      wv <- tuneR::normalize(object = wv, unit = as.character(bit.depth))
    }

    wv <- try(tuneR::writeWave(extensible = FALSE, object = wv, filename = file.path(dest.path, paste0(substr(x, 0, nchar(x) - 4), ".wav"))), silent = TRUE)

    return(NULL)
  }

  ######

  fix_sox_FUN <- function(x) {
    px <- normalizePath(file.path(path, x))

    # name  and path of original file
    cll <- paste0("sox '", px, "' -t wavpcm")

    if (!is.null(bit.depth)) {
      cll <- paste(cll, paste("-b", bit.depth))
    }

    cll <- paste0(cll, " '", file.path(dest.path, paste0(substr(x, 0, nchar(x) - 4), ".wav")), "'")

    if (!is.null(samp.rate)) {
      cll <- paste(cll, "rate", samp.rate * 1000)
    }

    if (!is.null(mono)) {
      cll <- paste(cll, "remix 1")
    }

    if (!is.null(bit.depth)) {
      cll <- paste(cll, "dither -s")
    }

    if (Sys.info()[1] == "Windows") {
      cll <- gsub("'", "\"", cll)
    }

    out <- system(cll, ignore.stdout = FALSE, intern = TRUE)
  }


  ######

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  # run loop apply function
  out_l <- pblapply_wrblr_int(pbar = pb, X = files, cl = cl, FUN = function(i) {
    # suppressWarnings(mp3_conv_FUN(x = i, bit.depth))
    fix_sox_FUN(i)
  })

  # make it a vector
  out <- unlist(out_l)

  if (any(sapply(out, function(x) is(x, "try-error")))) {
    message2(paste("the following file(s) could not be converted:\n", paste(files[sapply(out, function(x) is(x, "try-error"))], collapse = ", ")))

    message2(paste("\nErrors found: \n", paste(unique(out[sapply(out, function(x) is(x, "try-error"))]), collapse = ", ")))
  }
}


##############################################################################################################
#' alternative name for \code{\link{mp32wav}}
#'
#' @keywords internal
#' @details see \code{\link{mp32wav}} for documentation. \code{\link{mp32wav}} will be deprecated in future versions.
#' @export

mp3_2_wav <- mp32wav
