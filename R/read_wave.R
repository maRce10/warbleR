#' A wrapper for tuneR's readWave that read sound files listed within selection tables
#'
#' \code{read_wave} is a wrapper for tuneR's \code{\link[tuneR]{readWave}} function that read sound files listed in data frames and selection tables
#' @usage read_wave(X, index, from = X$start[index], to = X$end[index], channel = NULL,
#' header = FALSE, path = NULL)
#' @param X 'data.frame', 'selection_table' or 'extended_selection_table' containing columns for sound file name (sound.files),
#' selection number (selec), and start and end time of signals (start and end). Alternatively, the name of a '.wav' file or URL address to a '.wav' or '.mp3' file can be provided. The file name can contain the directory path.
#' 'top.freq' and 'bottom.freq' columns are optional. Default is \code{NULL}.
#' @param index Index of the selection in 'X' that will be read. Ignored if 'X' is \code{NULL}.
#' @param from Where to start reading, in seconds. Default is \code{X$start[index]}.
#' @param to Where to stop reading, in seconds. Default is \code{X$end[index]}.  \code{Inf} can be used for reading the entire sound file (when 'X' is a sound file name),
#' @param channel Channel to be read from sound file (1 = left, 2 = right, or higher number for multichannel waves). If
#' \code{NULL} (default) or higher than the number of channels in a wave then the first channel is used. Only applies to '.wav' files in local directories.
#' @param header If \code{TRUE}, only the header information of the Wave object is returned, otherwise (the default) the whole Wave object.
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used. If 'X' refers to a sound file including its directory 'path' is ignored.
#' @return An object of class "Wave".
#' @export
#' @name read_wave
#' @details The function is a wrapper for \code{\link[tuneR]{readWave}} that read sound files listed within selection tables. It
#' is also used internally by warbleR functions to read wave objects from extended selection tables (see \code{\link{selection_table}} for details).
#' @examples
#' {
#'   # write wave files with lower case file extension
#'   data(list = c("Phae.long1"))
#'   writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#'
#'   # read from selection table
#'   read_wave(X = lbh_selec_table, index = 1, path = tempdir())
#'
#'   # from extended selection table
#'   library(NatureSounds)
#'   read_wave(X = lbh.est, index = 1)
#'
#'   # read WAV
#'   filepath <- system.file("extdata", "recording.wav", package = "bioacoustics")
#'   read_wave(filepath)
#'
#'   # read MP3
#'   filepath <- system.file("extdata", "recording.mp3", package = "bioacoustics")
#'   read_wave(filepath)
#'
#'   # URL file
#'   read_wave(X = "https://www.xeno-canto.org/513948/download")
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on may-7-2018 (MAS)

read_wave <- function(X, index, from = X$start[index], to = X$end[index], channel = NULL, header = FALSE, path = NULL) {
  # if is extended then index must be provided
  if (is_extended_selection_table(X) & missing(index)) stop('"index" needed when an extended selection table is provided')

  # if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X), is.character(X), is.factor(X))) stop("X is not of a class 'data.frame', 'selection_table', 'extended_selection_table' or a sound file")

  # if is not a data frame
  if (is.character(X) | is.factor(X)) {
    # if URL
    if (grepl("^www.|^http:|^https:", X)) {
      # get  temporary file name
      temp.file <- tempfile()

      # download it
      download.file(url = X, destfile = temp.file, quiet = TRUE, mode = "wb", cacheOK = TRUE, extra = getOption("download.file.extra"))

      if (!file.exists(temp.file)) stop("File couldn't be downloaded")

      # overwrite X
      X <- temp.file

      # extension is unknown
      extsn <- "unk"
    } else {
      # get extension
      pos <- regexpr("\\.([[:alnum:]]+)$", X)
      extsn <- tolower(ifelse(pos > -1L, substring(X, pos + 1L), ""))
    }

    # stop if extension not allowed
    if (!extsn %in% c("unk", "wav", "mp3")) stop("File format cannot be read")

    if (basename(as.character(X)) != X) {
      path <- dirname(as.character(X))
      X <- basename(as.character(X))
    } else
    # if path wasn't provided and still doesn't exist
    if (is.null(path)) {
      path <- getwd()
    }

    if (is.na(try_na(from))) from <- 0
    if (is.na(try_na(to))) to <- Inf

    if (extsn == "wav") {
      read_fun <- function(X, path, header, from, to) {
        obj <- tuneR::readWave(filename = file.path(path, X), header = header, from = from, to = to, units = "seconds", toWaveMC = TRUE)

        # get a single channel
        if (is(obj, "WaveMC")) {
          obj <- if (!is.null(channel)) {
            Wave(obj[, if (channel > ncol(obj)) 1 else channel])
          } else {
            Wave(obj[, 1])
          }
        }

        return(obj)
      }
    }

    if (extsn == "mp3") read_fun <- function(X, path, header, from, to) bioacoustics::read_mp3(file = file.path(path, X), from = from, to = to)

    if (extsn == "unk") {
      read_fun <- function(X, path, header, from, to) {
        suppressWarnings(object <- try(tuneR::readWave(filename = file.path(path, X), header = header, units = "seconds", from = from, to = to, toWaveMC = TRUE), silent = TRUE))

        if (is(object, "try-error")) {
          object <- try(bioacoustics::read_mp3(file = file.path(path, X), from = from, to = to), silent = TRUE)
        }

        return(object)
      }
    }

    object <- read_fun(X, path, header, from, to)

    if (is(object, "try-error")) {
      stop("file cannot be read (try read_sound_file() for other sound file formats)")
    }
  } else {
    # check columns
    if (!all(c(
      "sound.files",
      "start", "end"
    ) %in% colnames(X))) {
      stop(paste(paste(c("sound.files", "start", "end")[!(c(
        "sound.files",
        "start", "end"
      ) %in% colnames(X))], collapse = ", "), "column(s) not found in data frame"))
    }

    # if there are NAs in start or end stop
    if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")

    # if end or start are not numeric stop
    if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop("'start' and 'end' must be numeric")

    # if any start higher than end stop
    if (any(X$end - X$start < 0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start < 0)), "case(s)"))

    # check path to working directory
    if (is.null(path)) {
      path <- getwd()
    } else if (!dir.exists(path)) {
      stop("'path' provided does not exist")
    } else {
      path <- normalizePath(path)
    }

    # convert attr(X, "check.results")$sound.files to character if factor
    if (is.factor(attr(X, "check.results")$sound.files)) {
      attr(X, "check.results")$sound.files <- as.character(attr(X, "check.results")$sound.files)
    }

    filename <- file.path(path, X$sound.files[index])

    if (header) {
      if (any(is_selection_table(X), is_extended_selection_table(X))) {
        object <- list(sample.rate = attr(X, "check.results")$sample.rate[attr(X, "check.results")$sound.files == X$sound.files[index]][1] * 1000, channels = 1, bits = attr(X, "check.results")$bits[attr(X, "check.results")$sound.files == X$sound.files[index]][1], samples = attr(X, "check.results")$n.samples[attr(X, "check.results")$sound.files == X$sound.files[index]][1])
      } else {
        object <- tuneR::readWave(filename = filename, header = TRUE)
      }

      if (any(sapply(object, length) > 1)) object <- lapply(object, "[", 1)
    } else {
      if (is_selection_table(X) | is.data.frame(X) & !is_extended_selection_table(X)) # if no extended selection table
        {
          object <- tuneR::readWave(filename = filename, header = FALSE, units = "seconds", from = from, to = to, toWaveMC = TRUE)
          # if more than 1 channel
          if (is(object, "WaveMC") & !is.null(X$channel)) {
            object <- Wave(object[, X$channel[index]])
          } else {
            object <- Wave(object[, 1])
          }
        } else {
        object <- attr(X, "wave.objects")[[which(names(attr(X, "wave.objects")) == X$sound.files[index])[1]]]

        # if to is inifite then duration of sound file
        if (is.infinite(to)) to <- length(object@left) / object@samp.rate

        if (attr(X, "check.results")$mar.before[attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$selec == X$selec[index]] != 0 & attr(X, "check.results")$mar.after[attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$selec == X$selec[index]] != 0 & any(to < length(object@left) / object@samp.rate, from > 0)) object <- seewave::cutw(object, from = from, to = to, output = "Wave")
      }
    }
  }

  return(object)
}

