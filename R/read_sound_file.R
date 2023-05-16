#' An extended version of read_wave that reads several sound file formats and files from selection tables
#'
#' \code{read_sound_file} reads several sound file formats as well as files referenced in selection tables
#' @usage read_sound_file(X, index, from = X$start[index], to = X$end[index],
#' channel = X$channel[index], header = FALSE, path = NULL)
#' @param X 'data.frame', 'selection_table' or 'extended_selection_table' containing columns for sound file name (sound.files),
#' selection number (selec), and start and end time of signals (start and end). Alternatively, the name of a sound file or URL address to sound file can be provided. The function can read sound files in 'wav', 'mp3', 'flac' and 'wac' format. The file name can contain the directory path.
#' 'top.freq' and 'bottom.freq' columns are optional. Default is \code{NULL}.
#' @param index Index of the selection in 'X' that will be read. Ignored if 'X' is \code{NULL}.
#' @param from Where to start reading, in seconds. Default is \code{X$start[index]}.
#' @param to Where to stop reading, in seconds. Default is \code{X$end[index]}.
#' @param channel Channel to be read from sound file (1 = left, 2 = right, or higher number for multichannel waves). Default is \code{X$channel[index]}. If a 'channel' column does not exist it will read the first channel.
#' @param header If \code{TRUE}, only the header information of the Wave object is returned, otherwise (the default) the whole Wave object.
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used. If 'X' refers to a sound file including its directory 'path' is ignored.
#' @return An object of class "Wave".
#' @export
#' @name read_sound_file
#' @details The function is a wrapper for \code{\link[tuneR]{readWave}} that read sound files, including those referenced in selection tables. It
#' is also used internally by warbleR functions to read wave objects from extended selection tables (see \code{\link{selection_table}} for details). For reading 'flac' files on windows the path to the .exe is required. This can be set globally using the 'flac.path' argument in \code{\link{warbleR_options}}. Note that reading 'flac' files requires creating a temporary copy in 'wav' format, which can be particularly slow for long files. 
#' @examples
#' \dontrun{
#' # write wave files with lower case file extension
#' data(list = c("Phae.long1"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#'
#' # read from selection table
#' read_sound_file(X = lbh_selec_table, index = 1, path = tempdir())
#'
#' # from extended selection table
#' library(NatureSounds)
#' read_sound_file(X = lbh.est, index = 1)
#'
#' # read from selection table
#' read_sound_file(X = lbh_selec_table, index = 1, path = tempdir())
#'
#' # read WAV
#' filepath <- system.file("extdata", "recording.wav", package = "bioacoustics")
#' read_sound_file(filepath)
#'
#' # read MP3
#' filepath <- system.file("extdata", "recording.mp3", package = "bioacoustics")
#' read_sound_file(filepath)
#'
#' # read WAC
#' filepath <- system.file("extdata", "recording_20170716_230503.wac", package = "bioacoustics")
#' read_sound_file(filepath, from = 0, to = 0.2)
#'
#' # URL file
#' read_sound_file(X = "https://www.xeno-canto.org/513948/download")
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on may-7-2018 (MAS)

read_sound_file <- function(X, index = NULL, from = X$start[index], to = X$end[index], channel = X$channel[index], header = FALSE, path = NULL) {
  # if is extended then index must be provided
  if (is.data.frame(X) & is.null(index)) stop2('"index" needed when a  "data.frame", "selection_table", "extended_selection_table" is provided')

  # if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X), is.character(X), is.factor(X))) stop2("X is not of a class 'data.frame', 'selection_table', 'extended_selection_table' or a sound file name")

  # if is not a data frame
  if (is.character(X) | is.factor(X)) {
    # if URL
    if (grepl("^www.|^http:|^https:", X)) {
      # get  temporary file name
      temp.file <- tempfile()

      # download it
      download.file(url = X, destfile = temp.file, quiet = TRUE, mode = "wb", cacheOK = TRUE, extra = getOption("download.file.extra"))

      if (!file.exists(temp.file)) stop2("File couldn't be downloaded")

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
    if (!extsn %in% c("unk", "wav", "mp3", "wac", "flac")) stop2("File format cannot be read")

    if (basename(as.character(X)) != X) {
      filename <- X
    } else { # if path wasn't provided and still doesn't exist
      if (is.null(path)) {
        path <- getwd()
      }

      filename <- file.path(path, X)
    }

    if (is.na(warbleR::try_na(from))) from <- 0
    if (is.na(warbleR::try_na(to))) to <- Inf
    if (is.na(warbleR::try_na(channel))) channel <- 1

    object <- read_soundfile_wrblr_int(filename, header, from, to, extension = extsn, channel = channel)

    if (is(object, "try-error")) {
      stop2("file cannot be read")
    }
  } else {
    # check columns
    if (!all(c(
      "sound.files",
      "start", "end"
    ) %in% colnames(X))) {
      stop2(paste(paste(c("sound.files", "start", "end")[!(c(
        "sound.files",
        "start", "end"
      ) %in% colnames(X))], collapse = ", "), "column(s) not found in data frame"))
    }

    # if there are NAs in start or end stop
    if (any(is.na(c(X$end, X$start)))) stop2("NAs found in start and/or end")

    # if end or start are not numeric stop
    if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop2("'start' and 'end' must be numeric")

    # if any start higher than end stop
    if (any(X$end - X$start < 0)) stop2(paste("The start is higher than the end in", length(which(X$end - X$start < 0)), "case(s)"))

    # check path to working directory
    if (is.null(path)) {
      path <- getwd()
    } else if (!dir.exists(path)) {
      stop2("'path' provided does not exist")
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
        pos <- regexpr("\\.([[:alnum:]]+)$", X$sound.files[index])
        extsn <- tolower(ifelse(pos > -1L, substring(X$sound.files[index], pos + 1L), ""))

        object <- read_soundfile_wrblr_int(filename = filename, header = TRUE, extension = extsn)
      }

      if (any(sapply(object, length) > 1)) object <- lapply(object, "[", 1)
    } else {
      if (is_selection_table(X) | is.data.frame(X) & !is_extended_selection_table(X)) # if no extended selection table
        {
          pos <- regexpr("\\.([[:alnum:]]+)$", X$sound.files[index])
          extsn <- tolower(ifelse(pos > -1L, substring(X$sound.files[index], pos + 1L), ""))

          object <- read_soundfile_wrblr_int(filename = filename, header = FALSE, from = from, to = to, extension = extsn, channel = channel)
        } else {
        object <- attr(X, "wave.objects")[[which(names(attr(X, "wave.objects")) == X$sound.files[index])[1]]]

        # if to is infinite then duration of sound file
        if (is.infinite(to)) to <- length(object@left) / object@samp.rate

        # if margin before != 0 and
        if (attr(X, "check.results")$mar.before[attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$selec == X$selec[index]] != 0 & attr(X, "check.results")$mar.after[attr(X, "check.results")$sound.files == X$sound.files[index] & attr(X, "check.results")$selec == X$selec[index]] != 0 & any(to < length(object@left) / object@samp.rate, from > 0)) object <- seewave::cutw(object, from = from, to = to, output = "Wave")
      }
    }
  }

  return(object)
}


### internals for reading sound files
read_wave_wrblr_int <- function(filename, header = header, from = 0, to = Inf, channel = 1) {
  obj <- tuneR::readWave(filename = filename, header = header, from = from, to = to, units = "seconds", toWaveMC = TRUE)

  # Extract one channel
  if (!header) {
    obj <- Wave(obj[, channel])
  }

  return(obj)
}

read_mp3_wrblr_int <- function(filename, header = FALSE, from = 0, to = Inf, channel = 1) {
  obj <- tuneR::readMP3(file = filename)
  if (from > 0 | to != Inf) obj <- seewave::cutw(wave = obj, f = obj@samp.rate, from = from, to = to, output = "Wave")

  if (header) {
    obj <- list(sample.rate = obj@samp.rate, channels = if (obj@stereo) 2 else 1, bits = obj@bit, samples = length(obj@left))
  }

  return(obj)
}

read_wac_wrblr_int <- function(filename, header = FALSE, from = 0, to = Inf, channel = 1) {
  text_output <- testthat::capture_output_lines(obj <- bioacoustics::read_wac(file = filename)[[1]])
  if (from > 0 | to != Inf) obj <- seewave::cutw(wave = obj, f = obj@samp.rate, from = from, to = to, output = "Wave")

  if (header) {
    obj <- list(sample.rate = obj@samp.rate, channels = if (obj@stereo) 2 else 1, bits = obj@bit, samples = length(obj@left))
  }

  return(obj)
}

read_flac_wrblr_int <- function(filename, header = FALSE, from = 0, to = Inf, channel = 1, flac.path) {

   # set path to flac 
  if (is.null(getOption("warbleR")$flac.path)) {
    
    # on linox and macOS
    if (.Platform$OS.type == "unix") {
      if (missing(flac.path)) {
        run_flac <- "flac"
      } else {
        run_flac <- paste(flac.path, "flac", sep = "/")
      }
      if (system(paste(run_flac, "-v"), ignore.stderr = TRUE) !=
          0) 
        stop2("FLAC program was not found")
    }
    
    # on windows
    if (.Platform$OS.type == "windows") {
      if (missing(flac.path)) {
        "flac" <- "flac.exe"
      }
      if (missing(flac.path)) {
        run_flac <- paste("C:/Program Files/FLAC/", "flac",
                          sep = ""
        )
        if (!file.exists(run_flac)) {
          run_flac <- paste("C:/Program Files (x86)/FLAC/",
                            "flac",
                            sep = ""
          )
        }
      } else {
        run_flac <- paste(flac.path, "flac", sep = "/")
      }
      if (!file.exists(run_flac)) 
        stop2("FLAC program was not found")
    }
    
    warbleR_options(flac.path = if (missing("flac.path")) "" else flac.path)
  } else
    run_flac <- if (getOption("warbleR")$flac.path %in% c("", "flac")) "flac" else
file.path(getOption("warbleR")$flac.path, "flac")
  
   # create temporary file for convertin to a wav to be read at the end
  temp_wav <- tempfile()

  if (.Platform$OS.type == "unix") 
    system_call <- paste0(run_flac, " -d ", filename, " --output-name=", temp_wav)

  if (.Platform$OS.type == "windows")
    system_call <- paste(shQuote(run_flac), "-d", shQuote(filename,
      type = "cmd"
    ), sep = " ")

  # make call silent
  system_call <- paste(system_call, "--totally-silent")

  # add start and end time if supplied
  if (from > 0) {
    start <- gsub("\\.", ",", paste0(floor(from / 60), ":", from - (floor(from / 60) * 60)))
    system_call <- paste0(system_call, " --skip=", start)
  }

  if (to != Inf) {
    end <- gsub("\\.", ",", paste0(floor(to / 60), ":", to - (floor(to / 60) * 60)))
    system_call <- paste0(system_call, " --until=", end)
  }

  # run flac
  out <- system(system_call, ignore.stderr = TRUE)

  # read temporary wav file
  wav <- read_wave_wrblr_int(temp_wav, header, 0, Inf, channel)

  # remove temporary wav file
  unlink(temp_wav)

  return(wav)
}

read_soundfile_wrblr_int <- function(filename, header, from = 0, to = Inf, extension = "unk", channel = 1) {
  if (is.null(channel) | is.function(channel)) {
    channel <- 1
  }

  switch(EXPR = extension,
    wav = object <- read_wave_wrblr_int(filename, header, from, to, channel),
    mp3 = object <- read_mp3_wrblr_int(filename, header, from, to, channel),
    wac = object <- read_wac_wrblr_int(filename, header, from, to, channel),
    flac = object <- read_flac_wrblr_int(filename, header, from, to, channel),
    unk = {
      object <- try(read_wave_wrblr_int(filename, header, from, to, channel), silent = TRUE)

      if (is(object, "try-error")) {
        object <- try(read_mp3_wrblr_int(filename, header, from, to, channel), silent = TRUE)
      }

      if (is(object, "try-error")) {
        object <- try(read_wac_wrblr_int(filename, header, from, to, channel), silent = TRUE)
      }

      if (is(object, "try-error")) {
        object <- try(read_flac_wrblr_int(filename, header, from, to, channel), silent = TRUE)
      }
    }
  )
  return(object)
}
