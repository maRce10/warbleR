#' Remove channels in wave files
#'
#' \code{remove_channels} remove channels in wave files
#' @usage remove_channels(files = NULL, channels, path = NULL, parallel = 1, pb = TRUE)
#' @param files Character vector indicating the files that will be analyzed. If not provided. Optional.
#' then all wave files in the working directory (or path) will be processed.
#' @param channels Numeric vector indicating the index (or channel number) for the channels that will be kept (left = 1, right = 2; 3 to inf for multichannel sound files).
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @return Sound files that have been converted are saved in the new
#' folder "converted_sound_files". If `img = TRUE` then spectrogram images highlighting the silence segments
#' that were removed are also saved.
#' @export
#' @name remove_channels
#' @details The function removes channels from wave files. It works on regular and
#' multichannel wave files. Converted files are saved in a new directory ("converted_sound_files")
#' and original files are not modified.
#' @seealso \code{\link{fix_wavs}}, \code{\link{info_wavs}},
#' @examples{
#' # save sound file examples
#' data("Phae.long1")
#' Phae.long1.2 <- stereo(Phae.long1, Phae.long1)
#'
#' writeWave(Phae.long1.2, file.path(tempdir(), "Phae.long1.2.wav"))
#'
#' remove_channels(channels = 1, path = tempdir())
#'
#' #check this floder
#' tempdir()
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on Jul-30-2018 (MAS)

remove_channels <- function(files = NULL, channels, path = NULL, parallel = 1, pb = TRUE) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(remove_channels)

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
    for (q in seq_len(length(opt.argms))) {
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

  # read files
  fls <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)

  # stop if files are not in working directory
  if (length(fls) == 0) stop2("no sound files in working directory")

  # subet based on file list provided (flist)
  if (!is.null(files)) fls <- fls[fls %in% files]
  if (length(fls) == 0) stop2("sound files are not in working directory")

  dir.create(file.path(path, "converted_sound_files"))

  mcwv_FUN <- function(x, channels) {
    wv <- tuneR::readWave(file.path(path, x), toWaveMC = TRUE)

    if (nchannel(wv) >= max(channels)) {
      wv <- wv[, channels]

      if (nchannel(wv) <= 2) wv <- Wave(wv)

      writeWave(object = wv, filename = file.path(path, "converted_sound_files", x), extensible = FALSE)

      a <- 0
    } else {
      a <- 1
    }

    return(a)
  }

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  # run loop apply function
  out_l <- pblapply_wrblr_int(pbar = pb, X = fls, cl = cl, FUN = function(x) {
    mcwv_FUN(x, channels)
  })

  # make it a vector
  out <- unlist(out_l)

  if (sum(out) > 0) {
    warning2(x = paste(sum(out), "file(s) not processed (# channels < max(channels)"))
  }
}

##############################################################################################################
#' alternative name for \code{\link{remove_channels}}
#'
#' @keywords internal
#' @details see \code{\link{remove_channels}} for documentation. \code{\link{rm_channels}} will be deprecated in future versions.
#' @export

rm_channels <- remove_channels
