#' Get sound file parameter information
#'
#' \code{info_sound_files} summariz sound file information
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param files character vector indicating the set of files that will be consolidated. File names should not include the full file path. Optional.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param skip.error Logical to control if errors are omitted. If so, files that could not be read will be excluded and their name printed in the console. Default is \code{FALSE}, which will return an error if some files are problematic.
#' @param file.format Character string with the format of sound files. By default all sound file formats supported by warbleR are included ("\\\.wav$|\\\.wac$|\\\.mp3$|\\\.flac$"). Note that several formats can be included using regular expression syntax as in \code{\link[base]{grep}}. For instance \code{"\\\.wav$|\\\.mp3$"} will only include .wav and .mp3 files.
#' @return A data frame with descriptive information about the sound files in the working directory (or 'path'). See "details".
#' @export
#' @name info_sound_files
#' @details This function is a wrapper for \code{\link{selection_table}} that returns a data frame with the following descriptive parameters for each sound file in the working directory (or 'path'):
#' \itemize{
#'    \item \code{duration}: duration of selection in seconds
#'    \item \code{sample.rate}: sampling rate in kHz
#'    \item \code{channels}: number of channels
#'    \item \code{bits}: bit depth
#'    \item \code{wav.size}: sound file size in MB
#'    \item \code{samples}: number of samples in the sound file
#'    }
#'
#' @seealso \code{\link{fix_wavs}}, \code{\link{selection_table}} & \code{\link{check_sels}}
#' @examples{
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' #get info
#' info_sound_files(path = tempdir())
#' }
#'
#' @references 
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

info_sound_files <- function(path = NULL, files = NULL, parallel = 1, pb = TRUE, skip.error = FALSE, file.format = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$") {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(info_sound_files)

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

  # make a selection table from files
  st <- selection_table(whole.recs = TRUE, path = path, parallel = parallel, pb = pb, verbose = FALSE, skip.error = skip.error, file.format = file.format, files = files)

  # extract check sels
  cs <- attributes(st)$check.results

  # remove 'selec' column and rename 'n.samples'
  cs$selec <- NULL
  names(cs)[names(cs) == "n.samples"] <- "samples"

  # return cs data frame
  return(cs)
}
