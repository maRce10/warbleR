#' Find clipped selections
#'
#' \code{find_clipping} gets the proportion of samples that are clipped.
#' @usage find_clipping(X, path = NULL, parallel = 1, pb = TRUE)
#' @param X 'selection_table', 'extended_selection_table' or data frame with the following columns: 1) "sound.files": name of the sound
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end":
#' end time of selections.
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @return A data frame with the 'sound.files' and 'selec' columns in X plus an additional column ('prop.clipped') indicating
#' the proportion of clipped samples for each row. If sound files are stereo the average proportion of the two channels is returned.
#' @export
#' @name find_clipping
#' @examples
#' {
#'   # load data
#'   data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
#'   writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
#'   writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#'
#'   find_clipping(X = lbh_selec_table[1:5, ], path = tempdir())
#' }
#' @details Clipping (i.e. saturation) occurs when an audio signal is amplified above the maximum limit of the recorder. This leads to distortion and a lowering of audio quality. If stereo the mean proportion of both channels is returned. The function assumes specific range values for different bit depths as detailed in \code{\link[tuneR]{normalize}}.
#' @seealso \code{\link{sig2noise}}
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on mar-13-2018 (MAS)

find_clipping <- function(X, path = NULL, parallel = 1, pb = TRUE) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(compare_methods)

  # get warbleR options
  opt.argms <- if (!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0

  options(warn = -1)
  on.exit(options(warn = 0), add = TRUE)

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

  # if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop2("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")

  # check basic columns in X
  if (!all(c(
    "sound.files", "selec",
    "start", "end"
  ) %in% colnames(X))) {
    stop2(paste(paste(c("sound.files", "selec", "start", "end")[!(c(
      "sound.files", "selec",
      "start", "end"
    ) %in% colnames(X))], collapse = ", "), "column(s) not found in data frame"))
  }

  # if parallel is not numeric
  if (!is.numeric(parallel)) stop2("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop2("'parallel' should be a positive integer")


  out <- pblapply_wrblr_int(1:nrow(X), pbar = pb, cl = parallel, function(x) {
    # read wave
    wv <- read_wave(X, index = x, path = path)

    # table with expected range of amplitudes by bit depth
    bit_ranges <- data.frame(bit = c(1, 8, 16, 24, 32, 64), low = c(-1, 0, -32767, -8388607, -2147483647, -1), high = c(1, 254, 32767, 8388607, 2147483647, 1))

    bit_range <- bit_ranges[bit_ranges$bit == wv@bit, c("low", "high"), drop = TRUE]

    # proportion clipped
    prop.clipped <- sum(wv@left >= bit_range$high) / length(wv@left)

    if (wv@stereo) {
      prop.clipped <- mean(prop.clipped, sum(wv@right >= bit_range$high) / length(wv@right))
    }

    # put all in a data frame
    out_df <- data.frame(sound.files = X$sound.files[x], selec = X$selec[x], prop.clipped)

    return(out_df)
  })

  output <- do.call(rbind, out)
  return(output)
}
