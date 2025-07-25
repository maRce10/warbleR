#' Measure wavelet packet decomposition features (EXPERIMENTAL)
#'
#' \code{wpd_features} Measure wavelet packet decomposition features.
#' @param X object of class 'selection_table', 'extended_selection_table' or data frame with the following columns: 1) "sound.files": name of the sound
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end":
#' end time of selections.
#' @param normalize Logical to determine if features are normalized by signal duration.
#' @param threshold1 Threshold (\%) for wavelet coefficient detection. Equivalent to denominator of equation 6 in Selin et al (2007). Must be a value between 0 and 1.
#' @param threshold2 Threshold for width detection. Equivalent to threshold 2 (th2) in equation 7 in Selin et al (2007).
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @return A data frame with rows for each of the selections in 'X' in addition to four wavelet packet decomposition features: max.energy, position, spread and width.
#' @export
#' @name wpd_features
#' @details Measures wavelet packet decomposition features. STILL UNDER DEVELOPMENT. USE IT UNDER YOUR OWN RISK.
#' @seealso \code{\link{mfcc_stats}}, \code{\link{mfcc_stats}}
#' @examples
#' {
#'   data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
#'   writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#'   writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#'
#'   # not normalize
#'   wpd_features(lbh_selec_table[1:5, ], threshold2 = 0.3, nor = FALSE, path = tempdir())
#' }
#'
#' @references 
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#'
#' Selin A., J. Turunen, and J. T. Tanttu, 2007. Wavelets in recognition of bird sounds. EURASIP Journal on Advances in Signal Processing.
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

wpd_features <- function(X, normalize = TRUE, threshold1 = 6, threshold2 = 0.5, path = NULL, pb = TRUE, parallel = 1) {
  # error message if wavethresh is not installed
  if (!requireNamespace("wavethresh", quietly = TRUE)) {
    stop2("must install 'wavethresh' to use this function")
  }

  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(wpd_features)

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

  # if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop2("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")

  if (!all(c(
    "sound.files", "selec",
    "start", "end"
  ) %in% colnames(X))) {
    stop2(paste(paste(c("sound.files", "selec", "start", "end")[!(c(
      "sound.files", "selec",
      "start", "end"
    ) %in% colnames(X))], collapse = ", "), "column(s) not found in data frame"))
  }

  # if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop2("NAs found in start and/or end")

  # if end or start are not numeric stop
  if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop2("'start' and 'end' must be numeric")

  # if any start higher than end stop
  if (any(X$end - X$start <= 0)) stop2(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))

  # if any selections longer than 20 secs warning
  if (any(X$end - X$start > 20)) warning2(paste(length(which(X$end - X$start > 20)), "selection(s) longer than 20 sec"))

  # return warning if not all sound files were found
  if (!is_extended_selection_table(X)) {
    fs <- list.files(path = path, ignore.case = TRUE)
    if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) {
      message2(paste(
        length(unique(X$sound.files)) - length(unique(X$sound.files[(X$sound.files %in% fs)])),
        "sound file(s) not found"
      ))
    }

    # count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% fs)
    if (length(d) == 0) {
      stop2("The sound files are not in the working directory")
    } else {
      X <- X[d, ]
    }
  }

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  # run loop apply function
  wdps <- .pblapply(pbar = pb, X = 1:nrow(X), cl = cl, message = "computing WPD features", total = 1, FUN = function(i) {
    # read rec segment
    r <- warbleR::read_sound_file(X = X, path = path, index = i)

    # run internal warbleR function to measure parameters
    ftrs <- wpd_feature_wrblr_int(wave = r, normalize = normalize, thr1 = threshold1, thr2 = threshold2)

    # return low and high freq
    return(data.frame(sound.files = X$sound.files[i], selec = X$selec[i], t(ftrs)))
  })

  out <- do.call(rbind, wdps)

  return(out)
}
