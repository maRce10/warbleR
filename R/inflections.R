#' Count number of inflections in a frequency contour
#'
#' \code{inflections} counts the number of inflections in a frequency contour (or any time series)
#' @param X data frame with the columns for "sound.files" (sound file name), "selec" (unique identifier for each selection) and columns for each of the frequency values of the contours. No other columns should be included.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @return A data frame with 3 columns: "sound.files", "selec" and "infls" (number of inflections).
#' @export
#' @name inflections
#' @details The function counts the number of inflections in a frequency contour.
#' @seealso \code{\link{freq_ts}}, \code{\link{track_freq_contour}},  \code{\link{gaps}}
#' @examples{
#' # get warbleR sound file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' # measure frequency contours
#' dom.freq.ts <- freq_ts(X = lbh_selec_table, path = tempdir())
#'
#' # get number of inflections
#' inflections(X = dom.freq.ts)
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on mar-27-2018 (MAS)

inflections <- function(X = NULL, parallel = 1, pb = TRUE) {
  # if parallel is not numeric
  if (!is.numeric(parallel)) stop2("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop2("'parallel' should be a positive integer")

  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(inflections)

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

  infls.FUN <- function(Y, l) {
    if (l) ts <- Y$frequency else ts <- Y[, !names(Y) %in% c("sound.files", "selec")]
    if (is.data.frame(ts)) ts <- unlist(ts)

    infls <- length(which(c(FALSE, diff(diff(ts) > 0) != 0)))

    Y$inflections <- infls

    Y <- Y[1, colnames(Y) %in% c("sound.files", "selec", "inflections"), drop = FALSE]

    return(Y)
  }

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  if (is.data.frame(X)) lvs <- 1:nrow(X) else lvs <- seq_len(length(X))

  # run loop apply function
  out <- .pblapply(pbar = pb, X = lvs, cl = cl, message = "computing inflections", total = 1, FUN = function(i) {
    is.df <- is.data.frame(X)

    if (is.df) Y <- X[i, , drop = FALSE] else Y <- X[[i]]

    return(infls.FUN(Y, l = !is.df))
  })

  return(do.call(rbind, out))
}
