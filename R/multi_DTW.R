#' A wrapper on \code{\link[dtw]{dtwDist}} for comparing multivariate contours
#'
#' \code{multi_DTW} is a wrapper on \code{\link[dtw]{dtwDist}} that simplify applying dynamic time warping on multivariate contours.
#' @param ts.df1 Optional. Data frame with frequency contour time series of signals to be compared.
#' @param ts.df2 Optional. Data frame with frequency contour time series of signals to be compared.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing). Not available in Windows OS.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @param window.type	\code{\link[dtw]{dtw}} windowing control parameter. Character: "none", "itakura", or a function (see \code{\link[dtw]{dtw}}).
#' @param open.end \code{\link[dtw]{dtw}} control parameter. Performs
#' open-ended alignments (see \code{\link[dtw]{dtw}}).
#' @param scale Logical. If \code{TRUE} dominant frequency values are z-transformed using the \code{\link[base]{scale}} function, which "ignores" differences in absolute frequencies between the signals in order to focus the
#' comparison in the frequency contour, regardless of the pitch of signals. Default is \code{TRUE}.
#' @param dist.mat Logical controlling whether a distance matrix (\code{TRUE},
#' default) or a tabular data frame (\code{FALSE}) is returned.
#' @param ... Additional arguments to be passed to \code{\link{track_freq_contour}} for customizing
#' graphical output.
#' @return A matrix with the pairwise dissimilarity values. If img is
#' \code{FALSE} it also produces image files with the spectrograms of the signals listed in the
#' input data frame showing the location of the dominant frequencies.
#' @family spectrogram creators
#' @seealso \code{\link{freq_ts}}
#' @export
#' @name multi_DTW
#' @details This function extracts the dominant frequency values as a time series and
#'  then calculates the pairwise acoustic dissimilarity using dynamic time warping.
#' The function uses the \code{\link[stats:approxfun]{approx}} function to interpolate values between dominant
#'  frequency  measures. If 'img' is  \code{TRUE} the function also produces image files
#'  with the spectrograms of the signals listed in the input data frame showing the
#'  location of the dominant frequencies.
#' @examples
#' \dontrun{
#' # load data
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#'
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' # measure
#' df <- freq_ts(X = lbh_selec_table, threshold = 10, img = FALSE, path = tempdir())
#' se <- freq_ts(X = lbh_selec_table, threshold = 10, img = FALSE, path = tempdir(), type = "entropy")
#'
#' # run function
#' multi_DTW(df, se)
#' }
#'
#' @references 
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

multi_DTW <- function(ts.df1 = NULL, ts.df2 = NULL, pb = TRUE, parallel = 1, window.type = "none", open.end = FALSE, scale = FALSE, dist.mat = TRUE, ...) {
  options(digits = 5)

  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(freq_ts)

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

  if (is.null(ts.df1) & is.null(ts.df2)) stop2("both 'ts.df1' or 'ts.df2' must be provided")

  if (!all.equal(dim(ts.df1), dim(ts.df2))) stop2("both time series data frames must have the same dimensions")

  # stop if only 1 selection
  if (nrow(ts.df1) < 2) stop2("you need more than one selection for DTW")

  if (!all(c("sound.files", "selec") %in% names(ts.df1))) {
    stop2(paste(paste(c("sound.files", "selec")[!(c("sound.files", "selec") %in% names(ts.df1))], collapse = ", "), "column(s) not found in ts.df1"))
  }

  if (!all(c("sound.files", "selec") %in% names(ts.df2))) {
    stop2(paste(paste(c("sound.files", "selec")[!(c("sound.files", "selec") %in% names(ts.df2))], collapse = ", "), "column(s) not found in ts.df2"))
  }


  if (!all(sapply(ts.df1[, 3:ncol(ts.df1)], is.numeric))) stop2(" columns 3:ncol(ts.df) must be numeric")

  # order time series data frames
  ts.df1 <- ts.df1[order(ts.df1$sound.files, ts.df1$selec), ]
  ts.df2 <- ts.df2[order(ts.df2$sound.files, ts.df2$selec), ]

  if (!all.equal(ts.df1[, names(ts.df1) %in% c("sound.files", "selec")], ts.df2[, names(ts.df2) %in% c("sound.files", "selec")])) stop2("Selections/sound file labels differ between the two time series data frames")


  if (any(is.na(ts.df1)) | any(is.na(ts.df2))) stop2("missing values in time series are not allowed")


  if (scale) {
    ts.df1[, 3:ncol(ts.df1)] <- t(apply(ts.df1[, 3:ncol(ts.df1)], 1, scale))
    ts.df2[, 3:ncol(ts.df2)] <- t(apply(ts.df2[, 3:ncol(ts.df2)], 1, scale))
  }


  multi.dtw.FUN <- function(ts.df1, ts.df2, combs, i, ...) {
    mts1 <- cbind(t(ts.df1[ts.df1$sf.sels == combs[i, 1], 3:ncol(ts.df2)]), t(ts.df2[ts.df1$sf.sels == combs[i, 1], 3:ncol(ts.df2)]))

    mts2 <- cbind(t(ts.df1[ts.df1$sf.sels == combs[i, 2], 3:ncol(ts.df2)]), t(ts.df2[ts.df1$sf.sels == combs[i, 2], 3:ncol(ts.df2)]))

    dst <- try(dtw::dtw(mts1, mts2, open.end = open.end, window.type = window.type, ...)$distance, silent = TRUE)
    if (is(dst, "try-error")) dst <- NA

    return(dst)
  }

  ts.df1$sf.sels <- paste(ts.df1$sound.files, ts.df1$selec, sep = "-")
  combs <- t(utils::combn(ts.df1$sf.sels, 2))

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  # run loop apply function
  out <- .pblapply(pbar = pb, X = 1:nrow(combs), cl = cl, message = "computing multidimensional DTW", total = 1, FUN = function(i) {
    multi.dtw.FUN(ts.df1, ts.df2, combs, i, ...)
  })


  dist.df <- data.frame(sound.file.selec.1 = combs[, 1], sound.file.selec.2 = combs[, 2], dtw.dist = unlist(out))
  if (dist.mat) {
    # create a similarity matrix with the max xcorr
    mat <- matrix(nrow = nrow(ts.df1), ncol = nrow(ts.df1))
    diag(mat) <- 0
    colnames(mat) <- rownames(mat) <- paste(ts.df1$sound.files, ts.df1$selec, sep = "-")

    mat[lower.tri(mat, diag = FALSE)] <- dist.df$dtw.dist
    mat <- t(mat)
    mat[lower.tri(mat, diag = FALSE)] <- dist.df$dtw.dist
    return(mat)
  } else {
    return(dist.df)
  }
}
