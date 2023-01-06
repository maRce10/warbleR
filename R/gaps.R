#' Gap duration
#'
#' \code{gaps} measures gap duration
#' @usage gaps(X = NULL, by = "sound.files", parallel = 1, pb = TRUE)
#' @param X 'selection_table', 'extended_selection_table' (created 'by.song') or data frame with the following columns: 1) "sound.files": name of the sound
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end":
#' end time of selections.
#' @param by Character vector with column names. Controls the levels at which gaps will be measured. "sound.files" must always be included.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @return A data frame identical to that supplied in 'X', with and additional column ('gaps') with the duration of the time interval between selections.
#' @export
#' @name gaps
#' @details The function measures the time intervals (i.e. gaps) between selections.  The gap for a given selection is calculated as the time interval to the selection immediately after. Hence, there is no gap for the last selection in a sound file (or level determined by the 'by' argument). Note that sound files are not required.
#' @seealso \code{\link{inflections}}, \code{\link{song_analysis}},
#' @examples{
#' # get warbleR sound file examples
#' data(list = "lbh_selec_table")
#'
#' # get gaps
#' gaps(X = lbh_selec_table)
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on mar-27-2018 (MAS)

gaps <- function(X = NULL, by = "sound.files", parallel = 1, pb = TRUE) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(gaps)

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

  # if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop("'parallel' should be a positive integer")

  # if character is not character
  if (!is.character(by)) stop("'by' must be a character vector")

  # if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")

  # if extended only by song
  if (is_extended_selection_table(X)) {
    if (!attributes(X)$by.song$by.song) stop("extended selection tables must be created 'by.song' to be used in song.param()")
  }

  # function to calculate gaps
  gaps_FUN <- function(Y) {
    # sort
    Y <- Y[order(Y$start), ]

    # fill with NAs
    Y$gaps <- NA

    # if more than 1 row calculate gaps
    if (nrow(Y) > 1) {
      Y$gaps[-nrow(Y)] <- Y$end[-1] - Y$start[-nrow(Y)]
    }

    Y <- as.data.frame(Y)
    return(Y)
  }

  # add sound files to by
  if (all(by != "sound.files")) {
    by <- c("sound.files", by)
  }

  # set levels for splitting
  X$..by <- apply(X[, by, drop = FALSE], 1, paste, collapse = "-")

  # add order column to sort data after calculations
  X$..order <- 1:nrow(X)

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }


  # run loop apply function
  gaps_l <- pblapply_wrblr_int(pbar = pb, X = unique(X$..by), cl = cl, FUN = function(i) {
    Y <- X[X$..by == i, ]

    return(gaps_FUN(Y))
  })

  # put into a data frame
  gaps_df <- do.call(rbind, gaps_l)

  # order as original
  gaps_df <- gaps_df[order(gaps_df$..order), ]

  # remove extra columns
  gaps_df$..by <- gaps_df$..order <- NULL

  return(gaps_df)
}
