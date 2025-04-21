#' Calculates acoustic parameters at the song level
#'
#' \code{song_analysis} calculates descriptive statistics of songs or other higher levels of organization in the signals.
#' @param X 'selection_table', 'extended_selection_table' (created 'by.song') or data frame with the following columns: 1) "sound.files": name of the sound
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end":
#' end time of selections.
#' @param song_colm Character string with the column name containing song labels.
#' It can be used to label any hierarchical level at which parameters need to be calculated (e.g. syllables, phrases).
#' Note that
#' the function assumes that song labels are not repeated within a sound file.
#' @param mean_colm Numeric vector with the index of the columns that will be averaged. If \code{NULL} the mean of all numeric columns in 'X' is returned.
#' @param min_colm Character vector with the name(s) of the columns for which the minimum
#' value is needed. Default is \code{NULL}.
#' @param max_colm Character vector with the name(s) of the columns for which the maximum
#' value is needed. Default is \code{NULL}.
#' @param elm_colm Character vector with the name(s) of the columns identifying the element labels (i.e. element types). If supplied 'unq.elms' and 'mean.elm.count' are returned. Default is \code{NULL}.
#' @param elm_fun Function to be applied to the sequence of elements composing a song. Default is \code{NULL}. Ignored if 'elm_colm' is not supplied. The name of the column containing the function's output is "elm_fun'.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param sd Logical value indicating whether standard deviation is also returned for
#' variables in which averages are reported. Default is \code{FALSE}.
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param na.rm Logical value indicating whether 'NA' values should be ignored for calculations.
#' @param weight Character vector defining 1 or more numeric vectors to weight average
#' measurements (i.e. song parameters). Names of numeric columns in 'X' can also be used. See \code{\link[stats]{weighted.mean}}.
#'  for more details.  Default is \code{NULL} (unweighted average).
#' @return A data frame similar to the input 'X' data frame, but in this case each row corresponds to a single song. The data frame contains the mean or extreme
#'  values for numeric columns for each song. Columns that will be averaged can be defined with
#'  'mean_colm' (otherwise all numeric columns are used). Columns can be
#'  weighted by other columns in the data set (e.g. duration, frequency range). In addition, the function returns the following song level parameters:
#' \itemize{
#'    \item \code{elm.duration}: mean length of elements (in s)
#'    \item \code{song.duration}: length of song (in s)
#'    \item \code{num.elms}: number of elements (or song units)
#'    \item \code{start}: start time of song (in s)
#'    \item \code{end}: end time of song (in s)
#'    \item \code{bottom.freq}: lowest 'bottom.freq' from all song elements (in kHz)
#'    \item \code{top.freq}: highest 'top.freq' from all song elements (in kHz)
#'    \item \code{freq.range}: difference between song's 'top.freq' and 'bottom.freq' (in kHz)
#'    \item \code{song.rate}: number of elements per second (NA if only 1 element). Calculated as the number of elements in the 'song' divided by the duration of the song. In this case song duration is calculated as the time between the start of the first element and the start of the last element, which provides a rate that is less affected by the duration of individual elements. Note that this calculation is different than that from 'song.duration' above.
#'    \item \code{gap.duration}: average length of gaps (i.e. silences) in between elements
#'    (in s, NA if only 1 element)
#'    \item \code{elm.types}: number of element types (i.e. number of unique types, only if 'elm_colm' is supplied)
#'    \item \code{mean.elm.count}: mean number of times element types are found (only if 'elm_colm' is supplied)
#'    }
#'    This function assumes that song labels are not repeated within a sound file.
#'
#' @export
#' @name song_analysis
#' @details The function calculates average or extreme values of acoustic parameters of
#' elements in a song or other level of organization in the signals.
#' @seealso \code{\link{spectro_analysis}}
#' @examples{
#' # get warbleR sound file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#'
#' # add a 'song' column
#' lbh_selec_table$song <- c("song1", "song1", "song1", "song2",
#'   "song2", "song3", "song3", "song3", "song4", "song4", "song4")
#'
#' # measure acoustic parameters
#' sp <- spectro_analysis(lbh_selec_table[1:8, ], bp = c(1, 11), 300, fast = TRUE, path = tempdir())
#'
#' # add song data
#' sp <- merge(sp, lbh_selec_table[1:8, ], by = c("sound.files", "selec"))
#'
#' # caculate song-level parameters for all numeric parameters
#' song_analysis(X = sp, song_colm = "song", parallel = 1, pb = TRUE)
#'
#' # caculate song-level parameters selecting parameters with mean_colm
#' song_analysis(X = sp, song_colm = "song",mean_colm = c("dfrange", "duration"),
#'  parallel = 1, pb = TRUE)
#'
#' # caculate song-level parameters for selecting parameters with mean_colm, max_colm
#' # and min_colm and weighted by duration
#' song_analysis(X = sp, weight = "duration", song_colm = "song",
#' mean_colm =  c("dfrange", "duration"), min_colm =  "mindom", max_colm = "maxdom",
#'   parallel = 1, pb = TRUE)
#'
#' # with two weights
#' song_analysis(X = sp, weight = c("duration", "dfrange"), song_colm = "song",
#' mean_colm = c("kurt", "sp.ent"), parallel = 1, pb = TRUE)
#'
#' # with two weights no progress bar
#' song_analysis(X = sp, weight = c("duration", "dfrange"), song_colm = "song",
#' mean_colm = c("kurt", "sp.ent"), parallel = 1, pb = FALSE)
#' }
#'
#' @references 
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

song_analysis <- function(X = NULL, song_colm = "song", mean_colm = NULL, min_colm = NULL, max_colm = NULL, elm_colm = NULL, elm_fun = NULL,
                          sd = FALSE, parallel = 1, pb = TRUE, na.rm = FALSE, weight = NULL) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(song_analysis)

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

  # if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop2("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")

  # if extended only by song
  if (is_extended_selection_table(X)) {
    if (!attributes(X)$by.song$by.song) stop2("extended selection tables must be created 'by.song' to be used in song.param()")
  }

  # if parallel is not numeric
  if (!is.numeric(parallel)) stop2("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop2("'parallel' should be a positive integer")

  if (!any(names(X) == song_colm)) stop2("'song_colm' not found")


  # check suplied column names
  if (!is.null(mean_colm) & !any(names(X) %in% mean_colm)) stop2("at least 1 'mean_colm' supplied not found")

  if (!is.null(min_colm) & !any(names(X) %in% min_colm)) stop2("at least 1 'min_colm' supplied not found")

  if (!is.null(max_colm) & !any(names(X) %in% max_colm)) stop2("at least 1 'max_colm' supplied not found")

  if (!is.null(elm_colm) & !any(names(X) == elm_colm)) stop2("'elm_colm' not found")

  if (!is.null(elm_colm) & !is.null(elm_fun) & if (!is.null(elm_fun)) !is.function(get(as.character(quote(elm_fun)))) else FALSE) stop2("'elm_fun' not found")

  if (song_colm == "sound.files") {
    X$song <- X$sound.files
    song_colm <- "song"
  }

  # stop if any song is found in more than 1 sound file
  # if (any(tapply(X$sound.files, X[,song_colm], function(x) length(unique(x))) > 1))
  #  stop2("At least 1 'song' is found in multiple sound files, which is not allowed. \n Try `tapply(X$sound.files, X$song, function(x) length(unique(x)))` to find 'problematic' songs (those higher than 1)")

  if (!all(c(
    "sound.files", "selec",
    "start", "end"
  ) %in% colnames(X))) {
    stop2(paste(paste(c("sound.files", "selec", "start", "end")[!(c(
      "sound.files", "selec",
      "start", "end"
    ) %in% colnames(X))], collapse = ", "), "column(s) not found in data frame"))
  }

  # if (is.null(X$duration)) X$duration <- X$end - X$start

  if (!any(names(X) %in% weight) & !is.null(weight)) stop2("'weight' column not found")

  if (!is.null(mean_colm)) {
    if (!all(sapply(X[, mean_colm], is.numeric))) stop2("not all columns in 'mean_colm' are numeric")
  }

  songparam.FUN <- function(Y, song_colm, mean_colm, min_colm, max_colm, weight, na.rm) {
    Z <- Y[1, , drop = FALSE]

    # set weight and calculated mean parameters
    if (!is.null(weight) & nrow(Y) > 1) {
      wts <- Y[, weight, drop = FALSE]

      if (length(weight) > 1) {
        wts <- apply(wts, 2, function(e) e / max(e, na.rm = TRUE))
        wts <- apply(wts, 1, prod)
      }
      wts <- as.vector(unlist(wts))
    } else {
      wts <- rep(1, nrow(Y))
    }

    if (is.null(mean_colm)) {
      mean_colm <- names(X)[(sapply(X, is.numeric))]
      mean_colm <- as.vector(mean_colm[!mean_colm %in% c("selec", "channel", song_colm, "start", "end", "top.freq", "bottom.freq", min_colm, max_colm)])
      mean_colm <- mean_colm[!is.na(mean_colm)]
    }

    for (u in mean_colm) Z[, u] <- stats::weighted.mean(x = as.vector(c(Y[, u, drop = TRUE])), w = wts, na.rm = na.rm)

    if (sd) {
      W <- Z

      for (u in mean_colm) W[, u] <- stats::sd(x = as.vector(c(Y[, u, drop = TRUE])), na.rm = na.rm)

      names(W) <- paste0("sd.", names(W))
    }

    # minimums
    if (!is.null(min_colm)) {
      for (u in min_colm) Z[, u] <- min(x = as.vector(c(Y[, u, drop = TRUE])), na.rm = na.rm)
    }


    # maximums
    if (!is.null(max_colm)) for (u in max_colm) Z[, u] <- max(x = as.vector(c(Y[, u, drop = TRUE])), na.rm = na.rm)

    colm <- c(mean_colm, min_colm, max_colm)

    # sort columns
    Z <- Z[, c(names(Z) %in% c("sound.files", song_colm, colm))]
    Z <- Z[, match(c("sound.files", song_colm, colm), names(Z))]

    # add sd variables
    if (sd) {
      Z <- cbind(Z, W[, paste0("sd.", mean_colm), drop = FALSE])
    }

    Z$num.elms <- nrow(Y)
    Z$start <- min(Y$start)
    Z$end <- max(Y$end)
    Z$elm.duration <- mean(Y$end - Y$start)
    if (!is.null(Y$bottom.freq)) {
      try(Z$bottom.freq <- min(Y$bottom.freq, na.rm = na.rm), silent = TRUE)
      try(Z$top.freq <- max(Y$top.freq, na.rm = na.rm), silent = TRUE)
      try(Z$freq.range <- Z$top.freq - Z$bottom.freq, silent = TRUE)
    }
    Z$song.duration <- Z$end - Z$start
    Z$song.rate <- if (Z$num.elms == 1) NA else Z$num.elms / (Y$start[nrow(Y)] - Y$start[1])
    Z$gap.duration <- if (Z$num.elms == 1) NA else mean(Y$start[-1] - Y$end[-nrow(Y)])

    if (sd) {
      Z$sd.gap.duration <- if (Z$num.elms == 1) NA else sd(Y$start[-1] - Y$end[-nrow(Y)])
      Z$sd.elm.duration <- sd(Y$end - Y$start)
    }

    # add element parameters
    if (!is.null(elm_colm)) {
      Z$elm.types <- length(unique(Y[, elm_colm]))
      Z$mean.elm.count <- Z$num.elms / Z$elm.types

      # run element function
      if (!is.null(elm_fun)) {
        Z$......temp.name.... <- tapply(Y[, elm_colm], Y[, song_colm], elm_fun)
        names(Z)[names(Z) == "......temp.name...."] <- as.character(quote(elm_fun))
      }
    }


    # rename columns containing min or max
    if (!is.null(min_colm)) {
      names(Z)[names(Z) %in% min_colm] <- paste0("min.", min_colm)
    }

    if (!is.null(max_colm)) {
      names(Z)[names(Z) %in% max_colm] <- paste0("max.", max_colm)
    }

    return(Z)
  }

  X$.....SONGX... <- paste(X$sound.files, X[, song_colm])

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  # run loop apply function
  out <- .pblapply(pbar = pb, X = unique(X$.....SONGX...), cl = cl, message = "computing song features", total = 1, FUN = function(i) {
    # subset by song label
    Y <- X[X$.....SONGX... == i, , drop = FALSE]

    # order Y by start
    Y <- Y[order(Y$start), ]

    # make it a data frame just in case
    Y <- as.data.frame(Y)

    return(songparam.FUN(Y, song_colm, mean_colm, min_colm, max_colm, weight, na.rm))
  })

  df <- do.call(rbind, out)
  df <- as.data.frame(df)

  # rename columns
  rownames(df) <- 1:nrow(df)

  # add selec label
  df$selec <- 1

  # reorder columns
  df <- sort_colms(df)

  return(df)
}
