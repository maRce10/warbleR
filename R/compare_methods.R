#' @title Assessing the performance of acoustic distance measurements
#'
#' @description \code{compare_methods} creates graphs to visually assess performance of acoustic distance measurements
#' @usage compare_methods(X = NULL, flim = NULL, bp = NULL, mar = 0.1, wl = 512, ovlp = 90,
#' res = 150, n = 10, length.out = 30,
#' methods = NULL,
#' it = "jpeg", parallel = 1, path = NULL, sp = NULL, custom1 = NULL,
#' custom2 = NULL, pb = TRUE, grid = TRUE,  clip.edges = TRUE,
#' threshold = 15, na.rm = FALSE, scale = FALSE, pal = reverse.gray.colors.2,
#' img = TRUE, ...)
#' @param X 'selection_table' object or data frame with results from \code{\link{auto_detec}}
#' function, or any data frame with columns for sound file name (sound.files),
#' selection number (selec), and start and end time of signal (start and end).
#' Default \code{NULL}.
#' @param flim A numeric vector of length 2 for the frequency limit in kHz of
#'   the spectrogram, as in \code{\link[seewave]{spectro}}. Default is \code{NULL}.
#' @param bp numeric vector of length 2 giving the lower and upper limits of the
#' frequency bandpass filter (in kHz) used in the acoustic distance methods. Default is \code{NULL}.
#' @param mar Numeric vector of length 1. Specifies plot margins around selection in seconds. Default is 0.1.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram and cross-correlation, default
#'   is 512.
#' @param ovlp Numeric vector of length 1 specifying the percent overlap between two
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 90.
#' @param res Numeric argument of length 1. Controls image resolution.
#'   Default is 150.
#' @param n Numeric argument of length 1. Defines the number of plots to be produce.
#' Default is 10.
#' @param length.out A character vector of length 1 giving the number of measurements of fundamental or dominant
#' frequency desired (the length of the time series). Default is 30.
#' @param methods A character vector of length 2 giving the names of the acoustic distance
#' methods that would be compared. The methods available are:
#' \itemize{
#'    \item \code{XCORR}: cross-correlation (\code{\link{cross_correlation}} function)
#'    \item \code{dfDTW}: dynamic time warping on dominant frequency contours (\code{\link{freq_DTW}} function)
#'    \item \code{ffDTW}: dynamic time warping on fundamental frequency contours (\code{\link{freq_DTW}} function)
#'    \item \code{SP}: spectral parameters (\code{\link{spectro_analysis}} function)
#'    \item \code{SPharm}: spectral parameters (\code{\link{spectro_analysis}} function with argument \code{harmonicity  = TRUE})
#'    \item \code{MFCC}: statistical descriptors of Mel frequency cepstral coefficients (\code{\link{mfcc_stats}} function)
#'    }
#'  Default \code{NULL}.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param sp DEPRECATED.
#' @param custom1 Data frame containing user parameters. The data frame must have 4 columns: the first 2 columns are 'sound.files'
#' and "selec' columns as in 'X', the other 2 (columns 3 and 4) are
#' 2 numeric columns to be used as the 2 parameters representing custom measurements. If the data has more than 2 parameters try using PCA (i.e. \code{\link[stats]{prcomp}} function)to summarize it in 2 dimensions before using it as an input. Default is \code{NULL}.
#' @param custom2 Data frame containing user parameters with the same format as 'custom1'. 'custom1' must be provided first. Default is \code{NULL}.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param grid Logical argument to control the presence of a grid on the spectrograms (default is \code{TRUE}).
#' @param clip.edges Logical argument to control whether edges (start or end of signal) in
#' which amplitude values above the threshold were not detected will be removed when using dfDTW and
#' ffDTW methods. If \code{TRUE} this edges will be excluded and signal contour will be calculated on the
#' remaining values. Default is \code{TRUE}.
#' @param threshold amplitude threshold (\%) for dominant and/or fundamental frequency detection when using dfDTW, ffDTW
#' and SP methods. Default is 15.
#' @param na.rm Logical. If \code{TRUE} all NAs produced when pairwise cross-correlations failed are removed from the
#' results. This means that all selections with at least 1 cross-correlation that failed are excluded in both methods under
#' comparison. Only apply if XCORR is one of the methods being compared.
#' @param scale Logical. If \code{TRUE} dominant and/or fundamental frequency values are z-transformed using the \code{\link[base]{scale}} function, which "ignores" differences in absolute frequencies between the signals in order to focus the
#' comparison in the frequency contour, regardless of the pitch of signals. Default is \code{TRUE}.
#' @param pal A color palette function to be used to assign colors in the
#'   spectrograms, as in \code{\link[seewave]{spectro}}. Default is reverse.gray.colors.2.
#' @param img A logical argument specifying whether an image files would be produced. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to a modified version of \code{\link[seewave]{spectro}} for customizing
#' graphical output. This includes fast.spec, an argument that speeds up the plotting of spectrograms (see description in
#' \code{\link{spectrograms}}).
#' @return Image files with 4 spectrograms of the selection being compared and scatterplots
#' of the acoustic space of all signals in the input data frame 'X'.
#' @export
#' @name compare_methods
#' @details This function produces graphs with spectrograms from 4 signals in the
#' provided data frame that allow visual inspection of the performance of acoustic
#' distance methods at comparing those signals. The signals are randomly picked up
#' from the provided data frame (X argument).The spectrograms are all plotted with
#' the same frequency and time scales. The function compares 2 methods at a time. The
#' methods available are: cross-correlation
#' (XCORR, from \code{\link{cross_correlation}}), dynamic time warping on dominant frequency time
#' series (dfDTW, from \code{\link[dtw]{dtw}} applied on \code{\link{freq_ts}} output), dynamic time
#' warping on dominant frequency time series (ffDTW, from \code{\link[dtw]{dtw}} applied on
#' \code{\link{freq_ts}} output), spectral parameters (SP, from \code{\link{spectro_analysis}}). The graph also
#' contains 2 scatterplots (1 for each method) of the acoustic space of all signals in the
#' input data frame 'X', including the centroid as black dot. The compared selections are randomly picked up from the pool of
#' selections in the input data frame. The argument 'n' defines the number of comparisons (i.e. graphs)
#' to be produced. The acoustic pairwise distance between signals is shown next
#' to the arrows linking them. The font color of a distance value correspond to the font
#' color of the method that generated it, as shown in the scatterplots. Distances are
#' standardized, being 0 the distance of a signal to itself and 1 the farthest pairwise
#' distance in the pool of signals. Principal Component Analysis (\code{\link[stats]{prcomp}})
#' is applied to calculate distances when using spectral parameters (SP) and descriptors of cepstral coefficients (MFCC). In those cases the first 2 PC's are used. Classical
#' Multidimensional Scalling (also known as Principal Coordinates Analysis,
#' (\code{\link[stats]{cmdscale}})) is used for cross-correlation (XCORR) and any dynamic time warping method. The graphs are return as image files in the
#' working directory. The file name contains the methods being compared and the
#' row number of the selections. This function uses internally a modified version
#' of the \code{\link[seewave]{spectro}} function from seewave package to create spectrograms. Custom data can also be compared against the available methods (or against each other) using the arguments 'custom1' and 'custom2'.
#' @seealso \href{https://marce10.github.io/2017/02/17/Choosing_the_right_method_for_measuring_acoustic_signal_structure.html}{blog post on comparing methods}
#' @examples
#' \dontrun{
#' # Save to temporary working directory
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' compare_methods(
#'   X = lbh_selec_table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
#'   ovlp = 90, res = 200, n = 10, length.out = 30,
#'   methods = c("XCORR", "dfDTW"), parallel = 1, it = "jpeg", path = tempdir()
#' )
#'
#' # remove progress bar
#' compare_methods(
#'   X = lbh_selec_table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
#'   ovlp = 90, res = 200, n = 10, length.out = 30,
#'   methods = c("XCORR", "dfDTW"), parallel = 1, it = "jpeg", pb = FALSE, path = tempdir()
#' )
#'
#' # check this folder!
#' getwd()
#'
#'
#' # compare SP and XCORR
#' compare_methods(
#'   X = lbh_selec_table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
#'   ovlp = 90, res = 200, n = 10, length.out = 30,
#'   methods = c("XCORR", "SP"), parallel = 1, it = "jpeg", path = tempdir()
#' )
#'
#' # compare SP method against dfDTW
#' compare_methods(
#'   X = lbh_selec_table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
#'   ovlp = 90, res = 200, n = 10, length.out = 30,
#'   methods = c("dfDTW", "SP"), parallel = 1, it = "jpeg",
#'   path = tempdir()
#' )
#'
#' # alternatively we can provide our own SP matrix
#' Y <- spectro_analysis(lbh_selec_table, path = tempdir())
#'
#' # selec a subset of variables
#' Y <- Y[, 1:7]
#'
#' # PCA
#' Y <- prcomp(Y[, 3:ncol(Y)])$x
#'
#' # add sound files and selec columns
#' Y <- data.frame(lbh_selec_table[, c(1, 3)], Y[, 1:2])
#'
#' compare_methods(
#'   X = lbh_selec_table, methods = c("dfDTW"), custom1 = Y,
#'   path = tempdir()
#' )
#' }
#'
#' @references {Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}). It uses
#' internally a modified version of the \code{\link[seewave]{spectro}} function from
#' seewave package to create spectrograms.
# last modification on mar-13-2018 (MAS)

compare_methods <- function(X = NULL, flim = NULL, bp = NULL, mar = 0.1, wl = 512, ovlp = 90,
                            res = 150, n = 10, length.out = 30, methods = NULL,
                            it = "jpeg", parallel = 1, path = NULL, sp = NULL, custom1 = NULL, custom2 = NULL, pb = TRUE, grid = TRUE,
                            clip.edges = TRUE, threshold = 15, na.rm = FALSE, scale = FALSE,
                            pal = reverse.gray.colors.2, img = TRUE, ...) {
  # define number of steps in analysis to print message
  steps <- c(current = 1, total = 0)

  if (pb) {
    steps[2] <- 3
    if (any(methods == "XCORR")) steps[2] <- steps[2] + 1
    if (!is.null(custom1)) steps[2] <- steps[2] - 1
    if (!is.null(custom2)) steps[2] <- steps[2] - 1

    # for functions with no internal step count
    total.steps <- steps[2]
    current.step <- 1

    # set internally
    options("int_warbleR_steps" = steps)

    on.exit(options("int_warbleR_steps" = c(current = 0, total = 0)), add = TRUE)
  }

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
    stop("'path' provided does not exist")
  } else {
    path <- normalizePath(path)
  }

  # if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")

  # check basic columns in X
  if (!all(c(
    "sound.files", "selec",
    "start", "end"
  ) %in% colnames(X))) {
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c(
      "sound.files", "selec",
      "start", "end"
    ) %in% colnames(X))], collapse = ", "), "column(s) not found in data frame"))
  }

  # if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop("'parallel' should be a positive integer")

  # methods
  if (is.null(methods)) {
    if (is.null(custom1) & is.null(custom2)) stop("'methods' or custom data should be provided") else methods <- c(NA, NA)
  } else {
    if (length(methods) < 2 & is.null(custom1) & is.null(custom2)) stop("must provide 2 methods or custom data")
    if (length(methods) > 1 & !is.null(custom1) & is.null(custom2)) stop("only 1 method needed")
    if (length(setdiff(methods, c("XCORR", "dfDTW", "ffDTW", "SP", "SPharm", "MFCC"))) > 0) {
      stop(paste(setdiff(methods, c("XCORR", "dfDTW", "ffDTW", "SP", "SPharm", "MFCC")), "is (are) not valid method(s)"))
    }
  }


  # if flim is not vector or length!=2 stop
  if (!is.null(flim)) {
    if (!is.vector(flim)) {
      stop("'flim' must be a numeric vector of length 2")
    } else {
      if (!length(flim) == 2) stop("'flim' must be a numeric vector of length 2")
    }
  }

  # if bp is not vector or length!=2 stop
  if (!is.null(bp)) {
    if (!is.vector(bp)) {
      stop("'bp' must be a numeric vector of length 2")
    } else {
      if (!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")
    }
  }

  # if wl is not vector or length!=1 stop
  if (is.null(wl)) {
    stop("'wl' must be a numeric vector of length 1")
  } else {
    if (!is.vector(wl)) {
      stop("'wl' must be a numeric vector of length 1")
    } else {
      if (!length(wl) == 1) stop("'wl' must be a numeric vector of length 1")
    }
  }

  # if res is not vector or length!=1 stop
  if (is.null(res)) {
    stop("'res' must be a numeric vector of length 1")
  } else {
    if (!is.vector(res)) {
      stop("'res' must be a numeric vector of length 1")
    } else {
      if (!length(res) == 1) stop("'res' must be a numeric vector of length 1")
    }
  }

  # if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")

  # if any start higher than end stop
  if (any(X$end - X$start <= 0)) stop(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))

  # if any selections longer than 20 secs stop
  if (any(X$end - X$start > 20)) stop(paste(length(which(X$end - X$start > 20)), "selection(s) longer than 20 sec"))

  # If n is not numeric
  if (!is.numeric(n)) stop("'n' must be a numeric vector of length 1")
  if (any(!(n %% 1 == 0), n < 1)) stop("'n' should be a positive integer")

  # If length.out is not numeric
  if (!is.numeric(length.out)) stop("'length.out' must be a numeric vector of length 1")
  if (any(!(length.out %% 1 == 0), length.out < 1)) stop("'length.out' should be a positive integer")

  # return warning if not all sound files were found
  if (!is_extended_selection_table(X)) {
    fs <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)
    if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) {
      message2(paste(
        length(unique(X$sound.files)) - length(unique(X$sound.files[(X$sound.files %in% fs)])),
        "sound file(s) not found"
      ))
    }

    # count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% fs)
    if (length(d) == 0) {
      stop("The sound files are not in the working directory")
    } else {
      X <- X[d, ]
    }
  }

  # check sp data frame
  if (!is.null(sp)) warning2(x = "'sp' has been deprecated and will be ignored. Use 'custom1' instead")

  # check custom1 data frame
  if (!is.null(custom1)) {
    if (!is.data.frame(custom1)) stop("'custom1' must be a data frame")
    if (nrow(custom1) != nrow(X)) stop("'custom1' must have the same number of selections than X")
    if (ncol(custom1) != 4) stop("'custom1' must have 4 columns")
    if (!all(c("sound.files", "selec") %in% names(custom1))) stop("'sound.files' or 'selec' columns missing in 'custom1'")

    # over write method names
    if (is.null(custom2)) {
      methods[2] <- "custom1"
    } else {
      methods[1] <- "custom1"
    }
  }

  # check custom2 data frame
  if (!is.null(custom2)) {
    if (!is.null(custom2) & is.null(custom1)) stop("please provide 'custom1' first")
    if (!is.data.frame(custom2)) stop("'custom1' must be a data frame")
    if (nrow(custom2) != nrow(X)) stop("'custom1' must have the same number of selections than X")
    if (ncol(custom2) != 4) stop("'custom2' must have 4 columns")
    if (!all(c("sound.files", "selec") %in% names(custom2))) stop("'sound.files' or 'selec' columns missing in 'custom1'")

    methods[2] <- "custom2"
  }

  # if it argument is not "jpeg" or "tiff"
  if (!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))

  # create empty list for method results
  bidims <- list()

  if ("custom1" %in% methods) {
    if (anyNA(custom1)) stop("NAs were found in 'custom1'")

    if (length(setdiff(paste(custom1$sound.files, custom1$selec), paste(X$sound.files, X$selec))) > 0) stop("some/all custom 1 'sound.files' and/or 'selec' values don't match with those in 'X'")

    custom1 <- custom1[match(paste(custom1$sound.files, custom1$selec), paste(X$sound.files, X$selec)), ]

    bidims[[length(bidims) + 1]] <- custom1[, 3:4]
  }

  if ("custom2" %in% methods) {
    if (anyNA(custom2)) stop("NAs were found in 'custom2'")

    if (length(setdiff(paste(custom2$sound.files, custom2$selec), paste(X$sound.files, X$selec))) > 0) stop("some/all custom 2 'sound.files' and/or 'selec' values don't match with those in 'X'")


    custom2 <- custom2[match(paste(X$sound.files, X$selec), paste(custom2$sound.files, custom2$selec)), ]

    bidims[[length(bidims) + 1]] <- custom2[, 3:4]
  }

  if ("XCORR" %in% methods) {
    xcmat <- warbleR::cross_correlation(X, wl = wl, bp = bp, ovlp = ovlp, parallel = parallel, pb = pb, na.rm = na.rm, path = path, dens = NULL)

    MDSxcorr <- stats::cmdscale(1 - xcmat)
    MDSxcorr <- scale(MDSxcorr)
    bidims[[length(bidims) + 1]] <- MDSxcorr

    # remove the ones that failed cross-corr
    if (na.rm) X <- X[paste(X$sound.files, X$selec, sep = "-") %in% rownames(MDSxcorr), ]
    # update number of steps
    current.step <- steps[1] <- steps[1] + 2
    options("int_warbleR_steps" = steps)
  }

  if ("dfDTW" %in% methods) {
    if (pb) {
      message2(x = paste0("measuring dominant frequency contours (step ", current.step, " of ", total.steps, "):"), color = "black")
    }

    dtwmat <- warbleR::freq_ts(X,
      wl = wl, flim = flim, ovlp = ovlp, img = FALSE, parallel = parallel, length.out = length.out,
      pb = pb, clip.edges = clip.edges, threshold = threshold, path = path
    )

    dtwmat <- dtwmat[, 3:ncol(dtwmat)]

    if (scale) {
      dtwmat <- t(apply(dtwmat, 1, scale))
    }

    dm <- dtw::dtwDist(dtwmat, dtwmat, method = "DTW")

    MDSdtw <- stats::cmdscale(dm)
    MDSdtw <- scale(MDSdtw)
    bidims[[length(bidims) + 1]] <- MDSdtw

    # update number of steps
    current.step <- steps[1] <- steps[1] + 1
    options("int_warbleR_steps" = steps)
  }

  if ("ffDTW" %in% methods) {
    if (pb) {
     message2(x = paste0("measuring fundamental frequency contours (step ", current.step, " of ", total.steps, "):"), "black")
    }

    dtwmat <- warbleR::freq_ts(X,
      type = "fundamental", bp = bp, wl = wl, flim = flim, ovlp = ovlp, img = FALSE, parallel = parallel, length.out = length.out, ff.method = "seewave",
      pb = pb, clip.edges = clip.edges, threshold = threshold, path = path
    )

    dtwmat <- dtwmat[, 3:ncol(dtwmat)]

    if (scale) {
      dtwmat <- t(apply(dtwmat, 1, scale))
    }

    dm <- dtw::dtwDist(dtwmat, dtwmat, method = "DTW")

    MDSdtw <- stats::cmdscale(dm)
    MDSdtw <- scale(MDSdtw)
    bidims[[length(bidims) + 1]] <- MDSdtw

    # update number of steps
    current.step <- steps[1] <- steps[1] + 1
    options("int_warbleR_steps" = steps)
  }

  if ("SP" %in% methods) {
    if (pb) {
     message2(x = paste0("measuring spectral parameters (step ", current.step, " of ", total.steps, "):"), color = "black")
    }

    spmat <- warbleR::spectro_analysis(X, wl = wl, bp = bp, parallel = parallel, pb = pb, threshold = threshold, harmonicity = FALSE, path = path)

    PCsp <- prcomp(scale(spmat[, 3:ncol(spmat)]), center = TRUE, scale. = TRUE, rank. = 2)$x

    bidims[[length(bidims) + 1]] <- PCsp

    # update number of steps
    current.step <- steps[1] <- steps[1] + 1
    options("int_warbleR_steps" = steps)
  }

  if ("SPharm" %in% methods) {
    if (pb) {
     message2(x = paste0("measuring spectral parameters + harmonicity (step ", current.step, " of ", total.steps, "):"), "black")
    }

    spmat <- warbleR::spectro_analysis(X, wl = wl, bp = bp, parallel = parallel, pb = pb, threshold = threshold, harmonicity = TRUE, path = path)

    if (any(sapply(spmat, anyNA))) {
      spmat <- spmat[, !sapply(spmat, anyNA)]
      warning2("some NAs prdouced by SPharm, columns were removed")
    }

    PCsp <- prcomp(scale(spmat[, 3:ncol(spmat)]), center = TRUE, scale. = TRUE, rank. = 2)$x

    bidims[[length(bidims) + 1]] <- PCsp

    # update number of steps
    current.step <- steps[1] <- steps[1] + 1
    options("int_warbleR_steps" = steps)
  }

  if ("MFCC" %in% methods) {
    if (pb) {
     message2(x = paste0("measuring mel frequency cepstral coefficients (step ", current.step, " of ", total.steps, "):"), "black")
    }
    mfcc <- warbleR::mfcc_stats(X, wl = wl, bp = bp, parallel = parallel, pb = pb, path = path)

    if (any(sapply(mfcc, anyNA))) stop("NAs generated when calculated MFCC's, try a higher 'wl'")

    PCmfcc <- prcomp(mfcc[, 3:ncol(mfcc)], center = TRUE, scale. = TRUE, rank. = 2)$x

    bidims[[length(bidims) + 1]] <- PCmfcc

    # update number of steps
    current.step <- steps[1] <- steps[1] + 1
    options("int_warbleR_steps" = steps)
  }

  # name matchs changing order to match order in whic methods are ran
  nms <- match(c("custom1", "custom2", "XCORR", "dfDTW", "ffDTW", "SP", "SPharm", "MFCC"), methods)

  # add names
  names(bidims) <- c("custom1", "custom2", "XCORR", "dfDTW", "ffDTW", "SP", "SPharm", "MFCC")[!is.na(nms)]

  #
  maxdist <- lapply(bidims, function(x) max(stats::dist(x)))

  X$labels <- 1:nrow(X)

  # maximum of 100 items for combinations
  smps <- sample(x = 1:nrow(X), size = ifelse(nrow(X) > 100, 100, nrow(X)))
  combs <- combn(smps, 4)

  if (nrow(X) == 4) {
    n <- 1
    combs <- as.matrix(1:4)
    message2("Only 1 possible combination of signals")
  } else if (n > ncol(combs)) {
    n <- ncol(combs)
    message2(paste("Only", n, "possible combinations of signals"))
  }

  if (nrow(X) > 4) combs <- as.data.frame(combs[, sample(1:ncol(combs), n)])

  # create matrix for sppliting screen
  m <- rbind(
    c(0, 2.5 / 7, 3 / 10, 5 / 10), # 1
    c(4.5 / 7, 1, 3 / 10, 5 / 10), # 2
    c(0, 2.5 / 7, 0, 2 / 10), # 3
    c(4.5 / 7, 1, 0, 2 / 10), # 4
    c(0, 1 / 2, 5 / 10, 9 / 10), # 5
    c(1 / 2, 1, 5 / 10, 9 / 10), # 6
    c(0, 2.5 / 7, 2 / 10, 3 / 10), # 7
    c(2.5 / 7, 4.5 / 7, 0, 5 / 10), # 8
    c(4.5 / 7, 1, 2 / 10, 3 / 10), # 9
    c(0, 3.5 / 7, 9 / 10, 10 / 10), # 10
    c(3.5 / 7, 1, 9 / 10, 10 / 10)
  ) # 11

  # screen 1:4 for spectros
  # screen 5,6 for scatterplots
  # screen 7:9 for similarities/arrows
  # screen 10:11 method labels

  comp.methFUN <- function(X, u, res, bidims, m, mar, flim) {
    rs <- combs[, u]
    X <- X[rs, ]

    if (img) {
      img_wrlbr_int(filename = paste("comp.meth-", names(bidims)[1], "-", names(bidims)[2], "-", paste(X$labels, collapse = "-"), paste0(".", it), sep = ""), path = path, width = 16.25, height = 16.25, units = "cm", res = res)
    }

    graphics::split.screen(m, erase = FALSE)

    mxdur <- max(X$end - X$start) + mar * 2

    # set colors for numbers in scatterplots and spectrograms
    col <- rep("gray40", nrow(bidims[[1]]))

    col <- adjustcolor(col, alpha.f = 0.5)

    col[rs] <- hcl(h = seq(15, 375, length = 4 + 1), l = 65, c = 100)[1:4]

    col[rs] <- adjustcolor(col[rs], alpha.f = 0.8)

    invisible(lapply(c(7:9, 1:4, 5:6, 10:11), function(x) {
      graphics::screen(x, new = FALSE)
      par(mar = rep(0, 4))
      if (x < 5) {
        r <- warbleR::read_sound_file(X = X, path = path, index = x, header = TRUE)
        tlim <- c((X$end[x] - X$start[x]) / 2 + X$start[x] - mxdur / 2, (X$end[x] - X$start[x]) / 2 + X$start[x] + mxdur / 2)

        mar1 <- X$start[x] - tlim[1]
        mar2 <- mar1 + X$end[x] - X$start[x]

        if (tlim[1] < 0) {
          tlim1.fix <- TRUE
          tlim[2] <- abs(tlim[1]) + tlim[2]
          mar1 <- mar1 + tlim[1]
          mar2 <- mar2 + tlim[1]
          tlim[1] <- 0
        } else {
          tlim1.fix <- FALSE
        }

        if (tlim[2] > r$samples / r$sample.rate) {
          if (!tlim1.fix) {
            tlim[1] <- tlim[1] - (r$samples / r$sample.rate - tlim[2])
            mar1 <- X$start[x] - tlim[1]
            mar2 <- mar1 + X$end[x] - X$start[x]
            tlim[2] <- r$samples / r$sample.rate
          } else {
            tlim[2] <- r$samples / r$sample.rate
          }
        }

        if (is.null(flim)) {
          flim <- c(0, floor(r$sample.rate / 2000))
        }

        if (flim[2] > floor(r$sample.rate / 2000)) flim[2] <- floor(r$sample.rate / 2000)

        r <- warbleR::read_sound_file(X = X, path = path, index = x, from = tlim[1], to = tlim[2])

        spectro_wrblr_int2(wave = r, f = r@samp.rate, flim = flim, wl = wl, ovlp = ovlp, axisX = FALSE, axisY = FALSE, tlab = FALSE, flab = FALSE, palette = pal, grid = grid, ...)
        box(lwd = 2)
        if (x == 1 | x == 3) {
          text(tlim[2] - tlim[1], ((flim[2] - flim[1]) * 0.86) + flim[1], labels = X$labels[x], col = col[rs[x]], cex = 1.5, font = 2, pos = 2)
        } else {
          text(0, ((flim[2] - flim[1]) * 0.86) + flim[1], labels = X$labels[x], col = col[rs[x]], cex = 1.5, font = 2, pos = 4)
        }
        if (grid) {
          abline(v = c(mar1, mar2), lty = 4, col = "gray")
        }
      }

      # upper left
      if (x == 5) {
        plot(bidims[[1]], col = "white", xaxt = "n", yaxt = "n", xlim = c(min(bidims[[1]][, 1]) * 1.1, max(bidims[[1]][, 1]) * 1.1), ylim = c(min(bidims[[1]][, 2]) * 1.1, max(bidims[[1]][, 2]) * 1.1))
        box(lwd = 4)
        centro <- apply(bidims[[1]], 2, mean)
        points(centro[1], centro[2], pch = 20, cex = 2, col = "gray3")
        cex <- rep(1, nrow(bidims[[1]]))
        cex[rs] <- 1.4
        text(bidims[[1]], labels = 1:nrow(bidims[[1]]), col = col, cex = cex, font = 2)
      }

      # upper right
      if (x == 6) {
        plot(bidims[[2]], col = "white", xaxt = "n", yaxt = "n", xlim = c(min(bidims[[2]][, 1]) * 1.1, max(bidims[[2]][, 1]) * 1.1), ylim = c(min(bidims[[2]][, 2]) * 1.1, max(bidims[[2]][, 2]) * 1.1))
        box(lwd = 4)
        centro <- apply(bidims[[2]], 2, mean)
        points(centro[1], centro[2], pch = 20, cex = 2, col = "gray3")
        cex <- rep(1, nrow(bidims[[2]]))
        cex[rs] <- 1.4
        text(bidims[[2]], labels = 1:nrow(bidims[[2]]), col = col, cex = cex, font = 2)
      }

      # lower mid
      if (x == 8) {
        plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
        lim <- par("usr")
        rect(lim[1], lim[3] - 1, lim[2], lim[4] + 1, border = adjustcolor("#EFAA7B", alpha.f = 0), col = adjustcolor("#EFAA7B", alpha.f = 0.15))
        arrows(0, 5.5 / 7, 1, 5.5 / 7, code = 3, length = 0.09, lwd = 2)
        text(0.5, 5.36 / 7, labels = round(stats::dist(bidims[[1]][rs[c(1, 2)], ]) / maxdist[[1]], 2), col = "black", font = 2, pos = 3)
        text(0.5, 5.545 / 7, labels = round(stats::dist(bidims[[2]][rs[c(1, 2)], ]) / maxdist[[2]], 2), col = "gray50", font = 2, pos = 1)
        arrows(0, 1.5 / 7, 1, 1.5 / 7, code = 3, length = 0.09, lwd = 2)
        text(0.5, 1.4 / 7, labels = round(stats::dist(bidims[[1]][rs[c(3, 4)], ]) / maxdist[[1]], 2), col = "black", font = 2, pos = 3)
        text(0.5, 1.63 / 7, labels = round(stats::dist(bidims[[2]][rs[c(3, 4)], ]) / maxdist[[2]], 2), col = "gray50", font = 2, pos = 1)
        arrows(0, 2 / 7, 1, 5 / 7, code = 3, length = 0.09, lwd = 2)
        text(0.69, 4.16 / 7, labels = round(stats::dist(bidims[[1]][rs[c(2, 3)], ]) / maxdist[[1]], 2), col = "black", font = 2, pos = 3)
        text(0.85, 4.4 / 7, labels = round(stats::dist(bidims[[2]][rs[c(2, 3)], ]) / maxdist[[2]], 2), col = "gray50", font = 2, pos = 1)
        arrows(0, 5 / 7, 1, 2 / 7, code = 3, length = 0.09, lwd = 2)
        text(0.3, 4.16 / 7, labels = round(stats::dist(bidims[[1]][rs[c(1, 4)], ]) / maxdist[[1]], 2), col = "black", font = 2, pos = 3)
        text(0.15, 4.4 / 7, labels = round(stats::dist(bidims[[2]][rs[c(1, 4)], ]) / maxdist[[2]], 2), col = "gray50", font = 2, pos = 1)
      }

      # in between left
      if (x == 7) {
        plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
        lim <- par("usr")
        rect(lim[1], lim[3] - 1, lim[2], lim[4] + 1, border = adjustcolor("#EFAA7B", alpha.f = 0.15), col = adjustcolor("#EFAA7B", alpha.f = 0.15))
        arrows(0.5, 0, 0.5, 1, code = 3, length = 0.09, lwd = 2)
        text(0.53, 0.5, labels = round(stats::dist(bidims[[1]][rs[c(1, 3)], ]) / maxdist[[1]], 2), col = "black", font = 2, pos = 2)
        text(0.47, 0.5, labels = round(stats::dist(bidims[[2]][rs[c(1, 3)], ]) / maxdist[[2]], 2), col = "gray50", font = 2, pos = 4)
      }

      # in between right
      if (x == 9) {
        plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
        lim <- par("usr")
        rect(lim[1], lim[3] - 1, lim[2], lim[4] + 1, border = adjustcolor("#EFAA7B", alpha.f = 0.15), col = adjustcolor("#EFAA7B", alpha.f = 0.15))
        arrows(0.5, 0, 0.5, 1, code = 3, length = 0.09, lwd = 2)
        text(0.53, 0.5, labels = round(stats::dist(bidims[[1]][rs[c(2, 4)], ]) / maxdist[[1]], 2), col = "black", font = 2, pos = 2)
        text(0.47, 0.5, labels = round(stats::dist(bidims[[2]][rs[c(2, 4)], ]) / maxdist[[2]], 2), col = "gray50", font = 2, pos = 4)
      }

      # top (for method labels)
      if (x == 10) {
        plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
        lim <- par("usr")
        rect(lim[1], lim[3] - 1, lim[2], lim[4] + 1, border = "black", col = adjustcolor("#4ABDAC", alpha.f = 0.3))
        text(0.5, 0.5, labels = names(bidims)[1], col = "black", font = 2, cex = 1.2)
        box(lwd = 4)
      }

      if (x == 11) {
        plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
        lim <- par("usr")
        rect(lim[1], lim[3] - 1, lim[2], lim[4] + 1, border = "black", col = adjustcolor("#4ABDAC", alpha.f = 0.3))
        text(0.5, 0.5, labels = names(bidims)[2], col = "gray50", font = 2, cex = 1.2)
        box(lwd = 4)
      }
    }))
    if (img) {
      invisible(dev.off())
    }
    on.exit(invisible(close.screen(all.screens = TRUE)))
  }

  # save image files
  if (pb) {
   message2(x = paste0("creating image files (step ", current.step, " of ", total.steps, "):"), "black")
  }

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  a1 <- pblapply_wrblr_int(pbar = pb, X = 1:ncol(combs), cl = cl, FUN = function(u) {
    comp.methFUN(X, u, res, bidims, m, mar, flim)
  })
}
