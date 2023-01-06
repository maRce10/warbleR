#' Detect frequency range iteratively
#'
#' \code{freq_range} detect frequency range iteratively from signals in a selection table.
#' @usage freq_range(X, wl = 512, it = "jpeg", line = TRUE, fsmooth = 0.1, threshold = 10,
#' dB.threshold = NULL, wn = "hanning", flim = NULL, bp = NULL,
#' propwidth = FALSE, xl = 1, picsize = 1, res = 100, fast.spec = FALSE, ovlp = 50,
#' pal = reverse.gray.colors.2, parallel = 1, widths = c(2, 1), main = NULL,
#' img = TRUE, mar = 0.05, path = NULL, pb = TRUE, impute = FALSE)
#' @param X object of class 'selection_table', 'extended_selection_table' or data frame with the following columns: 1) "sound.files": name of the sound
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end":
#' end time of selections. The output of \code{\link{auto_detec}} can
#' also be used as the input data frame.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default
#'   is 512. This is used for calculating the frequency spectrum (using \code{\link[seewave]{meanspec}})
#'   and producing the spectrogram (using \code{\link[seewave]{spectro}}, if \code{img = TRUE}).
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param line Logical argument to add red lines (or box if bottom.freq and top.freq columns are provided) at start and end times of selection. Default is \code{TRUE}.
#' @param fsmooth A numeric vector of length 1 to smooth the frequency spectrum with a mean
#'  sliding window in kHz. This help to average amplitude "hills" to minimize the effect of
#'  amplitude modulation. Default is 0.1.
#' @param threshold Amplitude threshold (\%) for frequency range detection. The frequency range (not the cumulative amplitude) is represented as percentage (100\% = highest amplitude). Default is 10. Ignored if 'dB.threshold' is supplied.
#' @param dB.threshold Amplitude threshold for frequency range detection (in dB). The value indicates the decrease in dB in relation to the highest amplitude (e.g. the peak frequency) in which range will be detected. For instance a dB.threshold = 20 means that the amplitude threshold would be 20 dB below the highest amplitude. If provided 'threshold' is ignored. Default is \code{NULL}.
#' Note that the power spectrum is normalized when using a dB scale, so it looks different than the one produced when no dB scale is used (e.g. when using 'threshold' argument).
#' @param wn Character vector of length 1 specifying window name. Default is
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options. This is used for calculating the frequency spectrum (using \code{\link[seewave]{meanspec}}) and producing the spectrogram (using \code{\link[seewave]{spectro}}, if \code{img = TRUE}).
#' @param flim A numeric vector of length 2 for the frequency limit of
#'   the spectrogram (in kHz), as in \code{\link[seewave]{spectro}}. Default is \code{NULL}.
#' @param bp A numeric vector of length 2 for the lower and upper limits of a
#'   frequency bandpass filter (in kHz) or "frange" to indicate that values in 'bottom.freq'
#'   and 'top.freq' columns will be used as bandpass limits. Default is c(0, 22).
#' @param propwidth Logical argument to scale the width of spectrogram
#'   proportionally to duration of the selected call. Default is \code{FALSE}.
#' @param xl Numeric vector of length 1. A constant by which to scale
#'   spectrogram width. Default is 1.
#' @param picsize Numeric argument of length 1. Controls relative size of
#'   spectrogram. Default is 1.
#' @param res Numeric argument of length 1. Controls image resolution.
#'   Default is 100 (faster) although 300 - 400 is recommended for publication/
#'   presentation quality.
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR:specCols]{gray.1}}, \code{\link[monitoR:specCols]{gray.2}},
#' \code{\link[monitoR:specCols]{gray.3}}, \code{\link[monitoR:specCols]{topo.1}} and \code{\link[monitoR:specCols]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast.spec' spectrograms. Palette colors \code{\link[monitoR:specCols]{gray.1}}, \code{\link[monitoR:specCols]{gray.2}},
#' \code{\link[monitoR:specCols]{gray.3}} offer
#' decreasing darkness levels.
#' @param pal Color palette function for spectrogram. Default is reverse.gray.colors.2. See
#' \code{\link[seewave]{spectro}} for more palettes. Palettes as \code{\link[monitoR:specCols]{gray.2}} may work better when \code{fast.spec = TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 50. This is used for calculating the frequency spectrum (using \code{\link[seewave]{meanspec}}) and producing the spectrogram (using \code{\link[seewave]{spectro}}, if \code{img = TRUE}).
#' @param widths Numeric vector of length 2 to control the relative widths of the spectro (first element) and spectrum (second element).
#' @param main  Character vector of length 1 specifying the img title. Default is \code{NULL}.
#' @param img Logical. Controls whether a plot is produced. Default is \code{TRUE}.
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the selections
#'  to set spectrogram limits. Default is 0.05.
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param impute Logical. If \code{TRUE} then missing range values are imputed
#' with the corresponding bandpass value (hence ignored when \code{bp = NULL}). Default is \code{FALSE}.
#' @return The original data frame with an additional 2 columns for low and high frequency values. A plot is produced in the working directory if \code{img = TRUE} (see details).
#' @export
#' @name freq_range
#' @details This functions aims to automatize the detection of frequency ranges. The frequency range is calculated as follows:
#' \itemize{
#'  \item bottom.freq = the start frequency of the amplitude 'hill' containing the highest amplitude at the given threshold.
#'  \item top.freq = the end frequency of the amplitude 'hill' containing the highest amplitude at the given threshold.
#'   }
#'   If \code{img = TRUE} a graph including a spectrogram and a frequency spectrum is
#'   generated for each selection (saved as an image file in the working directory). The graph would include gray areas in the frequency ranges excluded by the bandpass ('bp' argument), dotted lines highlighting the detected range. The function \code{\link{freq_range_detec}} is used internally.
#' @seealso \code{\link{freq_range_detec}}, \code{\link{auto_detec}}
#' @examples
#' {
#'   data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#'   writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#'   writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#'   writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#'   writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#'   freq_range(
#'     X = lbh_selec_table, wl = 112, fsmooth = 1, threshold = 13, widths = c(4, 1),
#'     img = TRUE, pb = TRUE, it = "tiff", line = TRUE, mar = 0.1, bp = c(1, 10.5),
#'     flim = c(0, 11), path = tempdir()
#'   )
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on mar-12-2018 (MAS)

freq_range <- function(X, wl = 512, it = "jpeg", line = TRUE, fsmooth = 0.1, threshold = 10, dB.threshold = NULL, wn = "hanning", flim = NULL, bp = NULL, propwidth = FALSE, xl = 1, picsize = 1, res = 100, fast.spec = FALSE, ovlp = 50, pal = reverse.gray.colors.2, parallel = 1, widths = c(2, 1), main = NULL, img = TRUE, mar = 0.05, path = NULL, pb = TRUE, impute = FALSE) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(freq_range)

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

  if (!all(c(
    "sound.files", "selec",
    "start", "end"
  ) %in% colnames(X))) {
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c(
      "sound.files", "selec",
      "start", "end"
    ) %in% colnames(X))], collapse = ", "), "column(s) not found in data frame"))
  }

  # if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")

  # if end or start are not numeric stop
  if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop("'start' and 'end' must be numeric")

  # if any start higher than end stop
  if (any(X$end - X$start <= 0)) stop(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))

  # if any selections longer than 20 secs warning
  if (any(X$end - X$start > 20)) warning2(paste(length(which(X$end - X$start > 20)), "selection(s) longer than 20 sec"))

  # return warning if not all sound files were found
  if (!is_extended_selection_table(X)) {
    fs <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)
    if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) {
      cat(paste(
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

  # internal function to detect freq range
  frangeFUN <- function(X, i, img, bp, wl, fsmooth, threshold, wn, flim, ovlp, fast.spec, pal, widths) {
    r <- warbleR::read_sound_file(X = X, path = path, index = i, header = TRUE)
    f <- r$sample.rate
    t <- c(X$start[i] - mar, X$end[i] + mar)

    # adjust margins if signal is close to start or end of sound file
    mar1 <- mar

    if (t[1] < 0) {
      t[1] <- 0
      mar1 <- X$start[i]
    }

    mar2 <- mar1 + X$end[i] - X$start[i]

    if (t[2] > r$samples / f) t[2] <- r$samples / f


    # read rec segment
    r <- warbleR::read_sound_file(X = X, path = path, index = i, from = t[1], to = t[2])

    frng <- frd_wrblr_int(wave = seewave::cutw(r, from = mar1, to = mar2, output = "Wave"), wl = wl, fsmooth = fsmooth, threshold = threshold, dB.threshold = dB.threshold, wn = wn, bp = bp, ovlp = ovlp)

    if (img) {
      # Spectrogram width can be proportional to signal duration
      if (propwidth) {
        pwc <- (13.16) * ((t[2] - t[1]) / 0.27) * xl * picsize
      } else {
        pwc <- (13.16) * xl * picsize
      }

      # call image function
      img_wrlbr_int(
        filename = paste0(X$sound.files[i], "-", X$selec[i], "-", "freq_range.", it), path = path,
        width = pwc, height = (10.16), units = "cm", res = res
      )

      frd_plot_wrblr_int(wave = r, detections = frng, wl = wl, wn = wn, flim = flim, bp = bp, fast.spec = fast.spec, ovlp = ovlp, pal = pal, widths = widths, main = paste(X$sound.files[i], X$selec[i], sep = "-"), all.detec = F)

      dev.off()
    }
    # return low and high freq
    return(data.frame(X[i, grep("bottom.freq|top.freq", names(X), invert = TRUE)], bottom.freq = frng$frange$bottom.freq, top.freq = frng$frange$top.freq))
  }




  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  # run loop apply function
  fr <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(X), cl = cl, FUN = function(i) {
    # if bp is frange
    if (!is.null(bp)) {
      if (bp[1] == "frange") b <- c(X$bottom.freq[i], X$top.freq[i]) else b <- bp
    } else {
      b <- bp
    }

    frangeFUN(X = X, i = i, img = img, bp = b, wl = wl, fsmooth = fsmooth, threshold = threshold, wn = wn, flim = flim, ovlp = ovlp, fast.spec = fast.spec, pal = pal, widths = widths)
  })

  fr <- do.call(rbind, fr)

  if (impute & !is.null(bp)) {
    fr$bottom.freq[is.na(fr$bottom.freq)] <- bp[1]
    fr$top.freq[is.na(fr$top.freq)] <- bp[2]
  }

  if (any(!sapply(fr[, c("start", "end", "bottom.freq", "top.freq")], is.numeric))) fr[, c("start", "end", "bottom.freq", "top.freq")] <- rapply(fr[, c("start", "end", "bottom.freq", "top.freq")], as.numeric)

  row.names(fr) <- 1:nrow(fr)

  return(fr)
}

##############################################################################################################
#' alternative name for \code{\link{freq_range}}
#'
#' @keywords internal
#' @details see \code{\link{freq_range}} for documentation. \code{\link{frange}} will be deprecated in future versions.
#' @export

frange <- freq_range
