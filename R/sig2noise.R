#' Measure signal-to-noise ratio
#'
#' \code{sig2noise} measures signal-to-noise ratio across multiple files.
#' @usage sig2noise(X, mar, parallel = 1, path = NULL, pb = TRUE, type = 1, eq.dur = FALSE,
#' in.dB = TRUE, before = FALSE, lim.dB = TRUE, bp = NULL, wl = 10)
#' @param X object of class 'selection_table', 'extended_selection_table' or any data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end).
#' @param mar numeric vector of length 1. Specifies the margins adjacent to
#'   the start and end points of selection over which to measure noise.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing). It can also be
#' set globally using the 'parallel' option (see \code{\link{warbleR_options}}).
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used. It can also be
#' set globally using the 'wav.path' option (see \code{\link{warbleR_options}}).
#' @param pb Logical argument to control if progress bar is shown. Default is \code{TRUE}. It can also be
#' set globally using the 'pb' option (see \code{\link{warbleR_options}}).
#' @param type Numeric. Determine the formula to be used to calculate the signal-to-noise ratio (S = signal
#' , N = background noise):
#' \itemize{
#' \item \code{1}: ratio of S mean amplitude envelope to
#'   N mean amplitude envelope (\code{mean(env(S))/mean(env(N))})
#' \item \code{2}: ratio of S amplitude envelope RMS (root mean square) to N amplitude envelope RMS
#'  (\code{rms(env(S))/rms(env(N))})
#' \item \code{3}: ratio of the difference between S amplitude envelope RMS and N amplitude envelope RMS to N amplitude envelope RMS (\code{(rms(env(S)) - rms(env(N)))/rms(env(N))})
#' }
#' @param eq.dur Logical. Controls whether the noise segment that is measured has the same duration
#' than the signal (if \code{TRUE}, default \code{FALSE}). If \code{TRUE} then 'mar' argument is ignored.
#' @param in.dB Logical. Controls whether the signal-to-noise ratio is returned in decibels (20*log10(SNR)).
#' Default is \code{TRUE}.
#' @param before Logical. If \code{TRUE} noise is only measured right before the signal (instead of before and after). Default is \code{FALSE}.
#' @param lim.dB Logical. If \code{TRUE} the lowest signal-to-noise would be limited to -40 dB (if \code{in.dB = TRUE}). This would remove NA's that can be produced when noise segments have a higher amplitude than the signal
#' itself. Default is \code{TRUE}.
#' @param bp Numeric vector of length 2 giving the lower and upper limits of a frequency bandpass filter (in kHz). Default is \code{NULL}.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram for applying bandpass. Default
#'   is 10. Ignored if \code{bp = NULL}. It can also be
#' set globally using the 'wl' option (see \code{\link{warbleR_options}}).
#'  Note that lower values will increase time resolution, which is more important for signal-to-noise ratio calculations.
#' @return Data frame similar to \code{\link{auto_detec}} output, but also includes a new variable
#' with the signal-to-noise values.
#' @export
#' @name sig2noise
#' @details  Signal-to-noise ratio (SNR) is a measure of the level of a desired signal compared to
#'  background noise. The function divides the mean amplitude of the signal by
#'   the mean amplitude of the background noise adjacent to the signal.
#'   A general margin to apply before and after the acoustic signal must
#'   be specified. Setting margins for individual signals that have been
#'   previously clipped from larger files may take some optimization, as
#'   for calls within a larger file that are irregularly separated. When
#'   margins overlap with another acoustic signal nearby, the signal-to-noise
#'   ratio (SNR) will be inaccurate. Any SNR less than or equal to one suggests
#'   background noise is equal to or overpowering the acoustic signal.
#'   \code{\link{snr_spectrograms}} can be used to troubleshoot different noise margins.
#' @examples
#' {
#'   data(list = c("Phae.long1", "lbh_selec_table"))
#'   writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
#'
#'   # specifying the correct margin is important
#'   # use snr_spectrograms to troubleshoot margins for sound files
#'   sig2noise(lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ],
#'     mar = 0.2,
#'     path = tempdir()
#'   )
#'
#'   # this smaller margin doesn't overlap neighboring signals
#'   sig2noise(lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ],
#'     mar = 0.1,
#'     path = tempdir()
#'   )
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) and Grace Smith Vidaurre
#' @references {Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' \href{https://en.wikipedia.org/wiki/Signal-to-noise_ratio}{Wikipedia: Signal-to-noise ratio}
#' }
# last modification on aug-06-2018 (MAS)

sig2noise <- function(X, mar, parallel = 1, path = NULL, pb = TRUE, type = 1, eq.dur = FALSE,
                      in.dB = TRUE, before = FALSE, lim.dB = TRUE, bp = NULL, wl = 10) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(sig2noise)

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

  # if any selections longer than 20 secs stop
  if (any(X$end - X$start > 20)) stop2(paste(length(which(X$end - X$start > 20)), "selection(s) longer than 20 sec"))

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
      stop2("The sound files are not in the working directory")
    } else {
      X <- X[d, , drop = FALSE]
    }
  } else {
    d <- 1:nrow(X)
  }

  # If parallel is not numeric
  if (!is.numeric(parallel)) stop2("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop2("'parallel' should be a positive integer")


  # function to run over single selection
  snr_FUN <- function(y, mar, bp, wl, type, before, in.dB, lim.dB) {
    # Read sound files to get sample rate and length
    r <- warbleR::read_sound_file(X = X, path = path, index = y, header = TRUE)

    # read sample rate
    f <- r$sample.rate

    # set margin to half of signal duration
    if (eq.dur) mar <- (X$end[y] - X$start[y]) / 2

    # reset time coordinates of signals if lower than 0 o higher than duration
    stn <- X$start[y] - mar
    enn <- X$end[y] + mar
    mar1 <- mar

    if (stn < 0) {
      mar1 <- mar1 + stn
      stn <- 0
    }

    mar2 <- mar1 + X$end[y] - X$start[y]

    if (enn > r$samples / f) enn <- r$samples / f

    r <- warbleR::read_sound_file(X = X, path = path, index = y, from = stn, to = enn)

    # add band-pass frequency filter
    if (!is.null(bp)) {
      if (bp[1] == "frange") bp <- c(X$bottom.freq[y], X$top.freq[y])

      r <- seewave::ffilter(r,
        f = f, from = bp[1] * 1000, ovlp = 0,
        to = bp[2] * 1000, bandpass = TRUE, wl = wl,
        output = "Wave"
      )
    }

    # Identify the signal
    signal <- seewave::cutw(r, from = mar1, to = mar2, f = f)

    # Identify areas before and after signal over which to measure noise
    noise1 <- seewave::cutw(r, from = 0, to = mar1, f = f)

    noise2 <- seewave::cutw(r, from = mar2, to = seewave::duration(r), f = f)

    if (type == 1) { # Calculate mean noise amplitude
      if (before) {
        noisamp <- mean(warbleR::envelope(x = noise1))
      } else {
        noisamp <- mean(c(
          warbleR::envelope(x = noise1),
          warbleR::envelope(x = noise2)
        ))
      }

      # Calculate mean signal amplitude
      sigamp <- mean(warbleR::envelope(signal))
    }

    if (type %in% 2:3) { # Calculate mean noise amplitude
      if (before) {
        noisamp <- seewave::rms(warbleR::envelope(x = noise1))
      } else {
        noisamp <- seewave::rms(c(
          warbleR::envelope(x = noise1),
          warbleR::envelope(x = noise2)
        ))
      }

      sigamp <- seewave::rms(warbleR::envelope(x = signal))

      if (type == 3) {
        sigamp <- sigamp - noisamp
      }
    }


    # Calculate signal-to-noise ratio
    snr <- sigamp / noisamp

    # set lowest dB limit
    if (in.dB & lim.dB) snr[snr <= 0] <- 0.01

    if (in.dB) {
      return(20 * log10(snr))
    } else {
      return(snr)
    }
  }

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  # run loop apply function
  SNR_l <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(X), cl = cl, FUN = function(y) {
    snr_FUN(y, mar, bp, wl, type, before, in.dB, lim.dB)
  })

  # Add SNR data to X
  z <- data.frame(X, SNR = unlist(SNR_l))

  # fix extended selection table
  if (is_extended_selection_table(X)) z <- fix_extended_selection_table(X = z, Y = X)

  return(z)
}


##############################################################################################################
#' alternative name for \code{\link{sig2noise}}
#'
#' @keywords internal
#' @details see \code{\link{sig2noise}} for documentation. \code{\link{sig2noise}} will be deprecated in future versions.
#' @export

signal_2_noise <- sig2noise
