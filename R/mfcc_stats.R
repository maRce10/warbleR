#' Calculate descriptive statistics on Mel-frequency cepstral coefficients
#'
#' \code{mfcc_stats} calculates descriptive statistics on Mel-frequency cepstral coefficients and its derivatives.
#' @param X 'selection_table', 'extended_selection_table' or data frame with the following columns: 1) "sound.files": name of the sound
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end":
#' end time of selections.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two
#' consecutive windows. Internally this is used to set the 'hoptime' argument in \code{\link[tuneR]{melfcc}}. Default is 50.
#' @param wl A numeric vector of length 1 specifying the spectrogram window length. Default is 512. See 'wl.freq' for setting windows length independently in the frequency domain.
#' @param bp A numeric vector of length 2 for the lower and upper limits of a
#'   frequency bandpass filter (in kHz) or "frange" (default) to indicate that values in minimum of 'bottom.freq'
#'   and maximum of 'top.freq' columns will be used as bandpass limits.
#' @param path Character string containing the directory path where the sound files are located.
#' @param numcep Numeric vector of length 1 controlling the number of cepstra to
#' return (see \code{\link[tuneR]{melfcc}}).
#' @param nbands Numeric vector of length 1 controlling the number of warped spectral bands to use (see \code{\link[tuneR]{melfcc}}). Default is 40.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param ... Additional parameters to be passed to \code{\link[tuneR]{melfcc}}.
#' @return A data frame containing the descriptive statistics for each of the Mel-frequency
#' cepstral coefficients (set by 'numcep' argument). See details.
#' @export
#' @name mfcc_stats
#' @details The function calculates descriptive statistics on Mel-frequency cepstral coefficients (MFCCs) for each of the signals (rows) in a selection
#' data frame. The descriptive statistics are: minimum, maximum, mean, median, skewness, kurtosis and
#' variance.
#' It also returns the mean and variance for the first and second derivatives of the coefficients. These parameters are commonly used in acoustic signal processing and detection (e.g. Salamon et al 2014).
#' @seealso \code{\link{fix_wavs}}, \code{\link{remove_silence}},  \code{\link{spectro_analysis}}
#' @examples{
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' # run function
#' mel_st <- mfcc_stats(X = lbh_selec_table, pb = FALSE, path = tempdir())
#'
#' head(mel_st)
#'
#' # measure 12 coefficients
#' mel_st12 <- mfcc_stats(X = lbh_selec_table, numcep = 12, pb = FALSE, path = tempdir())
#'
#'  head(mel_st)
#' }
#' @references 
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#'
#' Lyon, R. H., & Ordubadi, A. (1982). Use of cepstra in acoustical signal analysis. Journal of Mechanical Design, 104(2), 303-306.
#'
#' Salamon, J., Jacoby, C., & Bello, J. P. (2014). A dataset and taxonomy for urban sound research. In Proceedings of the 22nd ACM international conference on Multimedia. 1041-1044.
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

mfcc_stats <- function(X, ovlp = 50, wl = 512, bp = "frange", path = NULL,
                       numcep = 25, nbands = 40, parallel = 1, pb = TRUE, ...) {
  # error message if wavethresh is not installed
  if (!requireNamespace("Sim.DiffProc", quietly = TRUE)) {
    stop2("must install 'Sim.DiffProc' to use this function")
  }

  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(mfcc_stats)

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

  # bp checking
  if (bp[1] != "frange") {
    if (!is.vector(bp)) {
      stop2("'bp' must be a numeric vector of length 2 or 'frange'")
    } else {
      if (!length(bp) == 2) stop2("'bp' must be a numeric vector of length 2 or 'frange'")
    }
  } else {
    if (!any(names(X) == "bottom.freq") & !any(names(X) == "top.freq")) stop2("'bp' = 'frange' requires bottom.freq and top.freq columns in X")
    if (any(is.na(c(X$bottom.freq, X$top.freq)))) stop2("NAs found in bottom.freq and/or top.freq")
    if (any(c(X$bottom.freq, X$top.freq) < 0)) stop2("Negative values found in bottom.freq and/or top.freq")
    if (any(X$top.freq - X$bottom.freq < 0)) stop2("top.freq should be higher than bottom.freq")

    bp <- c(min(X$bottom.freq), max(X$top.freq))
  }

  if (!is_extended_selection_table(X)) {
    # return warning if not all sound files were found
    fs <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)
    if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) {
      warning2(x = paste(
        length(unique(X$sound.files)) - length(unique(X$sound.files[(X$sound.files %in% fs)])),
        "sound files file(s) not found"
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

  # If parallel is not numeric
  if (!is.numeric(parallel)) stop2("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop2("'parallel' should be a positive integer")

  mfcc_FUN <- function(i, X, bp, wl, numcep, nbands) {
    # read wave file
    r <- warbleR::read_sound_file(X = X, path = path, index = i)

    # set bandpass
    if (!is.null(bp)) {
      if (bp[1] == "frange") bp <- c(X$bottom.freq[i], X$top.freq[i])
    }

    b <- bp

    # in case bp its higher than can be due to sampling rate
    if (b[2] > floor(r@samp.rate / 2000)) b[2] <- floor(r@samp.rate / 2000)

    # add a bit above and below to ensure range limits are included
    bpfr <- b
    bpfr <- bpfr + c(-0.2, 0.2)
    if (bpfr[1] < 0) bpfr[1] <- 0
    if (bpfr[2] > floor(r@samp.rate / 2000)) bpfr[2] <- floor(r@samp.rate / 2000)

    # measure MFCCs
    m <- try(melfcc(r,
      wintime = wl / r@samp.rate, hoptime = wl / r@samp.rate * (1 - (ovlp / 100)),
      numcep = numcep, nbands = nbands, minfreq = bpfr[1] * 1000, maxfreq = bpfr[2] * 1000, ...
    ), silent = TRUE)

    clm.nms <- paste(rep(c("min", "max", "median", "mean", "var", "skew", "kurt"), each = numcep), paste0("cc", 1:numcep), sep = ".")

    # if cepstral coefs were calculated
    if (!is(m, "try-error")) {
      # put them in a data frame
      outdf <- data.frame(t(c(
        apply(m, 2, min), apply(m, 2, max), apply(m, 2, stats::median), apply(m, 2, mean),
        apply(m, 2, stats::var), apply(m, 2, Sim.DiffProc::skewness), apply(m, 2, Sim.DiffProc::kurtosis)
      )), stringsAsFactors = FALSE)

      # name columns
      names(outdf) <- clm.nms

      # measure MFCC first and second derivatives var and mean
      m2 <- deltas(m)
      m3 <- deltas(m2)
      vm.d <- c(mean.d1.cc = mean(c(m2)), var.d1.cc = stats::var(c(m2)), mean.d2.cc = mean(c(m3)), var.d2.cc = stats::var(c(m3)))

      out.df <- cbind(X[i, c("sound.files", "selec")], outdf, t(vm.d), stringsAsFactors = FALSE)
    } else {
      out.df <- data.frame(X[i, c("sound.files", "selec")], t(rep(NA, length(clm.nms) + 4)))

      names(out.df)[-c(1:2)] <- c(clm.nms, "mean.d1.cc", "var.d1.cc", "mean.d2.cc", "var.d2.cc")
    }

    return(out.df)
  }

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  # run loop apply function
  ccs <- .pblapply(pbar = pb, X = 1:nrow(X), cl = cl, message = "computing MFCC stats", total = 1, FUN = function(i) {
    mfcc_FUN(i = i, X = X, bp, wl = wl, numcep = numcep, nbands = nbands)
  })

  # put results in a single data frame
  ccs <- do.call(rbind, ccs)

  # fix row names
  row.names(ccs) <- 1:nrow(ccs)

  # convert to numeric in case there is any non-numeric value
  # if(anyNA(ccs)){
  ccs$sound.files <- X$sound.files
  ccs$selec <- X$selec
  ccs[, -c(1, 2)] <- data.frame(apply(ccs[, -c(1, 2)], 2, as.numeric))
  # }

  return(ccs)
}
