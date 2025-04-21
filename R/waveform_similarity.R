#' Pairwise similarity of waveforms
#'
#' \code{waveform_similarity} estimates the similarity of two sound waveforms
#' @param  X 'selection_table', 'extended_selection_table' or data frame containing columns for sound files (sound.files),
#' selection number (selec), and start and end time of signal (start and end).
#' All selections must have the same sampling rate.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default
#' is 512. Only used when applying a bandpass filter (\code{bp != NULL}).
#' @param bp A numeric vector of length 2 for the lower and upper limits of a
#'   frequency bandpass filter (in kHz). If columns for bottom and top frequency ('bottom.freq' and 'top.freq') are supplied "pairwise.freq.range" can be used (default). If so, the lowest values in 'bottom.freq'
#'   and the highest values in 'top.freq' for the selections involved in a pairwise comparison will be used as bandpass limits.
#' @param ovlp Numeric vector of length 1 specifying the percentage of overlap between two
#' consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70. High values of overlap
#' slow down the function. Only used when applying a bandpass filter (\code{bp != NULL}).
#' @param sim.method A character string specifying the similarity method. Two option are available:
#' \itemize{
#'    \item \code{"correlation"}: calculates the Pearson correlation between the waveforms of the two signals. Higher values indicate higher similarity.
#'    \item \code{"DTW"}: calculates the Dynamic Time Warping distance between the waveforms of the two signals. Lower values indicate higher similarity.
#'    }
#' @param type A character string specifying the approach for estimating similarity. Two option are available:
#' \itemize{
#'    \item \code{"standard"}: estimates the similarity between the two waveforms with a single point estimate (e.g. the correlation or DTW distance between them).
#'    \item \code{"sliding"}: estimates the similarity between the two waveforms by calculating the correlation or DTW distance at each "sliding" step of the spectrogram of the shortest selection over the longest one. This approach is more computationally intensive but might be more appropriate when comparing sounds with large differences in duration or when the appropriate alignment of the waveforms is hard to determine.
#'    }
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param n Numeric. Number of values used to represent each waveform. Default is 100. Note that both waveforms are forced to have the same length which is done by interpolating amplitude values using the function \code{\link[stats:approxfun]{approx}}.
#' @return The function returns a matrix with the similarity for each pairwise comparison.
#' @export
#' @name waveform_similarity
#' @details This function calculates the pairwise similarity of multiple waveforms from annotations referenced in a selection table. Useful for the analysis of acoustic fine structure (e.g. Prior et al. 2018). Waveforms are forced to have the same length (see argument 'n'). This is done by interpolating amplitude values using the function \code{\link[stats:approxfun]{approx}}. The function can be used to compare waveforms using either the Pearson correlation coefficient or the Dynamic Time Warping distance. The latter is a measure of similarity between two sequences that may vary in the timing of occurrence of the changes in amplitude. 
#' Make sure all sound files have the same sampling rate (can be checked with \code{\link{check_sels}} or \code{\link{check_sound_files}}). Comparison can be done with a single point estimate (e.g. the correlation or DTW distance between them) or by calculating the correlation or DTW distance with a sliding window approach. This approach is more computationally intensive but might be more appropriate when comparing sounds with large differences in duration or when the appropriate alignment of the waveforms is hard to determine.
#'
#' @examples
#' {
#'   # load data
#'   data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#'
#'   # save sound files
#'   writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#'   writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#'   writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#'   writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#'   # run waveform correlation
#'   wcor <- waveform_similarity(X = lbh_selec_table, path = tempdir())
#' }
#' @seealso \code{\link{cross_correlation}}, \code{\link{spectro_analysis}}, \code{\link{freq_DTW}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references 
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' Müller, M. (2007). Dynamic time warping. Information retrieval for music and motion, 69-84.
#' 
#' Prior, N. H., Smith, E., Lawson, S., Ball, G. F., & Dooling, R. J. (2018). Acoustic fine structure may encode biologically relevant information for zebra finches. Scientific reports, 8(1), 6212.

waveform_similarity <-
  function(X = NULL,
           wl = 512,
           bp = "pairwise.freq.range",
           ovlp = 70,
           sim.method = "correlation",
           type = "standard",
           parallel = 1,
           path = NULL,
           pb = TRUE,
           n = 100)
  {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(waveform_similarity)

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
  } else if (!dir.exists(path) & !warbleR::is_extended_selection_table(X)) {
    stop2("'path' provided does not exist")
  } else {
    path <- normalizePath(path)
  }

  # if X is not a data frame
  if (!any(is.data.frame(X), warbleR::is_selection_table(X), warbleR::is_extended_selection_table(X))) stop2("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")

  # if is extended all should have the same sampling rate
  if (warbleR::is_extended_selection_table(X) & length(unique(attr(X, "check.results")$sample.rate)) > 1) stop2("all wave objects in the extended selection table must have the same sampling rate (they can be homogenized using resample_est_waves())")

  # if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop2("NAs found in start and/or end")

  # stop if only 1 selection
  if (nrow(X) == 1) stop2("you need more than one selection to do waveform-correlation")

  # bp needed when no bottom and top freq
  if (!is.null(bp))
  if (bp[1] == "pairwise.freq.range" & is.null(X$bottom.freq)) stop2("'bp' must be supplied when no frequency range columns are found in 'X' (bottom.freq & top.freq)")

  # if wl is not vector or length!=1 stop
  if (!is.numeric(wl)) {
    stop2("'wl' must be a numeric vector of length 1")
  } else {
    if (!is.vector(wl)) {
      stop2("'wl' must be a numeric vector of length 1")
    } else {
      if (!length(wl) == 1) stop2("'wl' must be a numeric vector of length 1")
    }
  }

  # if ovlp is not vector or length!=1 stop
  if (!is.numeric(ovlp)) {
    stop2("'ovlp' must be a numeric vector of length 1")
  } else {
    if (!is.vector(ovlp)) {
      stop2("'ovlp' must be a numeric vector of length 1")
    } else {
      if (!length(ovlp) == 1) stop2("'ovlp' must be a numeric vector of length 1")
    }
  }

  # If parallel is not numeric
  if (!is.numeric(parallel)) stop2("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop2("'parallel' should be a positive integer")

  # check sampling rate is the same for all selections if not a selection table
  if (warbleR::is_extended_selection_table(X) & length(unique(attr(X, "check.results")$sample.rate)) > 1) stop2("sampling rate must be the same for all selections")

  # add selection id column to X
  X$selection.id <- paste(X$sound.files, X$selec, sep = "-")

  # generate all possible combinations of selections, keep one with the original order of rows to create cor.table output
  wv.cmbs <- t(combn(X$selection.id, 2))

  # shuffle index so are not compared in sequence, which makes progress bar more precise when some selections are much longer than others
  ord.shuf <- sample(1:nrow(wv.cmbs))
  
  wv.cmbs <- wv.cmbs[ord.shuf, , drop = FALSE]
  
  
  # create function to calculate correlation between 2 spectrograms
  WC_FUN <- function(wv1, wv2, b = bp, tp = type, pt = path, sm = sim.method, wl = wl, ovl = ovlp, n = n) {
    
    w1 <- read_sound_file(X = X, index = which(X$selection.id ==
                                                 wv1), path = path)
    w2 <- read_sound_file(X = X, index = which(X$selection.id ==
                                                 wv2), path = path)
    
    # add band-pass frequency filter
    if (!is.null(b)) {
      w1 <- seewave::ffilter(w1,
                            f = w1@samp.rate, from = b[1] * 1000, ovlp = ovl,
                            to = b[2] * 1000, bandpass = TRUE, wl = wl,
                            output = "Wave"
      )
      w2 <- seewave::ffilter(w2,
                             f = w2@samp.rate, from = b[1] * 1000, ovlp = ovl,
                             to = b[2] * 1000, bandpass = TRUE, wl = wl,
                             output = "Wave"
      )
    } 
    
    # extract waveforms    
    w1 <- w1@left  
    w2 <- w2@left      
    
    # normalize
    w1 <- w1/max(abs(w1))
    w2 <- w2/max(abs(w2))
    
    # make same length if (length(s1) != length(s2))
    w1 <- approx(w1, n = n)$y
    w2 <- approx(w2, n = n)$y
    
    
    if (tp == "standard"){
      
      if (sm == "correlation") {
        sim <- cor(w1, w2)
      } 
      
      if (sm == "DTW") {
       sim <- dtw::dtwDist(mx = rbind(w1, w2))[1, 2]
      }
    }
    
    if (tp == "sliding"){
      
      # duplicate 1
      w1 <- rep(w1, 2)
      
      # run similarity over longer vector
      sims <- vapply(seq_len(length(w1) - length(w2)), function(x) {
        segment <- w1[x:min(c(x + length(w2) - 1), length(w1))]
        
        # run correlation
        if (sm == "correlation") {
          out <- cor(segment, w2)
        }
        
        # run dynamic time warping
        if (sm == "DTW") {
          out <- dtw::dtwDist(mx = rbind(w2, segment))[1, 2]
        }
        
        return(out)
      }, FUN.VALUE = numeric(1))
      
      if (sm == "correlation") 
        sim <- max(sims)
      
      if (sm == "DTW") 
        sim <- min(sims)
    }  
      
      return(sim)  
    }
    
  # run cross-correlation
  # set parallel cores
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }
  
  # get correlation
  wv_sims <- .pblapply(pbar = pb, X = 1:nrow(wv.cmbs), cl = cl, message = "running cross-correlation", current = 1, FUN = function(j, BP = bp) {
    
    if (!is.null(bp))
    if (BP[1] == "pairwise.freq.range") {
      BP <- c(min(X$bottom.freq[X$selection.id %in% wv.cmbs[j, ]]), max(X$top.freq[X$selection.id %in% wv.cmbs[j, ]]))
    }

    # get cross correlation
    WC_FUN(wv1 = wv.cmbs[j, 1], wv2 = wv.cmbs[j, 2], b = BP, tp = type, sm = sim.method, wl = wl, ovl = ovlp, n = n)
  })

  wv_sims <- unlist(wv_sims)

  # order as originally
  wv_sims <- wv_sims[order(ord.shuf)]

    # create a similarity matrix to return results
    mat <- matrix(nrow = nrow(X), ncol = nrow(X))
    
    if (sim.method == "correlation") {
      mat[] <- 1
    } else {
      mat[] <- 0
    }
    
    # add names to columns and rows
    colnames(mat) <- rownames(mat) <- X$selection.id

    # add similarity values to output matrix
    mat[lower.tri(mat, diag = FALSE)] <- wv_sims
    mat <- t(mat)
    mat[lower.tri(mat, diag = FALSE)] <- wv_sims

    return(mat)
}
