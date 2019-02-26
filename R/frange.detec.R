#' Detect frequency range on wave objects
#' 
#' \code{frange.detec} detects the frequency range of acoustic signals on wave objects.
#' @usage frange.detec(wave, wl = 512, fsmooth = 0.1, threshold = 10, 
#' dB.threshold = NULL, wn = "hanning", flim = c(0, 22), bp = NULL, 
#' fast.spec = FALSE, ovlp = 50, pal = reverse.gray.colors.2, 
#'  widths = c(2, 1), main = NULL, plot = TRUE, all.detec = FALSE)
#' @param wave A 'wave' object produced by  \code{\link[tuneR]{readWave}} or similar functions.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512. This is used for calculating the frequency spectrum (using \code{\link[seewave]{meanspec}}) 
#'   and producing the spectrogram (using \code{\link[seewave]{spectro}}, if \code{plot = TRUE}). 
#' @param fsmooth A numeric vector of length 1 to smooth the frequency spectrum with a mean
#'  sliding window in kHz. This help to average amplitude "hills" to minimize the effect of
#'  amplitude modulation. Default is 0.1.
#' @param threshold Amplitude threshold (\%) for frequency range detection. The frequency range (not the cummulative amplitude) is represented as percentage (100\% = highest amplitude). Default is 10. Ignored if 'dB.threshold' is supplied.
#' @param dB.threshold Amplitude threshold for frequency range detection (in dB). The
#' value indicates the decrease in dB in relation to the highest amplitude (e.g. 
#' the peak frequency) in which range will be detected. For instance a 
#' \code{dB.threshold = 20} means that the amplitude threshold would be 20 dB below
#' the highest amplitude. If provided 'threshold' is ignored. Default is \code{NULL}. 
#' Note that the power spectrum is normalized when using a dB scale, so it looks different than the one produced when no dB scale is used (e.g. when using 'threshold' argument).
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options. This is used for calculating the frequency spectrum (using \code{\link[seewave]{meanspec}}) and producing the spectrogram (using \code{\link[seewave]{spectro}}, if \code{plot = TRUE}). 
#' @param flim A numeric vector of length 2 for the frequency limit of 
#'   the spectrogram (in kHz), as in \code{\link[seewave]{spectro}}. Default is c(0, 22).
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz) or "frange" to indicate that values in 'bottom.freq' 
#'   and 'top.freq' columns will be used as bandpass limits. Default is c(0, 22).
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast.spec' spectograms. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}} offer 
#' decreasing darkness levels. 
#' @param pal Color palette function for spectrogram. Default is reverse.gray.colors.2. See 
#' \code{\link[seewave]{spectro}} for more palettes. Palettes as \code{\link[monitoR]{gray.2}} may work better when \code{fast.spec = TRUE}.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 50. This is used for calculating the frequency spectrum (using \code{\link[seewave]{meanspec}}) and producing the spectrogram (using \code{\link[seewave]{spectro}}, if \code{plot = TRUE}). 
#' @param widths Numeric vector of length 2 to control the relative widths of the spectro (first element) and spectrum (second element).
#' @param main  Character vector of length 1 specifying the plot title. Default is \code{NULL}.
#' @param plot Logical. Controls whether an image file is produced for each selection (in the 
#' working directory). Default is \code{TRUE}.
#' @param all.detec Logical. If \code{TRUE} returns the start and end of all detected amplitude
#' "hills". Otherwise only the range is returned. Default is \code{FALSE}. 
#' @return A data frame with 2 columns for low and high frequency values. A plot is produced (in the graphic device) if \code{plot = TRUE} (see details).
#' @export
#' @name frange.detec
#' @details This functions aims to automatize the detection of frequency ranges. The frequency range is calculated as follows:
#' \itemize{  
#'  \item bottom.freq = the start frequency of the amplitude 'hill' containing the highest amplitude at the given threshold.  
#'  \item top.freq = the end frequency of the amplitude 'hill' containing the highest amplitude at the given threshold.
#'   }
#'   If \code{plot = TRUE} a graph including a spectrogram and a frequency spectrum is 
#'   produced in the graphic device. The graph would include gray areas in the frequency ranges exluded by the bandpass ('bp' argument), dotted lines highlighting the detected range.
#' @seealso \code{\link{frange}}, \code{\link{autodetec}}
#' @examples
#' {
#' data(tico)
#' frange.detec(wave = tico, wl = 512, fsmooth = 0.01, threshold = 1, bp = c(2, 8),
#'  widths = c(4, 2))
#' 
#' data(sheep)
#' frange.detec(wave = sheep, wl = 512, fsmooth = 0.2, threshold = 50, bp = c(0.3, 1), 
#' flim = c(0, 1.5), pal = reverse.heat.colors, main = "sheep")
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on apr-28-2017 (MAS)

frange.detec <- function(wave, wl = 512, fsmooth = 0.1, threshold = 10, dB.threshold = NULL, wn = "hanning", flim = c(0, 22), bp = NULL, fast.spec = FALSE, ovlp = 50, pal = reverse.gray.colors.2, widths = c(2, 1), main = NULL, plot = TRUE, all.detec = FALSE)
{
  # close screens
  on.exit(invisible(close.screen(all.screens = TRUE)))
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(frange.detec)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]
  
  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]
  
  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]
  
  # set options left
  if (length(opt.argms) > 0)
    for (q in 1:length(opt.argms))
      assign(names(opt.argms)[q], opt.argms[[q]])
  
  frng <- frd_wrblr_int(wave = wave, wl = wl, fsmooth = fsmooth, threshold = threshold, dB.threshold = dB.threshold, wn = wn, bp = bp, ovlp = ovlp)
  
  if (plot)
    frd_plot_wrblr_int(wave = wave, detections = frng, wl = wl, wn = wn, flim = flim, bp = bp, fast.spec = fast.spec, ovlp = ovlp, pal = pal, widths = widths, main = main, all.detec = all.detec)   

    # return low and high freq
 if (all.detec) return(frng$detections) else return(frng$frange)
  
 
}


##############################################################################################################
#' alternative name for \code{\link{frange.detec}}
#'
#' @keywords internal
#' @details see \code{\link{frange.detec}} for documentation. \code{\link{frange.detec}} will be deprecated in future versions.
#' @export

freq_range_detec <- frange.detec
