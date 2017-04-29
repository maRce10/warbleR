#' Detect frequency range on wave objects
#' 
#' \code{frange.detec} Detects the frequency range of acoustic signals in wave objects.
#' @usage frange.detec(wave, wl = 512, fsmooth = 0.1, threshold = 10, wn = "hanning",
#'  flim = c(0, 22), bp = NULL, fast.spec = FALSE, ovlp = 50, pal = reverse.gray.colors.2, 
#'  widths = c(2, 1), min.range = NULL, main = NULL, plot = TRUE)
#' @param wave A 'wave' object produced by  \code{\link[tuneR]{readWave}} or similar functions.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512. This is used for calculating the frequency spectrum (using \code{\link[seewave]{meanspec}}) 
#'   and producing the spectrogram (using \code{\link[seewave]{spectro}}, if \code{plot = TRUE}). 
#' @param fsmooth A numeric vector of length 1 to smooth the frequency spectrum with a mean
#'  sliding window in kHz. This help to average amplitude "hills" to minimize the effect of
#'  amplitude modulation.
#' @param threshold Amplitude threshold (\%) for fundamental frequency and 
#'   dominant frequency detection. Default is 10.
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options. This is used for calculating the frequency spectrum (using \code{\link[seewave]{meanspec}}) and producing the spectrogram (using \code{\link[seewave]{spectro}}, if \code{plot = TRUE}). 
#' @param flim A numeric vector of length 2 for the frequency limit of 
#'   the spectrogram (in kHz), as in \code{\link[seewave]{spectro}}. Default is c(0, 22).
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz) or "frange" to indicate that values in low.f 
#'   and high.f columns will be used as bandpass limits. Default is c(0, 22).
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast.spec' spectograms. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}} offer 
#' decreasing darkness levels. THIS IS STILL BEING TESTED.
#' @param pal Color palette function for spectrogram. Default is reverse.gray.colors.2. See 
#' \code{\link[seewave]{spectro}} for more palettes. Palettes as \code{\link[monitoR]{gray.2}} may work better when \code{fast.spec = TRUE}.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 50. This is used for calculating the frequency spectrum (using \code{\link[seewave]{meanspec}}) and producing the spectrogram (using \code{\link[seewave]{spectro}}, if \code{plot = TRUE}). 
#' @param widths Numeric vector of length 2 to control the relative widths of the spectro (first element) and spectrum (second element).
#' @param min.range Numeric vector of length 1 specifying the minimum frequency range expected (in kHz). This
#' is used to find "a higher" high frequency. Default is \code{NULL}. 
#' @param main  Character vector of length 1 specifying the plot title. Default is \code{NULL}.
#' @param plot Logical. Controls whether a plot is produced. Default is \code{TRUE}.
#' @return A data frame with 2 columns for low and high frequency values. A plot is also produced
#' in the graphic devide if \code{plot = TRUE} (see details).
#' @export
#' @name frange.detec
#' @details This functions aims to automatize the detection of frequency ranges. If \code{plot = TRUE} 
#' a graph including a spectrogram and a frequency spectrum is produced. The graph would include 
#' gray areas in the frequency ranges exluded by the bandpass ('bp' argument), Dotted lines
#' highlight the detected range.
#' @seealso \code{\link{autodetec}}
#' @examples
#' \dontrun{
#' data(tico)
#' frange.detec(wave = tico, wl = 512, fsmooth = 0.01, threshold = 1, bp = c(2, 8),
#'  widths = c(4, 2))
#' 
#' data(sheep)
#' frange.detec(wave = sheep, wl = 512, fsmooth = 0.2, threshold = 50, bp = c(0.3, 1), 
#' flim = c(0, 1.5), pal = reverse.heat.colors, main = "sheep")
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on apr-28-2017 (MAS)

frange.detec <- function(wave, wl = 512, fsmooth = 0.1, threshold = 10, wn = "hanning", flim = c(0, 22), bp = NULL, fast.spec = FALSE, ovlp = 50, pal = reverse.gray.colors.2, widths = c(2, 1), min.range = NULL, main = NULL, plot = TRUE)
{
  # get sampling rate
  f <-  wave@samp.rate
  
  # fix flim
  flim <- c(0, floor(f/2000))
  if(flim[2] > ceiling(f/2000) - 1) flim[2] <- ceiling(f/2000) - 1 
  
  
  # mean spectrum
  spc <- meanspec(wave, plot = F, wl = wl, f = f, wn = wn, ovlp = ovlp)
  
  # get frequency windows length for smoothing
  step <- wave@samp.rate/wl/1000
  
  fsmooth <- fsmooth/step
  
  # number of samples
  n <- nrow(spc)
  
  # smoothing parameter
  FWL <- fsmooth - 1
  
  # smooth 
  z <- apply(as.matrix(1:(n - FWL)), 1, function(y) sum(spc[y:(y + 
                                                                 FWL), 2]))
  zf <- seq(min(spc[,1]), max(spc[,1]), length.out = length(z))
  
  if(!is.null(bp)) { 
    #remove range outsde bp
    z <- z[zf > bp[1] & zf < bp[2]]
    zf <- zf[zf > bp[1] & zf < bp[2]]
    
    # make minimum amplitude 0
    z <- z - min(z)
    z[z < 0] <- 0}
  
  # normalize amplitude from 0 to 1
  z <- z/max(z)
  z1 <- rep(0, length(z))
  
  #get freqs crossing threshold
  z1[z > threshold/100] <- 1 
  z2 <- z1[2:length(z1)] - z1[1:(length(z1) - 1)]
  z2 <- c(0, z2)
  
  start <- zf[z2 == 1]
  end <- zf[z2 == -1]
  
  # set low and hi f to flim/bp if not detected
  if(!is.null(bp)) { 
    min.start <- ifelse(length(start) == 0 || is.infinite(min(start)), yes = bp[1], no = min(start))
    max.end <- ifelse(length(end) == 0 || is.infinite(min(end)), yes = bp[2], no = max(end))
    
    if(!is.null(min.range) && max.end - min.start < min.range) max.end <- bp[2]
  } else {
    min.start <- ifelse(length(start) == 0 || is.infinite(min(start)), yes = flim[1], no = min(start))
    max.end <- ifelse(length(end) == 0 || is.infinite(min(end)), yes = flim[2], no = max(end))
    
    if(!is.null(min.range) & max.end - min.start < min.range) max.end <- flim[2]
  }
  
  if(plot)
  {
    # split screen
    m <- rbind(c(0, widths[1]/sum(widths), 0, 0.93), #1
               c(widths[1]/sum(widths), 1, 0 , 0.93),
               c(0, 1,  0.93 , 1)) #3 
    
    invisible(close.screen(all.screens = TRUE))  
    split.screen(m)
    screen(1)
    par(mar = c(3.4, 3.4, 0.5, 0))
    
    # create spectro
    spectro2(wave = wave, f = f, flim = flim, fast.spec = fast.spec, palette = pal, ovlp = ovlp, wl = wl, grid = F, tlab = "", flab = "")
    
    #add line highlighting freq range
    abline(h = c(min.start, max.end), col = "#80C3FF", lty = 3, lwd = 3.3)
    
    # add axis labels
    mtext(side = 1, text = "Time (s)", line = 2.3)
    mtext(side = 2, text = "Frequency (kHz)", line = 2.3)
    
    #second plot
    screen(2)
    par(mar = c(3.4, 0, 0.5, 0.5))
    
    plot(z, zf, type = "l", ylim = flim, yaxs = "i", xaxs = "i", yaxt = "n", xlab = "", col = "white", xaxt = "n")
    
    # add axis& labels
    axis(1, at = seq(0.2, 1, by = 0.4))
    mtext(side = 1, text = "Amplitude (%)", line = 2.3)
    
    # fix amplitude values to close polygon (just for ploting)
    z3 <- c(0, z, 0)
    
    if(!is.null(bp)) zf3 <- c(bp[1], zf, bp[2]) else zf3 <- c(flim[1], zf, flim[2])
    
    # plot amplitude values curve
    polygon(cbind(z3, zf3), col= adjustcolor("#4D69FF", 0.9))
    
    # add border line
    points(z3, zf3, type = "l", col = adjustcolor("black", 0.5))
    
    # add bacground color
    rect(xleft = 0, ybottom = flim[1], xright = 1, ytop = flim[2], col = adjustcolor("#4D69FF", 0.05))
    
    # add gray boxes in filtered out freq bands
    if(!is.null(bp))
    {  rect(xleft = 0, ybottom = bp[2], xright = 1, ytop = flim[2], col = adjustcolor("gray", 0.5)) 
      rect(xleft = 0, ybottom = flim[1], xright = 1, ytop = bp[1], col = adjustcolor("gray", 0.5))
    }
    
    # add line to highligh freq range
    abline(v = threshold/100, col = adjustcolor("blue4", 0.7), lty = 3, lwd = 2.3)
    abline(h = c(min.start, max.end), col = "#80C3FF", lty = 3, lwd = 3.3)
    
    if(!is.null(main))
    {
      screen(3)
      par( mar = rep(0, 4))
      plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      
      text(x = 0.5, y = 0.35, labels = main, font = 2)  
    }
    
  }
  # return low and high freq
  return(data.frame(low.f = min.start, high.f = max.end))
  
  # close screens
  on.exit(invisible(close.screen(all.screens = TRUE)))
}
