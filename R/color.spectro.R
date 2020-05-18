#' Highlight spectrogram regions
#' 
#' \code{color.spectro} highlights spectrogram regions specified by users
#' @usage color.spectro(wave, wl = 512, wn = "hanning", ovlp = 70,
#' dB = "max0", collevels = NULL, selec.col = "red2", col.clm = NULL, 
#' base.col = "black", bg.col = "white", strength = 1, 
#' cexlab = 1, cexaxis = 1, tlab = "Time (s)", flab = "Frequency (kHz)", 
#' title = NULL, axisX = TRUE, axisY = TRUE, flim = NULL, 
#' rm.zero = FALSE, X = NULL, fast.spec = FALSE, t.mar = NULL, 
#' f.mar = NULL, interactive = NULL, add = FALSE) 
#' @param wave A 'wave' object produced by  \code{\link[tuneR]{readWave}} or similar functions.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram. Default 
#'   is 512.
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param ovlp Numeric vector of length 1 specifying the percent overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70.
#' @param dB Character vector of length 1 controlling the amplitude weights as in 
#' \code{\link[seewave]{spectro}}. Default is 'max0'.
#' @param collevels Numeric. Levels used to partition amplitude range as in \code{\link[seewave]{spectro}}.
#' Default is \code{NULL}.
#' @param selec.col Character vector of length 1 specifying the color to be used to highlight selection. 
#' See 'col.clm' for specifying unique colors for each selection. Default is 'red2'. Ignored if 'col.cm' 
#' and 'X' are provided.
#' @param col.clm Character vector of length 1 indicating the name of the column in 'X' that contains the
#' color names for each selection. Ignored if \code{X == NULL} or \code{interactive != NULL}. Default is \code{NULL}.
#' @param base.col Character vector of length 1 specifying the color of the background spectrogram. 
#' Default is 'black'.
#' @param bg.col Character vector of length 1 specifying the background color for both base
#' and highlighted spectrograms. Default is 'white'.
#' @param strength Numeric vector of length 1 controlling the strength of the highlighting color (actually how many times it is repeated in the internal color palette). Must be a positive integer. Default is 1.
#' @param cexlab Numeric vector of length 1 specifying the relative size of axis 
#'   labels. See \code{\link[seewave]{spectro}}. Default is 1.
#' @param cexaxis Numeric vector of length 1 specifying the relative size of axis. See 
#' \code{\link[seewave]{spectro}}. Default is 1.
#' @param tlab Character vector of length 1 specifying the label of the time axis. 
#' @param flab Character vector of length 1 specifying the label of the frequency axis. 
#' @param title Logical argument to add a title to individual spectrograms. 
#'   Default is \code{TRUE}.
#' @param axisX Logical to control whether time axis is plotted. Default is \code{TRUE}.   
#' @param axisY Logical to control whether frequency axis is plotted. Default is \code{TRUE}.   
#' @param flim A numeric vector of length 2 for the frequency limit (in kHz) of 
#'   the spectrogram, as in \code{\link[seewave]{spectro}}. Default is \code{NULL}. 
#' @param rm.zero Logical indicated if the 0 at the start of the time axis should be removed. Default is \code{FALSE}.
#' @param  X Optional. Data frame containing columns for start and end time of signals ('start' and 'end') and low and high frequency ('bottom.freq' and 'top.freq'). 
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast' spectrograms. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}} offer 
#' decreasing darkness levels. 
#' @param t.mar Numeric vector of length 1. Specifies the margins adjacent to the start and end points to be added when highlighting selection. Default is \code{NULL}.
#' @param f.mar Numeric vector of length 1. Specifies the margins adjacent to the low and high frequencies to be added when highlighting selection. Default is \code{NULL}.
#' @param interactive Numeric. Allow user to interactively select the signals to be highlighted by clicking 
#' on the graphic device. Users must select the opposite corners of a square delimiting the spectrogram region
#' to be highlighted. Controls the number of signals that users would be able to select (2 clicks per signal).   
#' @param add Logical. If \code{TRUE} new highlighting can be applied to the current plot (which means
#'  that the function with \code{add = FALSE} should be run first). Default is \code{FALSE}.
#' @return A plot is produced in the graphic device.
#' @family spectrogram creators
#' @seealso \code{\link{trackfreqs}} for creating spectrograms to visualize 
#'   frequency measurements by \code{\link{specan}}, \code{\link{snrspecs}} for 
#'   creating spectrograms to optimize noise margins used in \code{\link{sig2noise}}
#' @export
#' @name color.spectro
#' @details This function highlights regions of the spectrogram with different colors. The regions to be 
#' highlighted can be provided in a selection table (as the example data 'lbh_selec_table') or interactively ('interactive' argument).
#' @examples
#' \dontrun{ 
#' data(list = c("Phae.long1", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) #save sound files 
#' 
#'  # subset selection table
#'  st <- lbh_selec_table[lbh_selec_table$sound.files == "Phae.long1.wav",]
#'  
#'  # read wave file as an R object
#'  sgnl <- tuneR::readWave(file.path(tempdir(), st$sound.files[1]))
#'  
#'  # create color column
#'  st$colors <- c("red2", "blue", "green")
#'  
#'  # highlight selections
#'  color.spectro(wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-40, 0, 5), 
#'  dB = "B", X = st, col.clm = "colors", base.col = "skyblue",  t.mar = 0.07, f.mar = 0.1, 
#'  interactive = NULL)
#'  
#'  # interactive (selected manually: you have to select them by clicking on the spectrogram)
#'  color.spectro(wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-40, 0, 5),
#'   dB = "B", col.clm = "colors", t.mar = 0.07, f.mar = 1, interactive = 2)
#' }
#' 
#' @references {Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.}
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com}) and Grace Smith Vidaurre
#last modification on jul-5-2016 (MAS)

color.spectro <- function(wave, wl = 512, wn = "hanning", ovlp = 70,
                          dB = "max0", collevels = NULL, selec.col = "red2", col.clm = NULL, 
                          base.col = "black", bg.col = "white", strength = 1, cexlab = 1, cexaxis = 1, tlab = "Time (s)", 
                          flab = "Frequency (kHz)", title = NULL, axisX = TRUE, axisY = TRUE, 
                          flim = NULL, rm.zero = FALSE, X = NULL, fast.spec = FALSE, t.mar = NULL, f.mar = NULL,
                          interactive = NULL, add = FALSE) 
{
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(color.spectro)
  
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
  
  # error if dB wrong
  if (!is.null(dB) && all(dB != c("max0", "A", "B", "C", "D"))) 
    stop("'dB' has to be one of the following character strings: 'max0', 'A', 'B', 'C' or 'D'")
  
  drtn <- duration(wave, f = wave@samp.rate)
  
  # filter to keep selections within the tlim range
  if (!is.null(X))
    sel.tab <- X[X$end < drtn,] else sel.tab <- col.clm <- NULL
  
  # add time margins
  if (!is.null(t.mar) & !is.null(sel.tab))
  {sel.tab$start <- sel.tab$start - t.mar
  sel.tab$end <- sel.tab$end + t.mar
  
  # fix the ones lower than 0 or longer than duration
  sel.tab$start[sel.tab$start < 0] <- 0  
  sel.tab$end[sel.tab$end > drtn] <- drtn
  }
  
  # interactive to NULL if 0
  if (!is.null(interactive)) if (interactive == 0) interactive <- NULL
  
  # set colors for colored signals
  if (!is.null(sel.tab))
  {if (!is.null(col.clm))
    colors <- sel.tab[ , col.clm] else colors <- rep(selec.col, nrow(sel.tab))}
  
  #pallete for background spectro
  if (is.null(collevels))
    collevels <- seq(-40, 0, 5)
  
  # set background spectro color
  basepal <- colorRampPalette(c(bg.col, base.col)) 
  
  #adjust flim if lower than higher top.freq
  # if (!is.null(flim) & !is.null(sel.tab)) {
  #   if (flim[1] > min(sel.tab$bottom.freq)) flim[1] <- min(sel.tab$bottom.freq)  
  #   if (flim[2] < max(sel.tab$top.freq)) flim[2] <- max(sel.tab$top.freq)  
  # }
  
  # add frequency margins
  if (!is.null(f.mar))
  {sel.tab$bottom.freq <- sel.tab$bottom.freq - f.mar
  sel.tab$top.freq <- sel.tab$top.freq + f.mar
  
  # fix the ones lower than 0 or longer than duration
  sel.tab$bottom.freq[sel.tab$bottom.freq < flim[1]] <- flim[1]  
  sel.tab$top.freq[sel.tab$top.freq > flim[2]] <- flim[2]
  }
  
  # read wave object  
  input <- inputw(wave = wave, f = wave@samp.rate)
  
  wave <- input$w
  
  f <- input$f
  
  n <- nrow(wave)
  step <- seq(1, n - wl, wl - (ovlp * wl/100))
  
  # to fix function name change in after version 2.0.5
  # if (exists("stdft")) stft <- stdft
  z <- stft_wrblr_int(wave = wave, f = f, wl = wl, zp = 0, step = step, 
            wn = wn, fftw = FALSE, scale = TRUE, complex = FALSE)
  
  X <- seq(0, n/f, length.out = length(step))
  
  if (is.null(flim)) {
    Y <- seq(0, (f/2) - (f/wl), length.out = nrow(z))/1000
  } else {
    fl1 <- flim[1] * nrow(z) * 2000/f
    fl2 <- flim[2] * nrow(z) * 2000/f
    z <- z[(fl1:fl2) + 1, ]
    Y <- seq(flim[1], flim[2], length.out = nrow(z))
  }
  
  if (!is.null(dB))  z <- 20 * log10(z)
  
  if (dB != "max0") {
    if (dB == "A") 
      z <- seewave::dBweight(Y * 1000, dBref = z)$A
    if (dB == "B") 
      z <- seewave::dBweight(Y * 1000, dBref = z)$B
    if (dB == "C") 
      z <- seewave::dBweight(Y * 1000, dBref = z)$C
    if (dB == "D") 
      z <- seewave::dBweight(Y * 1000, dBref = z)$D
  }
  
  Z <- t(z)
  
  maxz <- round(max(z, na.rm = TRUE))
  
  if (is.null(collevels))  {
    if (!is.null(dB)) 
      collevels <- seq(maxz - 30, maxz, by = 1) else   collevels <- seq(0, maxz, length = 30)
  }
  # opar <- par
  # par(bg = bg.col)
  # on.exit(par(opar), add = TRUE)
  # 
  if (!fast.spec)
  {   # seewave spectro 
    #plot background spectro
    if (!add)
    filled_contour_color_wrblr_int(x = X, y = Y, z = Z, levels = collevels, 
                       nlevels = 20, plot.title = title(main = title, 
                                                        xlab = tlab, ylab = flab), color.palette = basepal, 
                       axisX = axisX, axisY = axisY, col.lab = "black", 
                       colaxis = "black", bg.col = bg.col)
    
    if (!is.null(interactive))
    {
      write(file = "", x = "Select the signal(s) you want to highlight:")
      xy <- locator(interactive * 2)
      xy <- as.data.frame(xy)
      xy$selec <- rep(1:interactive, each = 2) 
      
      out <- lapply(unique(xy$selec), function(i) data.frame(start = min(xy$x[xy$selec == i]), end = max(xy$x[xy$selec == i]), bottom.freq = min(xy$y[xy$selec == i]), top.freq = max(xy$y[xy$selec == i])))
      
      sel.tab <- do.call(rbind, out)
      
      colors <- rep(selec.col, nrow(sel.tab))
    }
    
    #plot colored signals  
    if (!is.null(sel.tab))    
      out <- lapply(1:nrow(sel.tab), function(i)
        filled_contour_color_wrblr_int(x = X[X > sel.tab$start[i] & X < sel.tab$end[i]], y = Y[Y > sel.tab$bottom.freq[i] & Y < sel.tab$top.freq[i]], z = Z[X > sel.tab$start[i] & X < sel.tab$end[i], Y > sel.tab$bottom.freq[i] & Y < sel.tab$top.freq[i]], nlevels = 20, plot.title = FALSE, color.palette = colorRampPalette(c(rep(bg.col, 1), rep(colors[i], strength)), alpha = TRUE), levels = collevels,
                           axisX = FALSE, axisY = FALSE, col.lab = "black", 
                           colaxis = "black", add = TRUE, bg.col = bg.col)   
      )
  } else {  #fast spectro image
    #plot background spectro
    if (!add)
    image(x = X, y = Y, z = Z, col = basepal(length(collevels) - 1), xlab = tlab, ylab = flab, axes = FALSE)
    
    if (!is.null(interactive))
    {
      xy <- locator(interactive * 2)
      xy <- as.data.frame(xy)
      xy$selec <- rep(1:interactive, each = 2) 
    
    
    out <- lapply(unique(xy$selec), function(i) data.frame(start = min(xy$x[xy$selec == i]), end = max(xy$x[xy$selec == i]), bottom.freq = min(xy$y[xy$selec == i]), top.freq = max(xy$y[xy$selec == i])))
    
    sel.tab <- do.call(rbind, out)
    
    colors <- rep(selec.col, nrow(sel.tab))
  }
  
  
  #plot colored signals
  if (!is.null(sel.tab))
        out <- lapply(1:nrow(sel.tab), function(i)
                                       image(x = X[X > sel.tab$start[i] & X < sel.tab$end[i]], y = Y[Y > sel.tab$bottom.freq[i] & Y < sel.tab$top.freq[i]], z = Z[X > sel.tab$start[i] & X < sel.tab$end[i], Y > sel.tab$bottom.freq[i] & Y < sel.tab$top.freq[i]], col = colorRampPalette(c( rep(bg.col, 2), rep(colors[i], strength)), alpha = TRUE)(30), xlab = tlab, ylab = flab, axes = FALSE, xlim = range(X), add = TRUE)
                                     )


                                   if (axisY) axis(2, at = pretty(Y), labels = pretty(Y), cex.axis = cexlab)
                                   box()

                                   if (!is.null(title)) title(title)
}
 
  #add axis
   if (axisX) {
    if (rm.zero)
      axis(1, at = pretty(X)[-1], labels = pretty(X)[-1], cex.axis = cexaxis)  else
        axis(1, at = pretty(X), labels = pretty(X), cex.axis = cexaxis) 
  }
  
}


##############################################################################################################
#' alternative name for \code{\link{color.spectro}}
#'
#' @keywords internal
#' @details see \code{\link{color.spectro}} for documentation. \code{\link{color.spectro}} will be deprecated in future versions.
#' @export

color_spectro <- color.spectro
