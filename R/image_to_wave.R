#' Convert images into wave objects
#' 
#' \code{image_to_wave} converts images in 'png' format into wave objects using the inverse Fourier transformation
#' @export image_to_wave
#' @usage image_to_wave(file, duration = 1, samp.rate = 44.1, 
#' bit.depth = 16, flim = c(0, samp.rate / 2), plot = TRUE)
#' @param file Character with the name of image file to be converted. File must be in 'png' format. 
#' @param duration duration of the output wave object (in s).
#' @param samp.rate Numeric vector of length 1 indicating the sampling rate of the output wave object (in kHz). Default is 44.1.
#' @param bit.depth Numeric vector of length 1 with the dynamic interval (i.e. bit depth) for output files. Default is 16.
#' @param flim Numeric vector of length 2 indicating the highest and lowest 
#'   frequency limits (kHz) in which the image would be located. Default is \code{c(0, samp.rate / 2)}.
#' @param plot Logical argument to control if image is plotted after being imported into R.
#' @return A single wave object.
#' @name image_to_wave
#' @details This function converts images in 'png' format into wave objects using the inverse Fourier transformation.
#' @examples{ 
#'  ### create image with text to use in the spectrogram
#'  # remove margins of plot
#'  par(mar = rep(0, 4))
#'  
#'  # empty plot
#'  plot(0, type='n',axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))
#'  
#'  # text to include
#'  text <- " warbleR "
#'  
#'  # add text
#'  text(x = 0.5, y = 0.5, labels = text, cex = 11, font = 1)
#'  
#'  # save image in temporary directory
#'  dev2bitmap(file.path(tempdir(), "temp-img.png"), type = "pngmono", res = 30)
#'  
#'  # read it 
#'   wv <- image_to_wave(file = file.path(tempdir(), "temp-img.png"), plot = TRUE, flim = c(1, 12))
#' 
#'  # output wave object
#'  # wv
#'   
#'  ## plot it   
#'  # reset margins
#'  par(mar = c(5, 4, 4, 2) + 0.1)
#'  
#'  # plot spectrogram
#'  # spectro(wave = wv, scale = FALSE, collevels = seq(-30, 0, 5),
#'  # palette = reverse.terrain.colors, ovlp = 90, grid = FALSE, flim = c(2, 11))
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on dec-27-2019 (MAS)

image_to_wave <- function(file, duration = 1, samp.rate = 44.1, 
                        bit.depth = 16, flim = c(0, samp.rate / 2), plot = TRUE){
  
  # error message if jpeg package is not installed
  if (!requireNamespace("png",quietly = TRUE))
    stop("must install 'png' to use this function")

  # get previous graphic settings back when done
  on.exit(dev.off())
  
    # check file 
  if (!file.exists(file)) stop("'file' supplied was not found") 
  
  # no higher than nyquist frequency
  if (flim[2] > samp.rate / 2) stop("high frequency cannot be higher than half the sampling rate (Nyquist frequency)")

  # read image
  mat <- png::readPNG(file)

  # get dimensions
  dms <- dim(mat)
  
  # convert to a single layer if array
  if (length(dms) == 3) {

    mat <- 0.21 * mat[ , , 1] + 0.71 * mat[ , , 1] + 0.07 * mat[ , , 3]
    
    mat <- mat / max(mat)
  
  }
  
  # flip horizontally
  mat <- mat[dms[1]:1, ]
  
  # plot if requested
  if (plot){
  
    par(mar = c(0,0,0,0))
    
    image(t(mat), useRaster = TRUE, col = gray.colors(10))
    }
  
  # get inverse of color values so darker is louder sound
  mat <- 1 - mat
  
  # if flim is not to whole freq range
  if (!identical(flim, c(0, samp.rate / 2))){
    
    # calculate difference in freq range to force it to be in supplied flim
    low.dff <-  flim[1] / diff(flim)
    upp.dff <- ((samp.rate / 2) - flim[2]) / diff(flim)
        
    # calculate number of extra rows to add 
    low.extr <- round(dms[1] *  low.dff, 0)
    upp.extr <- round(dms[1] *  upp.dff, 0)
    
    # add empty cells
    if (low.extr > 0)
        mat <- rbind(matrix(0, nrow = low.extr, ncol = dms[2]), mat)
    
    if (upp.extr > 0)
      mat <- rbind(mat, matrix(0, nrow = upp.extr, ncol = dms[2]))

  }
  
  # base on number of columns calculate wl given the supplied sampling rate
  crrnt.wl <- (samp.rate * 1000) / dms[2] * duration
  
  # get FT windows
  win <- seewave::ftwindow(wl = crrnt.wl, wn = "hamming")
  
  # get inverse Fourier transformation for each column in image (modified from seewave::istft)
  inv.ft <- c(sapply(seq(0, crrnt.wl * (dms[2] - 1), by = crrnt.wl), function(b) {
    
    X <- mat[, 1 + b / crrnt.wl]
    mirror <- rev(X[-1])
    mirror <- complex(real = Re(mirror), imaginary = -Im(mirror))
    X <- c(X, complex(real = Re(X[length(X)]), imaginary = 0), mirror)
    xprim <- Re(stats::fft(X, inverse = TRUE)/length(X))
      y <- xprim[1:crrnt.wl] * win
    return(y)
      }
    ))
  
  # convert to wave object
  wave.obj <- seewave::outputw(wave = inv.ft, f = samp.rate * 1000, bit = bit.depth, format = "Wave")
  
  return(wave.obj)
}
