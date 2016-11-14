#' Acoustic dissimilarity using dynamic time warping on fundamental frequency contours
#' 
#' \code{ffDTW} calculates acoustic dissimilarity of fundamental frequency contours using dynamic
#' time warping. Internally it applies the \code{\link[dtw]{dtwDist}} function from the \code{dtw} package.
#' @usage ffDTW(X, wl = 512, flim = c(0, 22), length.out = 20, wn = "hanning", pal =
#'   reverse.gray.colors.2, ovlp = 70, inner.mar = c(5, 4, 4, 2), outer.mar = 
#'   c(0, 0, 0, 0), picsize = 1, res = 100, cexlab = 1, title = TRUE, propwidth = FALSE, 
#'   xl = 1, gr = FALSE, sc = FALSE, bp = c(0, 22), cex = 1, 
#'   threshold = 15, col = "red2", pch = 16,  mar = 0.05, 
#'   lpos = "topright", it = "jpeg", img = TRUE, parallel = 1, path = NULL,
#'    img.suffix = "ffDTW")
#' @param  X Data frame with results containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param flim A numeric vector of length 2 for the frequency limit of 
#'   the spectrogram (in kHz), as in \code{\link[seewave]{spectro}}. Default is c(0, 22).
#' @param length.out A character vector of length 1 giving the number of measurements of fundamental 
#' frequency desired (the length of the time series).
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param pal A color palette function to be used to assign colors in the 
#'   plot, as in \code{\link[seewave]{spectro}}. Default is reverse.gray.colors.2.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70. 
#' @param inner.mar Numeric vector with 4 elements, default is c(5,4,4,2). 
#'   Specifies number of lines in inner plot margins where axis labels fall, 
#'   with form c(bottom, left, top, right). See \code{\link[graphics]{par}}.
#' @param outer.mar Numeric vector with 4 elements, default is c(0,0,0,0). 
#'   Specifies number of lines in outer plot margins beyond axis labels, with 
#'   form c(bottom, left, top, right). See \code{\link[graphics]{par}}.
#' @param picsize Numeric argument of length 1. Controls relative size of 
#'   spectrogram. Default is 1.
#' @param res Numeric argument of length 1. Controls image resolution.
#'   Default is 100 (faster) although 300 - 400 is recommended for publication/ 
#'   presentation quality.
#' @param cexlab Numeric vector of length 1 specifying the relative size of axis 
#'   labels. See \code{\link[seewave]{spectro}}.
#' @param title Logical argument to add a title to individual spectrograms. 
#'   Default is \code{TRUE}.
#' @param propwidth Logical argument to scale the width of spectrogram 
#'   proportionally to duration of the selected call. Default is \code{FALSE}.
#' @param xl Numeric vector of length 1. A constant by which to scale 
#'   spectrogram width. Default is 1.
#' @param gr Logical argument to add grid to spectrogram. Default is \code{FALSE}.
#' @param sc Logical argument to add amplitude scale to spectrogram, default is 
#'   \code{FALSE}.
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param cex Numeric vector of length 1, specifies relative size of points 
#'   plotted for frequency measurements and legend font/points, respectively. 
#'   See \code{\link[seewave]{spectro}}.
#' @param threshold amplitude threshold (\%) for fundamental frequency detection. Default is 15.
#' @param col Vector of length 1 specifying colors of points plotted to mark 
#'  fundamental frequency measurements. Default is "red2".
#' @param pch Numeric vector of length 1 specifying plotting characters for 
#'   the frequency measurements. Default is 16.
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the selections
#'  to set spectrogram limits. Default is 0.05.
#' @param lpos Character vector of length 1 or numeric vector of length 2, 
#'   specifiying position of legend. If the former, any keyword accepted by 
#'   xy.coords can be used (see below). If the latter, the first value will be the x 
#'   coordinate and the second value the y coordinate for the legend's position.
#'   Default is "topright".
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#'  Not available in Windows OS.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.  
#' @param img.suffix A character vector of length 1 with a sufix (label) to add at the end of the names of 
#' image files. Default is \code{NULL}.
#' @return A matrix with the pairwise dissimilarity values. If img is 
#' \code{FALSE} it also produces image files with the spectrograms of the signals listed in the 
#' input data frame showing the location of the fundamental frequencies.
#' @family spectrogram creators
#' @seealso \code{\link{specreator}} for creating spectrograms from selections,
#'  \code{\link{snrspecs}} for creating spectrograms to 
#'   optimize noise margins used in \code{\link{sig2noise}}
#' @export
#' @name ffDTW
#' @details This function extracts the fundamental frequency values as a time series and
#'  then calculates the pairwise acoustic dissimilarity of the selections using dynamic time warping.
#' The function uses the \code{\link[stats]{approx}} function to interpolate values between fundamental
#'  frequency  measures. If 'img' is  \code{TRUE} the function also produces image files
#'  with the spectrograms of the signals listed in the input data frame showing the
#'  location of the fundamental frequencies. Note that if no amplitude is detected at the begining or end 
#'  of the signals then NAs will be generated. On the other hand, if amplitude is not detected in between signal
#'  segments in which amplitude was detected then the values of this adjacent segments will be interpolated to fill out the missing values (e.g. no NAs in between detected amplitude segments). 
#' @seealso dfDTW \code{\link{dfts}}, \code{\link{ffts}}, \code{\link{dfDTW}}
#' @examples
#' \dontrun{
#' # set the temp directory
#' setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2","manualoc.df"))
#' writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
#' writeWave(Phae.long1, "Phae.long1.wav")
#' 
#' # run function 
#' ffDTW(manualoc.df, length.out = 30, flim = c(1, 12), img = T, bp = c(1, 9), wl = 300)
#' 
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on oct-26-2016 (MAS)

ffDTW <- function(X, wl = 512, flim = c(0, 22), length.out = 20, wn = "hanning", pal = reverse.gray.colors.2, ovlp = 70, 
                  inner.mar = c(5,4,4,2), outer.mar = c(0,0,0,0), picsize = 1, res = 100, cexlab = 1,
                  title = TRUE, propwidth = FALSE, xl = 1, gr = FALSE, sc = FALSE, 
                  bp = c(0, 22), cex = 1, threshold = 15, col = "red2", pch = 16,
                  mar = 0.05, lpos = "topright", it = "jpeg", img = TRUE, parallel = 1, path = NULL,
                  img.suffix = "ffDTW"){     
    
  # run ffts function
  res <- ffts(X = X, wl = wl, flim = flim, length.out = length.out, wn = wn, pal = pal, ovlp = ovlp, 
       inner.mar = inner.mar, outer.mar = outer.mar, picsize = picsize, res = res, cexlab = cexlab, 
       title = title, propwidth = propwidth, xl = xl, gr = gr, sc = sc, bp = bp, cex = cex,
           threshold = threshold, col = col, pch = pch,  mar = mar,
           lpos = lpos, it = it, img = img, parallel = parallel, path = path, img.suffix =  img.suffix)
  
  #matrix of fund freq time series
  mat <- res[,3:ncol(res)]
    
  #stop if NAs in matrix
    if(any(is.na(mat))) stop("missing values in frequency time series (fundamental frequency was not detected at one or both extremes of the signal)")
  
  dm <- dtw::dtwDist(mat, mat)       
  rownames(dm) <- colnames(dm) <- paste(res$sound.files, res$selec, sep = "-")
  return(dm)

  }
