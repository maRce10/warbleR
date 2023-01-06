#' Setting warbleR options
#'
#' \code{warbleR_options} sets global parameters for warbleR functions
#' @usage warbleR_options(reset = FALSE, ...)
#' @param reset Logical. If \code{TRUE} then all global parameters are removed. Default is \code{FALSE}.
#' @param ... Arguments in `parameter = value`` form, or a list of tagged values. The tags (i.e. parameters)
#'  must come from the list of parameters described below.
#' @return When parameters are set by warbleR_options, their former values are
#' returned in an invisible named list. Such a list can be passed as an argument to
#' pboptions to restore the parameter values. If the function is called with no arguments the current option values are printed.
#' @export
#' @name warbleR_options
#' @details The function aims to simplify the use of parameters that apply to many warbleR functions (i.e. global parameters)
#' by setting a default value that will be used to any function in downstream
#' analyses. Tags that are set with warbleR_options will be used iby the functions
#' that share those arguments. However, if an argument is set within a function call
#' it will overwrite the values set by warbleR_options. Hence, the functions remain
#' 'flexible' as their parameters can also be modified 'on the fly'. The following tags are available:
#' \itemize{
#'    \item \code{bp}: Numeric vector of length 2 giving the lower and upper limits of a
#'   frequency bandpass filter (in kHz).
#'    \item \code{collevels}:  A numeric vector of length 3. Specifies levels to partition the
#'   amplitude range of the spectrogram (in dB) as in \code{\link[seewave]{spectro}}. The more levels the higher the
#'   resolution of the spectrogram. The lower the first value the darker the spectrograms.
#'    \item \code{flim}: A numeric vector of length 2 for the frequency limit in kHz of
#'   the spectrogram, as in \code{\link[seewave]{spectro}}.
#'    \item \code{it}: A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted.
#'    \item \code{osci}: Logical argument to add an oscillogram underneath spectrogram, as
#'   in \code{\link[seewave]{spectro}}.
#'    \item \code{pal}: A color palette function to be used to assign colors in the
#'   plot, as in \code{\link[seewave]{spectro}}.
#'    \item \code{parallel}: Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used in iterative functions.
#'    \item \code{pb}: Logical argument to control whether progress bar is used.
#'    \item \code{res}: Numeric argument of length 1. Controls image resolution in all image creating functions.
#'    \item \code{wav.path}: Character string containing the directory path where the
#'    sound files are located. Used as 'path' in all functions in which sound files are read.
#'    \item \code{wl}: A numeric vector of length 1 specifying the window length for creating spectrogram (either for plotting or for measuring spectrogram parameters).
#'    \item \code{wn}: Character vector of length 1 specifying the window name for creating spectrogram (either for plotting or for measuring spectrogram parameters). See function \code{\link[seewave]{ftwindow}} for options.
#'    }
#' @examples
#' {
#'   # load data and save in temporary working directory
#'   data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#'   writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#'   writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#'   writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#'   writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#'   # sig2noise with progress bar (by default is TRUE)
#'   a <- sig2noise(X = lbh_selec_table, mar = 0.1, path = tempdir())
#'
#'   # set progress bar to FALSE with warbleR_options
#'   warbleR_options(pb = FALSE, path = tempdir())
#'
#'   # sig2noise without progress bar
#'   a <- sig2noise(X = lbh_selec_table, mar = 0.1)
#'
#'   # sig2noise with progress bar by setting it within the function call (overwritting options)
#'   a <- sig2noise(X = lbh_selec_table, pb = TRUE, mar = 0.1)
#'
#'   # sig2noise without progress bar using warbleR_options setting again
#'   a <- sig2noise(X = lbh_selec_table, mar = 0.1)
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on apr-18-2018 (MAS)

warbleR_options <- function(reset = FALSE, ...) {
  opar <- getOption("warbleR")
  argms <- list(...)

  if (length(argms) > 0) {
    # rename wav.path as path
    names(argms)[names(argms) == "wav.path"] <- "path"

    if (!is.null(argms$path)) {
      if (!dir.exists(argms$path)) {
        stop2("provided 'wav.path' doesn't exist")
      } else {
        argms$path <- normalizePath(argms$path)
      }
    }

    if (!is.null(argms$img.path)) {
      if (!dir.exists(argms$img.path)) {
        stop2("provided 'dest.path' doesn't exist")
      } else {
        argms$img.path <- normalizePath(argms$img.path)
      }
    }

    if (!is.null(argms$dest.path)) {
      if (!dir.exists(argms$dest.path)) {
        stop2("provided 'dest.path' doesn't exist")
      } else {
        argms$dest.path <- normalizePath(argms$dest.path)
      }
    }


    if (length(argms) > 0) {
      if (length(argms) == 1 && is.list(argms[[1]])) {
        npar <- argms[[1]]
      } else {
        npar <- argms
      }

      # add previous options not reset in new call
      if (length(opar) > 0) {
        opar <- opar[!names(opar) %in% names(npar)]

        npar <- c(npar, opar)
      }

      # order aguments by name
      npar <- npar[order(names(npar))]


      options("warbleR" = npar)
    }
    invisible(opar)
  } else if (reset) {
    options("warbleR" = NULL)
  } else {
    print(opar)
  }
}
