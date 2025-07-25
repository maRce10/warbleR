#' Subset selection data frames based on manually filtered image files
#'
#' \code{filter_sels} subsets selection data frames based on image files that have been manually filtered.
#' @param X object of class 'selection_table', 'extended_selection_table' or data frame with the following columns: 1) "sound.files": name of the .wav
#' files, 2) "selec": number of the selections.
#' @param path Character string containing the directory path where the image files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' \code{\link{warbleR_options}} 'wav.path' argument does not apply.
#' @param lspec A logical argument indicating if the image files to be use for filtering were produced by the function \code{\link{full_spectrograms}}.
#' All the image files that correspond to a sound file must be deleted in order to be
#' filtered out.
#' @param img.suffix A character vector of length 1 with the suffix (label) at the end
#' of the names of the image files. Default is \code{NULL} (i.e. no suffix as in the images
#' produced by \code{\link{spectrograms}}). Ignored if \code{lspec = TRUE}.
#' @param it A character vector of length 1 giving the image type ("tiff", "jpeg" or "pdf") Default is "jpeg". Note that pdf files can only be generated by \code{\link{full_spectrogram2pdf}}.
#' @param incl.wav Logical. To indicate if sound files extensions are included ( \code{TRUE}, default) or not in the image
#' file names.
#' @param missing Logical. Controls whether the output data frame (or row index if is \code{index = TRUE})
#' contains the selections with images in the working directory (Default, \code{missing = FALSE})
#' or the ones with no image.
#' @param index Logical. If \code{TRUE} and \code{missing = FALSE} the row index for the selections with images in the working directory is returned. If \code{missing = TRUE}) then the row index of the ones with no image is returned instead. Default is \code{FALSE}.
#' @return If all sound files are ok, returns message "All files are ok!".
#'   Otherwise returns "These file(s) cannot be read" message with names of the
#'   corrupted sound files.
#' @details This function subsets selections (or sound files if \code{lspec} is \code{TRUE}) listed in a data frame
#'  based on the image files from spectrogram-creating functions (e.g. \code{\link{spectrograms}}) in the
#'  working directory. Only the selections/sound files with and image in the working directory will remain.
#'  This is useful for excluding selections from undesired signals. Note that the
#'  image files should be in the working directory (or the directory provided in 'path').
#' @export
#' @name filter_sels
#' @examples \dontrun{
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#'
#' spectrograms(lbh_selec_table,
#'   flim = c(0, 11), inner.mar = c(4, 4.5, 2, 1), outer.mar = c(4, 2, 2, 1),
#'   picsize = 2, res = 300, cexlab = 2, mar = 0.05, wl = 300, path = tempdir()
#' )
#'
#' # go to the working directory (tempdir()) and delete some images
#'
#' # filter selection data frame
#' fmloc <- filter_sels(X = lbh_selec_table, path = tempdir())
#'
#' # this data frame does not have the selections corresponding to the images that were deleted
#' fmloc
#'
#' # now using lspec images
#' full_spectrograms(
#'   sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300, ovlp = 10,
#'   path = tempdir()
#' )
#'
#' # go to the working directory (tempdir()) and delete lspec
#' # images (the ones with several rows of spectrograms)
#'
#' # filter selection data frame
#' fmloc2 <- filter_sels(
#'   X = lbh_selec_table, lspec = TRUE,
#'   path = tempdir()
#' )
#' }
#'
#' @references 
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on feb-6-2017 (MAS)

filter_sels <- function(X, path = NULL, lspec = FALSE, img.suffix = NULL, it = "jpeg",
                        incl.wav = TRUE, missing = FALSE, index = FALSE) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(filter_sels)

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

  # if it argument is not "jpeg" or "tiff"
  if (!any(it == "jpeg", it == "tiff", it == "pdf")) stop2(paste("Image type", it, "not allowed"))

  if (!all(c("sound.files", "selec") %in% colnames(X))) {
    stop2(paste(paste(c("sound.files", "selec")[!(c("sound.files", "selec") %in% colnames(X))], collapse = ", "), "column(s) not found in data frame"))
  }

  if (it != "pdf") {
    # check if files are in working directory
    imgs <- list.files(path = path, pattern = "\\.jpeg$|\\.tiff$", ignore.case = FALSE)
    if (length(imgs) == 0) {
      stop2("No image files in working directory")
    }

    # if not long spec
    if (!lspec) {
      # if img suffix not provided
      if (is.null(img.suffix)) {
        # if .wav extension is included
        if (incl.wav) {
          imgn <- paste(paste(X$sound.files, X$selec, sep = "-"), it, sep = ".")
        } else {
          imgn <- paste(paste(gsub("\\.wav$|\\.wac$|\\.mp3$|\\.flac$", "", X$sound.files, ignore.case = TRUE), X$selec, sep = "-"), it, sep = ".")
        }
      } else {
        if (incl.wav) {
          imgn <- paste(paste(X$sound.files, X$selec, img.suffix, sep = "-"), it, sep = ".")
        } else {
          imgn <- paste(paste(gsub("\\.wav$|\\.wac$|\\.mp3$|\\.flac$", "", X$sound.files, ignore.case = TRUE), X$selec, img.suffix, sep = "-"), it, sep = ".")
        }
      }
      # subset data frame X
      miss.index <- imgn %in% imgs

      if (!index) {
        if (missing) Y <- X[!miss.index, , drop = FALSE] else Y <- X[miss.index, , drop = FALSE]
      } else if (missing) Y <- which(!miss.index) else Y <- which(miss.index)
    } else {
      #   #remove the ones with no pX.it ending
      imgs <- grep("p\\d+\\.jpeg|p\\d+\\.tiff", imgs, value = TRUE)
      if (length(imgs) == 0) stop2("Images have not been produced by 'full_spectrograms' function")

      # subset selection table
      miss.index <- gsub("\\.wav$|\\.wac$|\\.mp3$|\\.flac$", "", X$sound.files, ignore.case = TRUE) %in% gsub("-p\\d+\\.jpeg$|-p\\d+\\.tiff$", "", imgs)

      if (!index) {
        if (missing) Y <- X[!miss.index, , drop = FALSE] else Y <- X[miss.index, , drop = FALSE]
      } else if (missing) Y <- which(!miss.index) else Y <- which(miss.index)
    }
  } else {
    # check if pdf files are in working directory
    imgs <- list.files(path = path, pattern = ".pdf$", ignore.case = FALSE)
    if (length(imgs) == 0) {
      stop2("No pdf files in working directory")
    }

    # subset selection table
    miss.index <- gsub("\\.wav$|\\.wac$|\\.mp3$|\\.flac$", "", X$sound.files, ignore.case = TRUE) %in% gsub(".pdf$", "", imgs)

    if (!index) {
      if (missing) miss.index <- !miss.index

      Y <- X[miss.index, , drop = FALSE]

      if (is_extended_selection_table(X)) {
        attributes(X)$check.results <- droplevels(attributes(X)$check.results[miss.index, ])
      }
    } else {
      Y <- which(miss.index)
    }
  }

  if (!index) {
    if (nrow(Y) == 0) stop2("Image files in working directory do not match sound file names in X (wrong working directory?)")
    return(Y)
  } else {
    if (length(Y) == 0) message2("Index vector is of length 0")
    return(Y)
  }
}
