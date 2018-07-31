#' Plot a mosaic of spectrograms with varying display parameters
#' 
#' \code{spec_param} plots a mosaic of spectrograms with varying display parameters to
#' facilitate selection of display parameters
#' @usage spec_param(X, length.out = 5, ovlp = 90, wl = c(100, 1000), wn = "hanning",
#'  collev.min = -40, pal = "reverse.gray.colors.2", path = NULL, rm.axes = TRUE, ...)
#' @param X object of class 'selection_table', 'extended_selection_table' or data frame with a single row and columns for sound file name (sound.files), selection number (selec), 
#' and start and end time of signal (start and end). Default is \code{NULL}.
#' @param length.out Numeric vector of length 1 controling the number of sublevels of
#'  the numeric arguments for which a range has been provided. Ranges are allowed for
#'  'ovlp', 'wl', and 'collev.min' arguments.
#' @param ovlp Numeric vector of length 1 or 2 specifying \% of overlap (or 
#' lower/upper values the desired range) between two consecutive windows, as in 
#' \code{\link[seewave]{spectro}}. Default is 90.
#' @param wl A numeric vector of length 1 or 2 specifying the window length (length 1)
#' or the lower and upper range limits of the desired window length range (length 2) for creating spectrograms.
#' Default is c(100, 1000).
#' @param wn Character vector specifying the window function names to be used. Several
#' names can be provided. See \code{\link[seewave]{ftwindow}}
#' for name options. Default is "hanning". If "all", then all window functions available are used.
#' @param collev.min A (negative) numeric vector of length 1 or 2. Determines the first argument
#' to use in 'collevels' for the internal spectrogram creating function. This replaces the 
#' first element in the 'collevels' as in \code{\link[seewave]{spectro}}. Note that
#' 'collevels' is not available in this function \code{\link[warbleR]{spec_param}}.
#' @param pal Color palette function for spectrogram. Default is "reverse.gray.colors.2". 
#' Several palettes can be provided in a character vector. Note that, contrary to
#'  other \code{warbleR} and \code{seewave} functions, the 
#' palette most be provided as character string rather than as a function. See 
#' \code{\link[seewave]{spectro}} for more palettes. 
#' @param path Character string containing the directory path where the sound file are located. 
#' @param rm.axes Logical. If \code{TRUE} frequency and time axes are excluded. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{\link{catalog}} function for customizing
#' graphical output. Check out \code{\link{catalog}} for more details.
#' @return Image files with spectrograms of whole sound files in the working directory. Multiple pages
#' can be returned, depending on the length of each sound file. 
#' @export
#' @name spec_param
#' @details This functions aims to simplify the selection of spectrogram parameters. 
#' The function plots, for a single selection, a mosaic of spectrograms with varying 
#' display parameters. For numeric arguments the upper and lower limits of a range can
#' be provided. The following arguments accept can have varying values:
#' \itemize{
#'    \item \code{wl}: Windows length (numeric range)
#'    \item \code{ovlp}: Overlap (numeric range)
#'    \item \code{collev.min}: Minimum value of the color levels (numeric range) 
#'    \item \code{wn}: window function names (character)
#'    \item \code{pal}: palette (character)
#'    }
#'  Outputs are similar to those of \code{\link{catalog}}. The output image files can be put together in a single pdf file with \code{\link{catalog2pdf}}.
#'   We recommend using low resolution (~60-100) and smaller dimensions (width & height < 10) if
#'   aiming to generate pdfs (otherwise pdfs could be pretty big).
#' @seealso \href{https://marce10.github.io/2017/03/17/Creating_song_catalogs.html}{blog post on creating catalogs},
#' \href{https://marce10.github.io/2017/07/31/Updates_on_catalog_function.html}{blog post on customizing catalogs}
#' , \code{\link{catalog2pdf}}
#' @examples
#' \dontrun{
#' # Set temporary working directory
#' # setwd(tempdir())
#' # save sound file examples
#' data(list = c("Phae.long1", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav") 
#' 
#' # variable collevels
#' spec_param(X = selec.table, wl = 164, ovlp = c(90), wn = c("flattop"), 
#' length.out = 16, nrow = 4, ncol = 4, width = 20, height = 11.3, rm.axes = TRUE, 
#' cex = 1, box = F, collev.min = c(-20, -150))
#' 
#' # variable overlap and wn
#' spec_param(X = selec.table, wl = 164, ovlp = c(50, 90), 
#' wn = c("hanning", "hamming", "rectangle", "bartlett", "blackman", "flattop"),
#' length.out = 7, nrow = 6, ncol = 7, width = 20, height = 11.3, rm.axes = TRUE, 
#' cex = 1, box = F)
#' 
#' # variable wl and wn
#' spec_param(X = selec.table, wl = c(100, 1000), ovlp = c(50, 90), wn = "all", 
#' length.out = 5, nrow = 10, ncol = 14, width = 20, height = 11.3, rm.axes = TRUE, 
#' cex = 0.7)
#' 
#' # variable wl, collev.min and wn 
#' spec_param(X = selec.table, wl = c(100, 1000), ovlp = 90, 
#' wn = c("hanning", "hamming", "rectangle"), collev.min = c(-110, -25), 
#' length.out = 3, nrow = 10, ncol = 14, width = 20, height = 11.3, rm.axes = TRUE,
#'  cex = 0.7)
#'  
#'  # variable wl, wn and pal
#'  spec_param(X = selec.table, wl = c(100, 1000), ovlp = 90, 
#'  wn = c("hanning", "hamming", "rectangle"), 
#'  pal = c("reverse.gray.colors.2", "reverse.topo.colors", 
#'  "reverse.terrain.colors", "reverse.cm.colors"), 
#'  length.out = 4, nrow = 5, ncol = 10, width = 20, height = 11.3,
#'   rm.axes = TRUE, cex = 0.7, lab.mar = 2)
#'   
#'   # wl, wn and pal
#'   spec_param(X = selec.table, wl = c(100, 1000), ovlp = 90,
#'    wn = c("hanning", "hamming", "rectangle"), 
#'   pal = c("reverse.gray.colors.2", "reverse.topo.colors", 
#'   "reverse.terrain.colors", "reverse.cm.colors"), 
#'   length.out = 4, nrow = 5, ncol = 10, width = 20, height = 11.3, rm.axes = TRUE,
#'    cex = 0.7, group.tag = "wn",  spec.mar = 0.4, lab.mar = 0.8, box = FALSE, 
#'    tag.pal = list(reverse.cm.colors))


#' 
#' check this floder
#' getwd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on mar-08-2018 (MAS)

spec_param <- function(X, length.out = 5, ovlp = 90, wl = c(100, 1000), 
                    wn = "hanning", collev.min = -40,
                    pal = "reverse.gray.colors.2", path = NULL, rm.axes = TRUE, ...)
{
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(spec_param)
  
  # get warbleR options
  opt.argms <- .Options$warbleR
  
  # rename path for sound files
  names(opt.argms)[names(opt.argms) == "wav.path"] <- "path"
  
  opt.argms <- opt.argms[which(names(opt.argms) == "path")]
  
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
  
  # stop if pal is function
  if (is.function(pal)) stop("'pal' should be a character vector")
  
  ## reset parameters
  # only seewave spectros
  fast.spec <- FALSE
  if (wn[1] == "all") wn <- c("bartlett", "blackman", "flattop", "hamming", "hanning", "rectangle")
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  if (nrow(X) > 1){
  X <- X[1, , drop = FALSE]
  write(file = "", x = "Data frame provided has more than 1 selection (row), only the first one was used")
  }
  
  if (length.out < 2) stop("'length.out' should be equal or higher than 2")
  
  exp.cols <- c("ovlp", "wl", "wn", "collev.min", "pal")[which(c(length(ovlp), length(wl), length(wn), length(collev.min), length(pal)) > 1)]
  
  # expand arguments
  if (length(wl) > 1)
  wl <- seq(wl[1], wl[2], length.out = length.out)

  if (length(ovlp) > 1)
    ovlp <- seq(ovlp[1], ovlp[2], length.out = length.out)
  
  if (length(collev.min) > 1)
    collev.min <- seq(collev.min[1], collev.min[2], length.out = length.out)
  
  if (is_extended_selection_table(X))  X.orig <- X
  
  # Expand data frame
  X <- suppressWarnings(data.frame(X, expand.grid(ovlp = ovlp, wl = wl, 
          collev.min = collev.min, wn = wn, pal = pal), stringsAsFactors = FALSE))
 
  X$ovlp <- round(X$ovlp, 0)
  X$wl <- round(X$wl, 0)
  X$collev.min <- round(X$collev.min, 0)
  X$selec2 <- X$selec
  X$selec <- 1:nrow(X)
  X$lbs <- ""
  
  
  if (length(exp.cols) > 0)
  for(i in seq_len(length(exp.cols)))
    X$lbs <- paste(X$lbs, exp.cols[i], "=", X[ , exp.cols[i]], " ")
  
  co <- 32
  if (max(nchar(X$lbs)) > co) {
    empty_spc <- sapply(gregexpr(" ", substr(X$lbs, co, 1000)), "[[", 1) + co

        for(i in 1:nrow(X)){
          if (nchar(X$lbs[i]) > co)
    substring(X$lbs[i], first = empty_spc[i], last = empty_spc[i] + 1) <- paste0("\n", substr(X$lbs[i], empty_spc[i], empty_spc[i] + 1))
        }
    }
  
  X$lbs <- gsub(" \n$| $|  $|^ |^  ", "", X$lbs)  
  
  
  if (exists("X.orig"))
    {
    attributes(X)$check.results <- do.call(rbind, lapply(1:nrow(X), function(x) attributes(X.orig)$check.results[1, ]))
    attributes(X)$check.results$selec <- 1:nrow(X)

    attributes(X)$wave.objects <- attributes(X.orig)$wave.objects[1]

    attributes(X)$by.song <- attributes(X.orig)$by.song
    class(X) <- class(X.orig)
    }

    catalog(X = X, ovlp = X$ovlp, wl = X$wl, collevels = "collev.min", title = paste(X$sound.files[1], X$selec2[1]), rm.axes = rm.axes, img.suffix = "spec_param", 
                  wn = X$wn, pal = "pal.list", path = path, labels = c("lbs"), ...)
  
}
