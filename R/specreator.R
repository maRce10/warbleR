#' Spectrograms of selected signals
#' 
#' \code{specreator} creates spectrograms of signals from selection tables.
#' @usage specreator(X, wl = 512, flim = "frange", wn = "hanning", pal  = reverse.gray.colors.2, 
#' ovlp = 70, inner.mar = c(5, 4, 4, 2), outer.mar = c(0, 0, 0, 0), picsize = 1, res = 100, 
#' cexlab = 1, propwidth = FALSE, xl = 1, osci = FALSE, gr = FALSE,  sc = FALSE, line = TRUE,
#' col = "#07889B", fill = adjustcolor("#07889B", alpha.f = 0.15), lty = 3, 
#' mar = 0.05, it = "jpeg", parallel = 1, path = NULL, pb = TRUE, fast.spec = FALSE, 
#' by.song = NULL, sel.labels = "selec", title.labels = NULL, dest.path = NULL, ...)
#' @param X 'selection_table', 'extended_selection_table' or data frame containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signals (start and end). 
#' 'top.freq' and 'bottom.freq' columns are optional.
#' The output of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. If using an 
#' 'extended_selection_table' the sound files are not required (see \code{\link{selection_table}}). 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param flim A numeric vector of length 2 for the frequency limit (in kHz) of 
#'   the spectrogram, as in \code{\link[seewave]{spectro}}. The function also 
#'   accepts 'frange' (default) which produces spectrograms with a frequency 
#'   limit around the range of each signal (adding a 1 kHz margin).  
#' @param wn Character vector of length 1 specifying window name. Default is 
#'   "hanning". See function \code{\link[seewave]{ftwindow}} for more options.
#' @param pal A color palette function to be used to assign colors in the 
#'   plot, as in \code{\link[seewave]{spectro}}. Default is reverse.gray.colors.2. 
#' @param ovlp Numeric vector of length 1 specifying the percent overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70.
#' @param inner.mar Numeric vector with 4 elements, default is c(5,4,4,2). 
#'   Specifies number of lines in inner plot margins where axis labels fall, 
#'   with form c(bottom, left, top, right). See \code{\link[graphics]{par}}.
#' @param outer.mar Numeric vector with 4 elements, default is c(0,0,0,0). 
#'   Specifies number of lines in outer plot margins beyond axis labels, with 
#'   form c(bottom, left, top, right). See \code{\link[graphics]{par}}.
#' @param picsize Numeric argument of length 1. Controls relative size of 
#'   spectrogram. Default is 1. Ignored when propwidth is \code{TRUE}.
#' @param res Numeric argument of length 1. Controls image resolution.
#'   Default is 100 (faster) although 300 - 400 is recommended for publication/ 
#'   presentation quality.
#' @param cexlab Numeric vector of length 1 specifying the relative size of axis 
#'   labels. See \code{\link[seewave]{spectro}}.
#' @param propwidth Logical argument to scale the width of spectrogram 
#'   proportionally to duration of the selection. Default is \code{FALSE}.
#' @param xl Numeric vector of length 1. A constant by which to scale 
#'   spectrogram width if propwidth = \code{TRUE}. Default is 1.
#' @param osci Logical argument to add an oscillogram underneath spectrogram, as
#'   in \code{\link[seewave]{spectro}}. Default is \code{FALSE}.
#' @param gr Logical argument to add grid to spectrogram. Default is \code{FALSE}.
#' @param sc Logical argument to add amplitude scale to spectrogram, default is 
#'   \code{FALSE}.
#' @param line Logical argument to add lines at start and end times of selection 
#'   (or box if bottom.freq and top.freq columns are provided). Default is \code{TRUE}.
#' @param col Color of 'line'. Default is "#07889B".
#' @param fill Fill color of box around selections. Default is  \code{adjustcolor("#07889B", alpha.f = 0.15)}.
#' @param lty Type of 'line' as in \code{\link[graphics]{par}}. Default is 1. 
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the start and end points of selections,
#'    dealineating spectrogram limits. Default is 0.05.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#'   "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'   It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#'   If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, 
#'   which substantially increases performance (much faster), although some options become unavailable, 
#'   as collevels, and sc (amplitude scale). This option is indicated for signals with high background noise 
#'   levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#'   \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}}
#'   (which should be imported from the package monitoR) seem to work better with 'fast' spectrograms. 
#'   Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, \code{\link[monitoR]{gray.3}} 
#'   offer decreasing darkness levels.
#' @param by.song Character string with the column name containing song labels. If
#' provide a single spectrogram containing all elements for each song will be produce. Note that 
#' the function assumes that each song has a unique label within a sound file. If \code{NULL} (default), spectrograms are produced for single selections.
#' @param sel.labels Character string with the name of the column(s) for selection 
#' labeling. Ignored if 'by.song' is \code{NULL}. Default is 'selec'. Set to \code{NULL} to remove labels.
#' @param title.labels Character string with the name(s) of the column(s) to use as title. Default is \code{NULL} (no title). Only sound file and song included if 'by.song' is provided.
#' @param dest.path Character string containing the directory path where the cut sound files will be saved.
#' If \code{NULL} (default) then the folder containing the sound files will be used instead.
#' @param ... Additional arguments to be passed to the internal spectrogram 
#' creating function for customizing graphical output. The function is a modified 
#' version of \code{\link[seewave]{spectro}}, so it takes the same arguments. 
#' @return Image files containing spectrograms of the signals listed in the input data frame.
#' @family spectrogram creators
#' @seealso \code{\link{trackfreqs}} for creating spectrograms to visualize 
#'   frequency measurements by \code{\link{specan}}, \code{\link{snrspecs}} for 
#'   creating spectrograms to optimize noise margins used in \code{\link{sig2noise}}
#' @export
#' @name specreator
#' @details This function provides access to batch process of (a modified version of) the \code{\link[seewave]{spectro}} function from the 'seewave' package. The function creates spectrograms for visualization of vocalizations. 
#' Setting inner.mar to c(4,4.5,2,1) and outer.mar to c(4,2,2,1) works well when picsize = 2 or 3. 
#' Title font size, inner.mar and outer.mar (from mar and oma) don't work well when osci or sc = TRUE,
#' this may take some optimization by the user. Setting 'fast' argument to TRUE significantly increases speed, although 
#' some options become unavailable, as collevels, and sc (amplitude scale). This option is indicated for signals with 
#' high background noise levels. 
#' @examples
#' { 
#' # load and save data
#' data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) #save sound files
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' 
#' # make spectrograms
#' specreator(X = lbh_selec_table, flim = c(0, 11), res = 300, mar = 0.05, wl = 300, path = tempdir())
#'  
#' # check this folder
#' tempdir()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com}) and Grace Smith Vidaurre
#last modification on mar-13-2018 (MAS)

specreator <- function(X, wl = 512, flim = "frange", wn = "hanning", pal = reverse.gray.colors.2, ovlp = 70, 
                        inner.mar = c(5, 4, 4, 2), outer.mar = c(0, 0, 0, 0), picsize = 1, res = 100, 
                        cexlab = 1, propwidth = FALSE, xl = 1, osci = FALSE,  gr = FALSE,
                       sc = FALSE, line = TRUE, col = "#07889B", fill = adjustcolor("#07889B", alpha.f = 0.15), lty = 3, mar = 0.05, 
                       it = "jpeg", parallel = 1, path = NULL, pb = TRUE, fast.spec = FALSE, by.song = NULL, sel.labels = "selec", title.labels = NULL, dest.path = NULL, ...){
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(specreator)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
  # rename path for sound files
  names(opt.argms)[names(opt.argms) == "wav.path"] <- "path"
  
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
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) stop("'path' provided does not exist") 

    #check dest.path to working directory
  if (is.null(dest.path)) dest.path <- path else 
    if (!dir.exists(dest.path)) stop("'dest.path' provided does not exist") 
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  if (!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  # check song and element label
  if (!is.null(by.song)) if (!any(names(X) == by.song)) stop("'by.song' not found")
  if (!is.null(sel.labels)) if (!any(names(X) == sel.labels)) stop("'sel.labels' not found")
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop("'start' and 'end' must be numeric")
  
  #if any start higher than end stop
  if (any(X$end - X$start <= 0)) stop(paste("The start is higher than or equal to the end in", length(which(X$end - X$start <= 0)), "case(s)"))  
  
  # flim checking
  if (flim[1] != "frange")
  {
    
    if (!is.vector(flim)) stop("'flim' must be a numeric vector of length 2") else
      if (!length(flim) == 2) stop("'flim' must be a numeric vector of length 2") 
    
    # add bottom and top freq if not included
    if (!is.null(flim[1])){
      # top minus 1 kHz
      if (is.null(X$bottom.freq)) X$bottom.freq <- flim[1] - 1
      # top plus 1 kHz
      if (is.null(X$top.freq)) X$top.freq <- flim[2] + 1
    } else {    
      # negative bottom so bottom line is not plotted
      if (is.null(X$bottom.freq)) X$bottom.freq <- -1
      # if no top freq then make it 501 kHz (which is half the highest sampling rate (1 million) + 1)
      if (is.null(X$top.freq)) X$top.freq <- 501
    }  
    
  } else
  {if (!any(names(X) == "bottom.freq") & !any(names(X) == "top.freq")) stop("'flim' = frange requires bottom.freq and top.freq columns in X")
    if (any(is.na(c(X$bottom.freq, X$top.freq)))) stop("NAs found in bottom.freq and/or top.freq") 
    if (any(c(X$bottom.freq, X$top.freq) < 0)) stop("Negative values found in bottom.freq and/or top.freq") 
    if (any(X$top.freq - X$bottom.freq <= 0)) stop("top.freq should be higher than bottom.freq")
    }
  
  #if it argument is not "jpeg" or "tiff" 
  if (!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
  # error if not title.labels character
  if (!is.character(title.labels) & !is.null(title.labels)) stop("'title.labels' must be a character string")
  
  #missing label columns
  if (!all(title.labels %in% colnames(X)))
    stop(paste(paste(title.labels[!(title.labels %in% colnames(X))], collapse=", "), "label column(s) not found in data frame"))
  
  #return warning if not all sound files were found
  if (!is_extended_selection_table(X))
  {
    recs.wd <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)
  if (length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files))) 
    (paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
           ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% recs.wd) 
  if (length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
    X <- X[d, , drop = FALSE]
  }
  }
  
  if (propwidth) picsize <- 1
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # by song
  if (!is.null(by.song)) {
    Y <- X
    X <- song_param(X = Y, song_colm = by.song, pb = FALSE)
    X$selec <- 1
    
    # fix extended selection table again
    if (is_extended_selection_table(Y)) X <- fix_extended_selection_table(X, Y)
    } else Y <- NULL
  
  #create function to run within Xapply functions downstream     
  specreFUN <- function(X, Y, i, mar, flim, xl, picsize, res, wl, ovlp, cexlab, by.song, sel.labels, pal, dest.path, fill){
    
    # Read sound files, initialize frequency and time limits for spectrogram
    r <- warbleR::read_wave(X = X, path = path, index = i, header = TRUE)
    f <- r$sample.rate
    t <- c(X$start[i] - mar, X$end[i] + mar)
    
    mar1 <- mar
    mar2 <- mar1 + X$end[i] - X$start[i]
    
    if (t[1] < 0) { 
      mar1 <- mar1  + t[1]
      mar2 <- mar2  + t[1]
      t[1] <- 0
    }
    
    if (t[2] > r$samples/f) t[2] <- r$samples/f
    
    # add low high freq
    if (flim[1] == "frange") flim <- range(c(X$bottom.freq[i], X$top.freq[i])) + c(-1, 1)
    
    fl <- flim #in case flim its higher than can be due to sampling rate
    if (fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
    if (fl[1] < 0) fl[1] <- 0
    
       # Spectrogram width can be proportional to signal duration
    if (propwidth) pwc <- (10.16) * ((t[2]-t[1])/0.27) * xl * picsize else pwc <- (10.16) * xl * picsize
    
    if (is.null(by.song)) fn <- paste(X$sound.files[i], "-", X$selec[i], ".", it, sep = "") else 
      if (by.song == "sound.files")
        fn <- paste(X$sound.files[i], ".", it, sep = "") else
      fn <- paste(X$sound.files[i], "-", X[i, by.song], ".", it, sep = "")
   
    img_wrlbr_int(filename = fn, path = dest.path, 
           width = pwc, height = (10.16) * picsize, units = "cm", res = res) 
    
    # Change relative heights of rows for spectrogram when osci = TRUE
    if (osci) hts <- c(3, 2) else hts <- NULL
    
    # Change relative widths of columns for spectrogram when sc = TRUE
    if (sc) wts <- c(3, 1) else wts <- NULL
    
    # Change inner and outer plot margins
    par(mar = inner.mar)
    par(oma = outer.mar)
    
    # Generate spectrogram using spectro_wrblr_int (modified from seewave::spectro)
  spectro_wrblr_int(wave = warbleR::read_wave(X = X, path = path, index = i, from = t[1], to = t[2]), f = f, wl = wl, ovlp = ovlp, heights = hts, wn = "hanning",
                     widths = wts, palette = pal, osc = osci, grid = gr, scale = sc, collab = "black", 
                     cexlab = cexlab, cex.axis = 1, flim = fl, tlab = "Time (s)", 
                     flab = "Frequency (kHz)", alab = "", trel = FALSE, fast.spec = fast.spec, ...)
    
    # Add title to spectrogram
    if (is.null(title.labels)){ 
      if (!is.null(by.song)) {
        if(by.song == "sound.files") 
          title(X$sound.files[i], cex.main = cexlab) else
        title(paste0(X$sound.files[i], "-", X[i, by.song]), cex.main = cexlab)
        }
      }else
        title(paste0(X[i, title.labels], collapse = " "), cex.main = cexlab) 
  
  
    # Plot lines to visualize selections (start and end of signal)
    if (line){  
      if (any(names(X) == "bottom.freq") & any(names(X) == "top.freq"))
      {   
        if (!is.null(by.song))
        {
          W <- Y[Y$sound.files == X$sound.files[i] & Y[, by.song] == X[i, by.song], ,  drop= FALSE]
          W$start <- W$start - X$start[i] + mar1
          W$end <- W$end - X$start[i] + mar1
        } else 
        { W <- X[i, , drop = FALSE]
          W$start <- mar1    
          W$end <- mar2
          }
          
        for(e in 1:nrow(W))  
          {
            # if freq columns are not provided
            ys <- if (is.null(W$top.freq)) fl[c(1, 2, 2, 1)] else
              c(W$bottom.freq[e], W$top.freq[e], W$top.freq[e], W$bottom.freq[e])
            
            #plot polygon
            polygon(x = rep(c(W$start[e], W$end[e]), each = 2), y = ys, lty = lty, border = col, col = fill, lwd = 1.2)
          
            if (!is.null(sel.labels)) text(labels= paste(W[e, sel.labels], collapse = "-"), x = (W$end[e] + W$start[e])/2, y = if (is.null(W$top.freq)) fl[2] - 2*((fl[2] - fl[1])/12) else W$top.freq[e], pos = 3)
            }  
        }
      
    }
    dev.off()
  }

  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  out <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(i) 
  { 
    specreFUN(X, Y, i, mar, flim, xl, picsize, res, wl, ovlp, cexlab, by.song, sel.labels, pal, dest.path, fill)
  }) 
}


##############################################################################################################
#' alternative name for \code{\link{specreator}}
#'
#' @keywords internal
#' @details see \code{\link{specreator}} for documentation. \code{\link{specreator}} will be deprecated in future versions.
#' @export

spectrograms <- specreator
