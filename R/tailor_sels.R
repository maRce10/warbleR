#' Interactive view of spectrograms to tailor selections 
#' 
#' \code{tailor_sels} produces an interactive spectrographic view in 
#' which the start/end times and frequency range of acoustic signals listed in a data frame can be adjusted.
#' @usage tailor_sels(X = NULL, wl = 512, flim = c(0,22), wn = "hanning", mar = 0.5,
#'  osci = TRUE, pal = reverse.gray.colors.2, ovlp = 70, auto.next = FALSE, pause = 1,
#'   comments = TRUE, path = NULL, frange = TRUE, fast.spec = FALSE, ext.window = TRUE,
#'   width = 15, height = 5, index = NULL, collevels = NULL, 
#'   title = c("sound.files", "selec"), ts.df = NULL, col = "#E37222", 
#'   alpha = 0.7, auto.contour = FALSE, ...)
#' @param X 'selection_table', 'extended_selection_table' object or data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The output of \code{\link{auto_detec}} can 
#' be used as the input data frame. Other data frames can be used as input, but must have at least the 4 columns mentioned above. Notice that, if an output file ("seltailor_output.csv") is found in the working directory it will be given priority over an input data frame.
#' @param wl A numeric vector of length 1 specifying the spectrogram window length. Default is 512.
#' @param flim A numeric vector of length 2 specifying the frequency limit (in kHz) of 
#'   the spectrogram, as in the function \code{\link[seewave]{spectro}}. 
#'   Default is c(0,22).
#' @param wn A character vector of length 1 specifying the window function (by default "hanning"). 
#' See function \code{\link[seewave]{ftwindow}} for more options.
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the 
#' start and end points of the selections to define spectrogram limits. Default is 0.5.
#' @param osci Logical argument. If \code{TRUE} adds a oscillogram whenever the spectrograms are produced 
#'   with higher resolution (see seltime). Default is \code{TRUE}.
#'   The external program must be closed before resuming analysis. Default is \code{NULL}.
#' @param pal A color palette function to be used to assign colors in the 
#'   plot, as in \code{\link[seewave]{spectro}}. Default is reverse.gray.colors.2. See Details.
#' @param ovlp Numeric vector of length 1 specifying the percent overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70.
#' @param auto.next Logical argument to control whether the functions moves automatically to the 
#' next selection. The time interval before moving to the next selection is controlled by the 'pause' argument. Ignored if \code{ts.df = TRUE}. 
#' @param pause Numeric vector of length 1. Controls the duration of the waiting period before 
#' moving to the next selection (in seconds). Default is 1. 
#' @param comments Logical argument specifying if 'sel.comment' (when in data frame) should be included 
#' in the title of the spectrograms. Default is \code{TRUE}.
#' @param path Character string containing the directory path where the sound files are located.
#' @param frange Logical argument specifying whether limits on frequency range should be
#'  recorded. 
#' If \code{TRUE} (default) time and frequency limits are recorded.
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR:specCols]{gray.1}}, \code{\link[monitoR:specCols]{gray.2}}, 
#' \code{\link[monitoR:specCols]{gray.3}}, \code{\link[monitoR:specCols]{topo.1}} and \code{\link[monitoR:specCols]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast' spectrograms. Palette colors \code{\link[monitoR:specCols]{gray.1}}, \code{\link[monitoR:specCols]{gray.2}}, 
#' \code{\link[monitoR:specCols]{gray.3}} offer 
#' decreasing darkness levels. 
#' @param ext.window Logical. If \code{TRUE} then and external graphic window is used. Default 
#' dimensions can be set using the 'width' and 'height' arguments. Default is \code{TRUE}.
#' @param width Numeric of length 1 controlling the width of the external graphic window. Ignored
#' if \code{ext.window = FALSE}. Default is 15.
#' @param height Numeric of length 1 controlling the height of the external graphic window.
#' Ignored if \code{ext.window = FALSE}. Default is 5.
#' @param index Numeric vector indicating which selections (rows) of 'X' should be tailored. 
#'  Default is \code{NULL}. Ignored when the process is resumed. This can be useful when combined
#'  with \code{\link{filtersels}}) output (see 'index' argument in \code{\link{filtersels}}).
#' @param collevels Numeric. Set of levels used to partition the amplitude range (see 
#'  \code{\link[seewave]{spectro}}).
#' @param title Character vector with the names of the columns to be included in the title for each
#' selection.
#' @param ts.df Optional. Data frame with frequency contour time series of signals to be tailored. If provided then 
#' 'autonext' is set to \code{FALSE}. Default is \code{NULL}. The data frame must include the 'sound.files' and 'selec' 
#' columns for the same selections included in 'X'.
#' @param col Character vector defining the color of the points when 'ts.df' is provided. Default is "#E37222" (orange).
#' @param alpha Numeric of length one to adjust transparency of points when adjusting frequency contours.
#' @param auto.contour Logical. If \code{TRUE} contours are displayed automatically
#' (without having to click on 'contour'). Note that adjusting the selection box 
#' (frequency/time limits) won't be available. Default is \code{FALSE}. Ignored if
#' 'ts.df' is not provided. 
#' @param ... Additional arguments to be passed to the internal spectrogram creating function for customizing graphical output. The function is a modified version of \code{\link[seewave]{spectro}}, so it takes the same arguments. 
#' @return data frame similar to X with the and a .csv file saved in the working directory with start and end time of 
#'   selections.
#' @export
#' @name tailor_sels
#' @examples
#' \dontrun{
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#' 
#' tailor_sels(X =  lbh_selec_table, flim = c(1,12), wl = 300, auto.next = TRUE, path = tempdir())
#' 
#' # Read output .csv file
#' seltailor.df <- read.csv(file.path(tempdir(), "seltailor_output.csv"))
#' seltailor.df
#' 
#' # check this directory for .csv file after stopping function
#' tempdir()
#' }
#' @details This function produces an interactive spectrographic
#'  view in which users can select new time/frequency 
#'  coordinates the selections. 4 "buttons" are provided at the upper right side of the spectrogram that
#'   allow to stop the analysis (stop symbol, a solid rectangle), go to the next sound file (">>"), return to the 
#'   previous selection ("<<") or delete 
#'   the current selection ("X"). An additional "button" to tailored frequency contour is shown
#'   when 'ts.df' is provided. The button contains a symbol with a 4 point contour. When a unit has been selected, the function plots 
#'   dotted lines in the start and end of the selection in the spectrogram (or a box if 
#'   \code{frange = TRUE}). Only the last selection is kept for each
#'    selection that is adjusted. The function produces a .csv file (seltailor_output.csv) 
#'    with the same information than the input data frame, except for the new time 
#'    coordinates, plus a new column (X$tailored) indicating if the selection 
#'   has been tailored. The file is saved in the working directory  and is updated every time the user
#'    moves into the next sound file (">>") or stop the process 
#'  (stop "button"). It also return the same data frame as and object in the R environment.
#'    If no selection is made (by clicking on ">>") the 
#'  original time/frequency coordinates are kept. When resuming the process (after "stop" and re-running 
#'  the function in the same working directory), the function will continue working on the
#'  selections that have not been analyzed. When deleting a file (X button) an orange "X" when returning to that selection. If X is used again the selection is recovered.

#'  The function also displays a progress bar right on
#'  top of the spectrogram. The zoom can be adjusted by setting the \code{mar} argument.
#'  To fix contours a data.frame containing the 'sound.files' and 'selec' columns as in 'X' as well 
#'  as the frequency values at each contour step must be provided. The function plots points corresponding to the 
#'  time/frequency coordinates of each element of the contour. Clicking on the spectrogram will substitute the 
#'  frequency value of the points. The contour point closest in time to the "click" will be replaced by the 
#'  frequency value of the "click". 
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on jul-5-2016 (MAS)

tailor_sels <- function(X = NULL, wl = 512, flim = c(0,22), wn = "hanning", mar = 0.5,
                       osci = TRUE, pal = reverse.gray.colors.2, ovlp = 70, auto.next = FALSE,
                       pause = 1, comments = TRUE, path = NULL, frange = TRUE, fast.spec = FALSE,
                       ext.window = TRUE, width = 15, height = 5, index = NULL,
                       collevels = NULL, title = c("sound.files", "selec"), 
                       ts.df = NULL, col = "#E37222", alpha = 0.7, auto.contour = FALSE, ...)
{
  
  # reset warnings
  on.exit(options(warn = .Options$warn), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(tailor_sels)
  
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
    if (!dir.exists(path)) 
      stop("'path' provided does not exist") else
        path <- normalizePath(path)
  
  # unique path for csv file
  csv.path <- path
  
  # no autonext if ts.df provided
  if (auto.next & !is.null(ts.df)) {
    write(file = "", x = "'auto.next' not available when 'ts.df' is provided") 
    auto.next <- FALSE
  }
  
  # merge ts.df and X
  if (!is.null(ts.df))
  {  if (is.data.frame(ts.df))
    {
    if (nrow(X) != nrow(ts.df)) stop("number of rows in 'ts.df' and 'X' do not match")
    
    # fix and extract colnames from ts.df
    names(ts.df)[-c(1:2)] <- gsub("_|-", ".", names(ts.df)[-c(1:2)])
    
    # get extra names other than sound.files and selec 
    ncl <- names(ts.df)[-c(1:2)]
    
    # stop if not alls selections have contour data
    if(!identical(paste(ts.df$sound.files, ts.df$selec), paste(X$sound.files, X$selec))) 
      stop("Not all selections in 'X' have countour data in 'ts.df'")
    
    # add contour data to X
    if (is_extended_selection_table(X)){
      
      # merge to a different object name
      X2 <- merge(X, ts.df, by = c("sound.files", "selec"))
      
      # and add acousitc and metadata
      X <- fix_extended_selection_table(X = X2, Y = X)
      
    } else
    X <- merge(X, ts.df, by = c("sound.files", "selec"))
    
    } else 
      { # if ts.df is a list
      if (nrow(X) != length(ts.df)) stop("number of rows in 'X' and length of 'ts.df' do not match")
    
    mxts <- max(sapply(ts.df, nrow))  
    
    out <- lapply(1:length(ts.df), function(x)
      {
      z <- ts.df[[x]]
     df <- data.frame(t(c(z$frequency, rep(NA, mxts - nrow(z)), z$absolute.time, rep(NA, mxts - nrow(z)))))
     colnames(df) <- paste0(colnames(df), rep(c("...FREQ", "...TIME"), each = mxts))
    df$sound.files.selec <- names(ts.df)[x] 
    return(df)  
    })
    
    ts.df2 <- do.call(rbind, out)
    X$sound.files.selec <- paste(X$sound.files, X$selec, sep = "-")
    X <- merge(X, ts.df2, by = c("sound.files.selec"))
    ncl <- names(ts.df2)[-ncol(ts.df2)]
    }
  } else auto.contour <- FALSE
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X))) stop("X is not of a class 'data.frame' or 'selection_table'")
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop("'start' and 'end' must be numeric")
  
  #if any start higher than end stop
  if (any(X$end - X$start <= 0)) stop(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))
  
  #check title columns
  if (any(!title %in% names(X))) stop(paste('title column(s)', title[!title %in% names(X)], "not found"))
  
  # stop if not all sound files were found
  if (!is_extended_selection_table(X)){
    fs <- list.files(path = path,pattern = "\\.wav$", ignore.case = TRUE)
  if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
    stop(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
               ".wav file(s) not found"))
  } else path <- NULL # if is extended then no path needed
  
  if (frange & !all(any(names(X) == "bottom.freq"), any(names(X) == "top.freq")))
    X$top.freq <- X$bottom.freq <- NA
  
  # if working on a extended selection table
  if (!file.exists(file.path(csv.path, "seltailor_output.csv")))
  {
    X$tailored <- ""
  X$tailored <- as.character(X$tailored)
  if (!is.null(index))   X$tailored[!1:nrow(X) %in% index] <- "y"
  write.csv(X[X$tailored != "delete", ], file.path(csv.path, "seltailor_output.csv"), row.names =  FALSE)  
  } else {
    # tell user file already existed
    write(file = "", x = "'seltailor_output.csv' found in working directory, resuming tailoring ...")

    if (is_extended_selection_table(X)){
      # read with different object name
      X2 <- read.csv(file.path(csv.path, "seltailor_output.csv"), stringsAsFactors = FALSE)
     
      # and add acousitc and metadata
      X <- fix_extended_selection_table(X = X2, Y = X)
      
       } else
        X <- read.csv(file.path(csv.path, "seltailor_output.csv"), stringsAsFactors = FALSE) 
       # stop if all selections were analyzed
       if (any(is.na(X$tailored))) X$tailored[is.na(X$tailored)] <-""
       if (all(any(!is.na(X$tailored)), sum(X$tailored %in% c("y", "delete")) == nrow(X))) {
         
         write(file = "", x = "all selections have been analyzed")
         stop() 
       } 
        
       # add with time series data    
        if(!is.null(ts.df)){
          if (any(!names(ts.df) %in% names(X)))
            stop("'seltailor_output.csv' file in working directory does not contain frequency contour columns")  
        
          ncl <- ncl[!ncl %in% c("sound.files", "selec")]
        }
  }
  
  dn <- 1:nrow(X)
  if (any(!is.na(X$tailored))) if (length(which(X$tailored == "y")) > 0) 
    dn <- which(X$tailored != "y")  
  
  #set external window function
  if (any(Sys.info()[1] == c("Linux", "Windows"))) extwin <- grDevices::X11 else extwin <- grDevices::quartz
  
  #start external graphic device
  if (ext.window)  extwin(width = width, height = height)
  
  #set original number of sels to tailor
  org.sel.n <- nrow(X)
  
  #this first loop runs over selections
  h <- 1
  
  repeat{
    j <- dn[h]
    
    if (exists("prev.plot")) rm(prev.plot)
    rec <- warbleR::read_wave(X, index = j, path = path, header = TRUE)
    main <- do.call(paste, as.list(X[j, names(X) %in% title])) 
    
    f <- rec$sample.rate #for spectro display
    fl<- flim #in case flim its higher than can be due to sampling rate
    if (fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
    len <- rec$samples/f  #for spectro display 
    start <- numeric() #save results
    end <- numeric() #save results
    selcount <- 0
    tlim <- c(X$start[j] - mar, X$end[j] + mar)
    if (tlim[1]<0) tlim[1]<-0
    if (tlim[2]>rec$samples/f) tlim[2]<-rec$samples/f
    org.start <- X$start[j] #original start value
    
    #set an undivided window
    par(mfrow = c(1,1), mar = c(3, 3, 1.8, 0.1))
    
    #create spectrogram
    spectro_wrblr_int(warbleR::read_wave(X = X, index = j, path = path, from =  tlim[1], to = tlim[2]), 
                      f = f, wl = wl, ovlp = ovlp, wn = wn, heights = c(3, 2), 
                      osc = osci, palette =  pal, main = NULL, axisX= TRUE, grid = FALSE, collab = "black", alab = "", fftw= TRUE, colwave = "#07889B", collevels = collevels,
                      flim = fl, scale = FALSE, axisY= TRUE, fast.spec = fast.spec, ...
                      )
    if (!osci)
    {
      mtext("Time (s)", side=1, line= 1.8)
      mtext("Frequency (kHz)", side = 2,  xpd = TRUE, las = 0)
    }
    
    # title
    mtext(main, side = 3, line = 0.65,  xpd = NA, las = 0, font = 2, cex = 1.3, col = "#062F4F")
    
    #progress bar
    prct <- 1 - (sum(X$tailored == "") / org.sel.n)
    x2 <- prct * diff(tlim)
    y <- fl[2] + diff(fl) *  0.022
    lines(x = c(0, x2), y = rep(y, 2), lwd = 7, col = adjustcolor("#E37222", alpha.f = 0.6), xpd = TRUE)
    text(x = x2 + (diff(tlim) * 0.017), y = y, xpd = TRUE, labels = paste0(floor(prct * 100), "%"), col = "#E37222", cex = 0.8)
    
    options(warn = -1)
    
    #add lines of selections on spectrogram
    if ((any(is.na(X$bottom.freq[j]), is.na(X$top.freq[j]))))
      polygon(x = rep(c(X$start[j], X$end[j]) - tlim[1], each = 2), y = c(fl, sort(fl, decreasing = TRUE)), lty = 3, border = "#07889B", lwd = 1.2, col = adjustcolor("#07889B", alpha.f = 0.15))  else
        polygon(x = rep(c(X$start[j], X$end[j]) - tlim[1], each = 2), y = c(c(X$bottom.freq[j], X$top.freq[j]),c(X$top.freq[j], X$bottom.freq[j])), lty = 3, border = "#07889B", lwd = 1.2, col = adjustcolor("#07889B", alpha.f = 0.15)) 
    
    #add buttons
    xs <- grconvertX(x = c(0.94, 0.94, 0.99, 0.99), from = "npc", to = "user")
    
    labels <- c("stop", "next", "previous", "delete")
    if (!is.null(ts.df)) labels <- c(labels, "contour")
    
    mrg <- 0.05
    cpy <- sapply(0:(length(labels)- 1), function(x) {0.95 - (x * 3 * mrg)})
    
    #mid position ofbuttons in c(0, 1) range
    ys <- c(-mrg, mrg, mrg, -mrg)
    
    grYs <- lapply(1:length(cpy), function(x) 
    {
      grY <- grconvertY(y = cpy[x]- ys, from = "npc", to = "user")
      polygon(x = xs, y = grY, border = "#4ABDAC", col = adjustcolor("#E37222", alpha.f = 0.22), lwd = 2)
    
      # plot symbols
      if (labels[x] == "stop") 
        points(x = mean(xs), y = mean(grY), pch = 15, cex = 1.5, col = "#4ABDAC")
        
      if (labels[x] == "next") 
        text(x = mean(xs), y = mean(grY), labels = ">>", cex = 1.2, font = 2, col = "#4ABDAC")
      
      if (labels[x] == "previous") 
        text(x = mean(xs), y = mean(grY), labels = "<<", cex = 1.2, font = 2, col = "#4ABDAC")
    
      if (labels[x] == "delete") 
        points(x = mean(xs), y = mean(grY), pch = 4, cex = 1.5, lwd = 3, col = if (X$tailored[j] != "delete") "#4ABDAC"else "#E37222")
        
      if (labels[x] == "contour") {
        points(x = c(min(xs) + ((max(xs) - min(xs)) / 4.2), min(xs) + ((max(xs) - min(xs)) / 2.5), min(xs) + ((max(xs) - min(xs)) / 1.8), min(xs) + ((max(xs) - min(xs))) / 1.4), y = c(((max(grY) - min(grY)) / 2), ((max(grY) - min(grY)) / 5), ((max(grY) - min(grY)) / 3), ((max(grY) - min(grY)) / 6)) * c(-1, 1, -1, 1) + grY[c(1, 2, 4, 3)], pch = 20, col = "#4ABDAC")
      
        lines(x = c(min(xs) + ((max(xs) - min(xs)) / 4.2), min(xs) + ((max(xs) - min(xs)) / 2.5), min(xs) + ((max(xs) - min(xs)) / 1.8), min(xs) + ((max(xs) - min(xs))) / 1.4), y = c(((max(grY) - min(grY)) / 2), ((max(grY) - min(grY)) / 5), ((max(grY) - min(grY)) / 3), ((max(grY) - min(grY)) / 5)) * c(-1, 1, -1, 1) + grY[c(1, 2, 4, 3)], col = "#4ABDAC")
      }
      
      return(grY)   
    })
    
    #ask users to select what to do next (1 click)
    if (!auto.contour) xy2 <- xy <- locator(n = 1, type = "n") else
    xy2 <- xy <- list(x = 0, y = 0)
  
    #if selected is lower than 0 make it 
    xy$x[xy$x < 0] <- 0  
    xy$y[xy$y < 0] <- 0 
    xy2$x[xy2$x < 0] <- 0  
    xy2$y[xy2$y < 0] <- 0 
    
    # fix freq
    if (!is.null(ts.df))
      if (xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[5]]) & xy$y < max(grYs[[5]]) | auto.contour)
      {
        Y <- fix_cntr_wrblr_int(X, j, ending.buttons = 1:4, ncl, tlim, xs, grYs, flim = fl, col, alpha, l = !is.data.frame(ts.df))
        X[, ncl] <- Y$ts.df
        xy <- Y$xy
        rm(Y)
        
        if (selcount > 0) X$tailored[j] <- "y"
        write.csv(X[X$tailored != "delete", ], file.path(csv.path, "seltailor_output.csv"), row.names =  FALSE)
      }
    
    #if delete
    if (xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[4]]) & xy$y < max(grYs[[4]]))
    {    
      # delete row if not deleted otherwise undelee
      if (X$tailored[j] != "delete")
              X$tailored[j] <- "delete" else
                X$tailored[j] <- "y"
      write.csv(X[X$tailored != "delete", ], file.path(csv.path, "seltailor_output.csv"), row.names =  FALSE)  
      if (sum(X$tailored %in% c("y", "delete")) == nrow(X)) {
        dev.off()
        #return X
        return(X[X$tailored != "delete", ])
        
        write(file = "", x = "all selections have been analyzed")
        stop() 
      } 
      h <- h + 1
    }
    
    #if previous
    if (xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[3]]) & xy$y < max(grYs[[3]]))
    {    
      h <- h - 1
      if (h == 0) {h <- 1
      write(file = "", x = "This selection was the first one during the selection procedure (can't go further back)")
      }
    }
    
    #if next sel
    if (xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[2]]) & xy$y < max(grYs[[2]]))
    {    
      X$tailored[j] <- "y"
      write.csv(X[X$tailored != "delete", ], file.path(csv.path, "seltailor_output.csv"), row.names =  FALSE)  
      
      if (sum(X$tailored %in% c("y", "delete")) == nrow(X)) {
        dev.off()
        #return X
        return(X[X$tailored != "delete", ])
        
        write(file = "", x = "all selections have been analyzed")
        stop() 
      } 
      h <- h + 1
    }
    
    # stop
    if (xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[1]]) & xy$y < max(grYs[[1]]))
    {
      dev.off()
      if (selcount > 0) X$tailored[j] <- "y"
      write.csv(X[X$tailored != "delete", ], file.path(csv.path, "seltailor_output.csv"), row.names =  FALSE)
      #return X
      return(X[X$tailored != "delete", ])
      
      write(file = "", x = "Stopped by user")
      stop() 
    } 
    
    # while not inside buttons
    out <- sapply(1:length(labels), function(w) out  <- !all(xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[w]]) & xy$y < max(grYs[[w]])))
    
    while(all(out))
    {
      
      #select second point
      xy <- locator(n = 1, type = "n")
      
      #if selected is lower than 0 make it 
      xy$x[xy$x < 0] <- 0  
      xy$y[xy$y < 0] <- 0 
      
      # fix freq
      if (!is.null(ts.df))
        if (xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[5]]) & xy$y < max(grYs[[5]]))
        {
          Y <- fix_cntr_wrblr_int(X, j, ending.buttons = 1:4, ncl, tlim, xs, grYs, flim = fl, col, alpha, l = !is.data.frame(ts.df))
          X[, ncl] <- Y$ts.df
          xy <- Y$xy
          rm(Y)
          
          if (selcount > 0) X$tailored[j] <- "y"
          write.csv(X[X$tailored != "delete", ], file.path(csv.path, "seltailor_output.csv"), row.names =  FALSE)
        }
      
      #if delete
      if (xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[4]]) & xy$y < max(grYs[[4]]))
      {    
        # delete row
        X$tailored[j] <- "delete"
        write.csv(X[X$tailored != "delete", ], file.path(csv.path, "seltailor_output.csv"), row.names =  FALSE)  
        if (sum(X$tailored %in% c("y", "delete")) == nrow(X)) {
          dev.off()
          
          #return X
          return(X[X$tailored != "delete", ])
          
          write(file = "", x = "all selections have been analyzed")
          stop() 
        } else {
          h <- h + 1
          break}
      }
      
      #if previous
      if (xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[3]]) & xy$y < max(grYs[[3]]))
      {    
        h <- h - 1
        if (h == 0) {
          h <- 1
          write(file = "", x = "This selection was the first one during the selection procedure (can't go further back)")
        }
        break
      }
      
      #if next sel
      if (xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[2]]) & xy$y < max(grYs[[2]]))
      {    
        X$tailored[j] <- "y"
        write.csv(X[X$tailored != "delete", ], file.path(csv.path, "seltailor_output.csv"), row.names =  FALSE)  
        
        if (sum(X$tailored %in% c("y", "delete")) == nrow(X)) {
          dev.off()
          
          #return X
          return(X[X$tailored != "delete", ])
          
          options(show.error.messages=FALSE)
          write(file = "", x = "all selections have been analyzed")
          stop() 
        } else  {
          h <- h + 1  
          break}
      }
      
      # stop
      if (xy$x > min(xs) & xy$x < max(xs) & xy$y > min(grYs[[1]]) & xy$y < max(grYs[[1]]))
      {dev.off()
        if (selcount > 0) X$tailored[j] <- "y"
        write.csv(X[X$tailored != "delete", ], file.path(csv.path, "seltailor_output.csv"), row.names =  FALSE)
      
        
        #return X
        return(X[X$tailored != "delete", ])
        
        write(file = "", x = "Stopped by user")
        stop()}
      
      if (exists("prev.plot")) xy2 <- locator(n = 1, type = "n")
      
      xy3 <- list()
      xy3$x <- c(xy$x, xy2$x)
      xy3$y <- c(xy$y, xy2$y)
      
      if (exists("prev.plot")) replayPlot(prev.plot) else prev.plot <- recordPlot()   
      
      if (frange) polygon(x = rep(sort(xy3$x), each = 2), y = c(sort(xy3$y),sort(xy3$y, decreasing = TRUE)), lty = 3, border = "#E37222", lwd = 1.2, col = adjustcolor("#EFAA7B", alpha.f = 0.15)) else
        polygon(x = rep(sort(xy3$x), each = 2), y = c(fl,sort(fl, decreasing = TRUE)), lty = 3, border = "#E37222", lwd = 1.2, col = adjustcolor("#EFAA7B", alpha.f = 0.15))
      
      X$start[j] <-  tlim[1] + min(xy3$x) 
      X$end[j] <-  tlim[1] + max(xy3$x)
      X$tailored[j] <- "y"
      if (frange) {
        X$bottom.freq[j] <- min(xy3$y)  
        if (min(xy3$y) < 0) X$bottom.freq[j] <- 0  
        X$top.freq[j] <- max(xy3$y)  
      }
      selcount <- selcount + 1
      
      #auto next
      if (auto.next){
        X$start[j] <-  tlim[1] + min(xy3$x) 
        X$end[j] <-  tlim[1] + max(xy3$x)
        
        # fix start if negative
        if (X$start[j] < 0)
          X$start[j] <- 0
        
        # fix if higher than file duration in extended selection tables
        if (is_extended_selection_table(X) & X$end[j] > duration(attr(X, "wave.objects")[[which(names(attr(X, "wave.objects")) == X$sound.files[j])[1]]]))
              X$end[j] <- duration(attr(X, "wave.objects")[[which(names(attr(X, "wave.objects")) == X$sound.files[j])[1]]])
        
        if (frange) {
          X$bottom.freq[j] <- min(xy3$y)  
          if (min(xy3$y) < 0) X$bottom.freq[j] <- 0  
          X$top.freq[j] <- max(xy3$y)
        }
        
        # save selections
        write.csv(X[X$tailored != "delete", ], file.path(csv.path, "seltailor_output.csv"), row.names =  FALSE)
        
        if (sum(X$tailored %in% c("y", "delete")) == nrow(X)) { 
          dev.off()
          
          #return X
          return(X[X$tailored != "delete", ])
          
          write(file = "", x = "all selections have been analyzed")
          stop() 
        } else {
          Sys.sleep(pause) 
          h <- h + 1
          break}
      }
    } 
  }
}


##############################################################################################################
#' alternative name for \code{\link{tailor_sels}}
#'
#' @keywords internal
#' @details see \code{\link{tailor_sels}} for documentation. \code{\link{seltailor}} will be deprecated in future versions.
#' @export

seltailor <- tailor_sels
