#' Create long spectrograms of whole sound files
#' 
#' \code{lspec} produces image files with spectrograms of whole sound files split into multiple 
#'   rows.
#' @usage lspec(X = NULL, flim = c(0,22), sxrow = 5, rows = 10, collevels = seq(-40, 0, 1), 
#' ovlp = 50, parallel = 1, wl = 512, gr = FALSE, pal = reverse.gray.colors.2, 
#' cex = 1, it = "jpeg", flist = NULL, overwrite = TRUE, path = NULL, pb = TRUE, 
#' fast.spec = FALSE, labels = "selec", horizontal = FALSE, song = NULL, suffix = NULL, ...) 
#' @param X 'selection_table' object or any data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). If given, a transparent box is  plotted around each selection and the selections are labeled with the selection number 
#' (and selection comment, if available). Default is \code{NULL}.Alternatively, it can also take the output of \code{\link{xcorr}} or \code{\link{autodetec}} (when 'output' is a 'list', see \code{\link{xcorr}} or \code{\link{autodetec}}). If supplied a secondary row is displayed under each spectrogram showing the detection (either cross-correlation scores or wave envelopes) values across time.
#' @param flim A numeric vector of length 2 indicating the highest and lowest 
#'   frequency limits (kHz) of the spectrogram, as in 
#'   \code{\link[seewave]{spectro}}. Default is c(0,22).
#' @param sxrow A numeric vector of length 1. Specifies seconds of spectrogram
#'   per row. Default is 5.
#' @param rows A numeric vector of length 1. Specifies number of rows per 
#'   image file. Default is 10.
#' @param collevels A numeric vector of length 3. Specifies levels to partition the 
#'   amplitude range of the spectrogram (in dB). The more levels the higher the
#'   resolution of the spectrogram. Default is seq(-40, 0, 1).
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 50. High values of ovlp 
#'   slow down the function but produce more accurate selection limits (when X is provided). 
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param gr Logical argument to add grid to spectrogram. Default is \code{FALSE}.
#' @param pal Color palette function for spectrogram. Default is reverse.gray.colors.2. See 
#' \code{\link[seewave]{spectro}} for more palettes.
#' @param cex A numeric vector of length 1 giving the amount by which text 
#'   (including sound file and page number) should be magnified. Default is 1.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param flist character vector or factor indicating the subset of files that will be analyzed. Ignored
#' if X is provided.
#' @param overwrite Logical argument. If \code{TRUE} all selections will be analyzed again 
#'   when code is rerun. If \code{FALSE} only the selections that do not have a image 
#'   file in the working directory will be analyzed. Default is \code{FALSE}.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR:specCols]{gray.1}}, \code{\link[monitoR:specCols]{gray.2}}, 
#' \code{\link[monitoR:specCols]{gray.3}}, \code{\link[monitoR:specCols]{topo.1}} and \code{\link[monitoR:specCols]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast' spectrograms. Palette colors \code{\link[monitoR:specCols]{gray.1}}, \code{\link[monitoR:specCols]{gray.2}}, 
#' \code{\link[monitoR:specCols]{gray.3}} offer 
#' decreasing darkness levels. 
#' @param labels Character string with the name of the column(s) for selection 
#' labeling. Default is 'selec'. Set to \code{NULL} to remove labels.
#' @param horizontal Logical. Controls if the images are produced as horizontal or vertical pages. Default is \code{FALSE}.
#' @param song Character string with the name of the column to used as a label a for higher 
#' organization level in the song (similar to 'song_colm' in \code{\link{song_param}}). If supplied then lines above the selections belonging to the same
#' 'song' are plotted. Ignored if 'X' is not provided.
#' @param suffix Character vector of length 1. Suffix for the output image file (to be added at the end of the default file name). Default is \code{NULL}.
#' @param ... Additional arguments for image formatting. It accepts 'width', 'height' (which will overwrite 'horizontal') and 'res' as in \code{\link[grDevices]{png}}.
#' @return image files with spectrograms of whole sound files in the working directory. Multiple pages
#' can be returned, depending on the length of each sound file. 
#' @export
#' @name lspec
#' @details The function creates spectrograms for complete sound files, printing
#'   the name of the sound files and the "page" number (p1-p2...) at the upper 
#'   right corner of the image files. If 'X' is
#'   supplied, the function delimits and labels the selections. 
#'   This function aims to facilitate visual inspection of multiple files as well as visual classification 
#'   of vocalization units and the analysis of animal vocal sequences.
#' @seealso \code{\link{lspec2pdf}}, \code{\link{catalog2pdf}}, \code{\link{xcorr}}, \code{\link{autodetec}}
#' \href{https://marce10.github.io/2017/01/07/Create_pdf_files_with_spectrograms_of_full_recordings.html}{blog post on spectrogram pdfs}
#' @examples
#' \dontrun{
#' # Save to temporary working directory
#' 
#' 
#' # save sound file examples
#' data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) 
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' 
#' lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300, path = tempdir())
#' 
#' # including selections
#' lspec(sxrow = 2, rows = 8, X = lbh_selec_table, pal = reverse.heat.colors, overwrite = TRUE,
#'  wl = 300, path = tempdir())
#' 
#' #check this floder
#' ,tempdir()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on mar-13-2018 (MAS)

lspec <- function(X = NULL, flim = c(0, 22), sxrow = 5, rows = 10, collevels = seq(-40, 0, 1),  ovlp = 50, parallel = 1, 
                  wl = 512, gr = FALSE, pal = reverse.gray.colors.2, cex = 1, it = "jpeg", flist = NULL, overwrite = TRUE, path = NULL, pb = TRUE, fast.spec = FALSE, labels = "selec", horizontal = FALSE, song = NULL, suffix = NULL, ...) {
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(lspec)
  
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
  
  #read files
  files <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)  
  
  #stop if files are not in working directory
  if (length(files) == 0) stop("no .wav files in working directory")
  
  #subet based on file list provided (flist)
  if (!is.null(flist)) files <- files[files %in% flist]
  if (length(files) == 0)  stop("selected .wav files are not in working directory")
  
  # set W to null by default (this is the detection data.frame)
  W <- NULL
  
  # if X provided and comes from xcorr 
  if (!is.null(X))
  if (is(X, "xcorr.output")) {  
    # no cutoff in only xcorr
    cutoff <- NA
    
    # extract scores
    W <- X$scores
    
    # remove whole.file from sound file name
    W$sound.files <- gsub("-whole.file", "", W$sound.files)

    # set X to null
    X <- NULL
    }
  
  # if X is not from xcorr.output
  if (!is.null(X)) {
    
    # if is a data frame or st/est
    if (!is(X, "find_peaks.output") & !is(X, "autodetec.output")) {
      #list only files in X
      files <- files[files %in% X$sound.files]
      
      #if X is not a data frame
      if (!any(is.data.frame(X), is_selection_table(X))) stop("X is not of a class 'data.frame' or 'selection_table'")
      
      #if there are NAs in start or end stop
      if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end columns")
      
      #check if all columns are found
      if (any(!(c("sound.files", "selec", "start", "end") %in% colnames(X)))) 
        stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                       "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
      
      #if end or start are not numeric stop
      if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop("'start' and 'end' must be numeric")
      
      #if any start higher than end stop
      if (any(X$end - X$start <= 0)) stop(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))  
      
      }
      
    # if coming from find_peaks
    if (is(X, "find_peaks.output")) {  
      
      # cut off for detection lines
      cutoff <- X$cutoff
      
      # get time contours
      W <- X$scores
      
      # remove whole.file from sound file name
      W$sound.files <- gsub("-whole.file", "", W$sound.files)
      
      # leave only wav file names
      if (any(!grepl("\\.wav$", ignore.case = TRUE, W$sound.files)))
        W$sound.files <- substr(x = W$sound.files, start = 0, stop =  sapply(gregexpr(pattern = "\\.wav", ignore.case = TRUE, W$sound.files), "[[", 1) + 3)
      
      
      # get selection table and overwrite X
      X <- X$selection.table
    
      # add label of type of detection
      X$type <- "find_peaks"
      
      #list only files in W
      files <- files[files %in% W$sound.files]
      }
    
    # if coming from autodetec
    if (is(X, "autodetec.output")){ 
      
      
      # cut off for detection lines
      if (is.null(X$bottom.line.thresholds))
      cutoff <- X$parameters$threshold else
        cutoff <- X$bottom.line.thresholds
    
      # get time contours
      W <- X$envelopes
      
      # rename column with contours
      names(W)[names(W) == "amplitude"] <- "scores"
        
      # get selection table and overwrite X
      X <- X$selection.table
    
      # add label of type of detection
      X$type <- "autodetec"
      
      #list only files in W
      files <- files[files %in% W$sound.files]
      
    }
    
  #stop if files are not in working directory
  if (length(files) == 0) stop(".wav files in X are not in working directory")
    } 
 
#if flim is not vector or length!=2 stop
  if (is.null(flim)) stop("'flim' must be a numeric vector of length 2") else {
    if (!is.vector(flim)) stop("'flim' must be a numeric vector of length 2") else{
      if (!length(flim) == 2) stop("'flim' must be a numeric vector of length 2")}}   
  
  #if wl is not vector or length!=1 stop
  if (is.null(wl)) stop("'wl' must be a numeric vector of length 1") else {
    if (!is.vector(wl)) stop("'wl' must be a numeric vector of length 1") else{
      if (!length(wl) == 1) stop("'wl' must be a numeric vector of length 1")}}  
  
  #if sxrow is not vector or length!=1 stop
  if (is.null(sxrow)) stop("'sxrow' must be a numeric vector of length 1") else {
    if (!is.vector(sxrow)) stop("'sxrow' must be a numeric vector of length 1") else{
      if (!length(sxrow) == 1) stop("'sxrow' must be a numeric vector of length 1")}}  
  
  #if rows is not vector or length!=1 stop
  if (is.null(rows)) stop("'rows' must be a numeric vector of length 1") else {
    if (!is.vector(rows)) stop("'rows' must be a numeric vector of length 1") else{
      if (!length(rows) == 1) stop("'rows' must be a numeric vector of length 1")}}  
  
  #if picsize is not vector or length!=1 stop
  if (is.null(cex)) stop("'picsize' must be a numeric vector of length 1") else {
    if (!is.vector(cex)) stop("'picsize' must be a numeric vector of length 1") else{
      if (!length(cex) == 1) stop("'picsize' must be a numeric vector of length 1")}}  
  
  #if it argument is not "jpeg" or "tiff" 
  if (!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  ## calculate song level parameters
  if (!is.null(X) & !is.null(song))
  {
    if (!any(names(X) == song)) stop(paste(song, "column not found"))
    
    Xsong <- song_param(X, song_colm = song, pb = FALSE)
  }
  
  # overwrite
  if (!overwrite) 
    files <- files[!gsub(".wav$","", list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE),ignore.case = TRUE) %in% 
      unlist(sapply(strsplit(as.character(list.files(path = path, pattern = paste(it, "$", 
                                                                     sep = ""), ignore.case = TRUE)), "-p",fixed = TRUE), "[",1))]
  
  files <- files[!is.na(files)]
  
  #stop if all files have been analyzed 
  if (length(files) == 0) stop("all .wav files have been processed")
  
  #create function for making spectrograms
  # z = sound files, fl = flim, sl = sxrow, li = rows, li = duplicated rows if Y provided, X = selection table, W = contours, autod = if Y comes from autodetec

  lspecFUN <-function(z, fl, sl, li, X, W) {
    
    rec <- warbleR::read_wave(X = z, path = path) #read wave file 
    
    f <- rec@samp.rate #set sampling rate
    
    #in case flim is higher than can be due to sampling rate
    frli<- fl 
    if (frli[2] > ceiling(f/2000) - 1) frli[2] <- ceiling(f/2000) - 1 
    
    #set duration    
    dur <- seewave::duration(rec)
    
    #if duration is multiple of sl
    if (!length(grep("[^[:digit:]]", as.character(dur/sl))))
  rec <- seewave::cutw(wave = rec, f = f, from = 0, to = dur-0.001, output = "Wave") #cut a 0.001 s segment of rec     
    
    dur <- seewave::duration(rec) #reset duration    
    
    if (!is.null(X)) Y <- X[X$sound.files == z,] #subset X data
    if (!is.null(X) & !is.null(song)) Ysong <- Xsong[Xsong$sound.files == z, , drop = FALSE]
      
    #loop over pages 
    no.out <- lapply(1 : ceiling(dur / (li * sl)), function(j)  
      {
      img_wrlbr_int(filename = paste0(substring(z, first = 1, last = nchar(z)-4), "-", suffix, "-p", j, ".", it), path = path, units = "in", horizontal = horizontal, ...) 
      
      # set number of rows
      mfrow <- c(li, 1)
      
      # if detections should be printed
      if (!is.null(W))
        if (any(W$sound.files == z))
          mfrow <- c(li * 2,  1)

      par(mfrow = mfrow, cex = 0.6, mar = c(0,  1,  0,  0), oma = c(2, 2, 0.5, 0.5), tcl = -0.25)
      
      #creates spectrogram rows
      x <- 0
      while(x <= li - 1){
        
        x <- x + 1
        
        #for rows with complete spectro
        if (all(((x)*sl+li*(sl)*(j-1))-sl<dur & (x)*sl+li*(sl)*(j-1)<dur)){ 
          spectro_wrblr_int(rec, f = f, wl = wl, flim = frli, tlim = c(((x)*sl+li*(sl)*(j-1))-sl, (x)*sl+li*(sl)*(j-1)), 
                  ovlp = ovlp, collevels = collevels, grid = gr, scale = FALSE, palette = pal, axisX = TRUE, fast.spec = fast.spec)
          if (x == 1) text((sl-0.01*sl) + (li*sl)*(j - 1), frli[2] - (frli[2]-frli[1])/10, paste(substring(z, first = 1, 
                                                                                                          last = nchar(z)-4), "-p", j, sep = ""), pos = 2, font = 2, cex = cex)
          if (!is.null(X))  {
          
            # loop for elements
            if (nrow(Y) > 0)
             for(e in 1:nrow(Y))  
             {
               # if freq columns are not provided   
            ys <- if (is.null(Y$top.freq)) frli[c(1, 2, 2, 1)] else
               c(Y$bottom.freq[e], Y$top.freq[e], Y$top.freq[e], Y$bottom.freq[e])
            
            #plot polygon 
             polygon(x = rep(c(Y$start[e], Y$end[e]), each = 2), y = ys, lty = 2, border = "#07889B", col = adjustcolor("#07889B", alpha.f = 0.12), lwd = 1.2)
            
            if (!is.null(labels)) 
            text(labels = paste(Y[e, labels], collapse = "-"), x = (Y$end[e] + Y$start[e]) / 2, y = if (is.null(Y$top.freq)) frli[2] - 2*((frli[2] - frli[1])/12) else Y$top.freq[e], pos = 3)
             }
            
            # loop for songs  
            if (!is.null(song))
                  for(w in 1:nrow(Ysong))  
              {
                
                lines(y = rep(frli[2] - 0.7 * ((frli[2] - frli[1])/12), 2), x = c(Ysong$start[w], Ysong$end[w]), lwd = 5, col = adjustcolor("#E37222", 0.5), lend = 0)
                if (!is.null(labels))    
                    text(labels = Ysong[w, song], x = (Ysong$end[w] + Ysong$start[w]) / 2, y = frli[2] - 0.7 * ((frli[2] - frli[1])/12), adj = 0, cex = 1.5)      
                      }
            }
          } else { #for rows with incomplete spectro (final row)
      if (all(((x)*sl+li*(sl)*(j-1))-sl < dur & (x)*sl+li*(sl)*(j-1)>dur)){
        spectro_wrblr_int(seewave::pastew(seewave::noisew(f = f,  d = (x)*sl+li*(sl)*(j-1)-dur+1,  type = "unif",   
      listen = FALSE,  output = "Wave"), seewave::cutw(wave = rec, f = f, from = ((x)*sl+li*(sl)*(j-1))-sl,
      to = dur, output = "Wave"), f =f,  output = "Wave"), f = f, wl = wl, flim = frli, 
      tlim = c(0, sl), ovlp = ovlp, collevels = collevels, grid = gr, scale = FALSE, palette = pal, axisX = FALSE, fast.spec = fast.spec)
                                     
        if (x == 1) text((sl-0.01*sl) + (li*sl)*(j - 1), frli[2] - (frli[2]-frli[1])/10, paste(substring(z, first = 1, 
      last = nchar(z)-4), "-p", j, sep = ""), pos = 2, font = 2, cex = cex)                             
                                     
                                     #add white polygon add final row on part without signal
                                     usr<-par("usr")    
                                     polygon(x = rep(c(sl - ((x)*sl+li*(sl)*(j-1)-dur), usr[2]), each = 2), y = c(usr[3], usr[4], usr[4], usr[3]), col = "white")
                                     
                                     #add X lines and labels
                                     
      if (!is.null(X)) {
       adjx <- ((x)*sl+li*(sl)*(j-1))-sl
      
       if (nrow(Y) > 0)
       for(e in 1:nrow(Y))  
      {
        ys <- if (is.null(Y$top.freq)) frli[c(1, 2, 2, 1)] else
          c(Y$bottom.freq[e], Y$top.freq[e], Y$top.freq[e], Y$bottom.freq[e])
        
        polygon(x = rep(c(Y$start[e], Y$end[e]), each = 2) - adjx, y = ys, lty = 2, border = "#07889B", col = adjustcolor("#07889B", alpha.f = 0.12), lwd = 1.2)
        
        if (!is.null(labels)) 
          text(labels = paste(Y[e, labels], collapse = "-"), x = (Y$end[e] + Y$start[e]) / 2 - adjx, y = if (is.null(Y$top.freq)) frli[2] - 2*((frli[2] - frli[1])/12) else Y$top.freq[e], adj = 0, pos = 3)
      }
       
       # loop for songs  
       if (!is.null(song))
         for(w in 1:nrow(Ysong))  
         {
           
           lines(y = rep(frli[2] - 0.7 * ((frli[2] - frli[1])/12), 2), x = c(Ysong$start[w], Ysong$end[w])  - adjx, lwd = 5, col = adjustcolor("#E37222", 0.5), lend = 0)
        if (!is.null(labels)) 
           text(labels = Ysong[w, song], x = ((Ysong$end[w] + Ysong$start[w]) / 2)  - adjx, y = frli[2] - 0.7 * ((frli[2] - frli[1])/12), adj = 0, cex = 1.5)      
         }
       
      }
                                     
              #add axis to last spectro row
              axis(1, at = c(0:sl), labels = c((((x)*sl+li*(sl)*(j-1))-sl):((x)*sl+li*(sl)*(j-1))) , tick = TRUE)
                                     
                #add text indicating end of sound.files
                text(dur-(((x)*sl+li*(sl)*(j-1))-sl), frli[2]-(frli[2]-frli[1])/2, "End of sound file", pos = 4, font = 2, cex = 1.1)
                                     
                #add line indicating end of sound file
                abline(v = dur-(((x)*sl+li*(sl)*(j-1))-sl), lwd = 2.5)} else {plot(1, 1, col = "white", col.axis =  "white", col.lab  =  "white", xaxt = "n", yaxt = "n")
                                     }}
        
        if (!is.null(W))
          if (any(W$sound.files == z)) {
            
            
            # multiply by 100 if from autodetec
            y.fctr <- 1
            if (!is.null(X)) if (X$type[1] == "autodetec") y.fctr <- 100
            
            # set time and scores
            xtime <- W$time[W$sound.files == z]
            yscore <- W$score[W$sound.files == z] * y.fctr
            
            # pad to 0
            xtime <- c(0, xtime, dur)
            yscore <- c(0, yscore, 0)
            
            # plot detection contour
            plot(x = xtime, y = yscore, type = "l", xlim = c(((x)*sl+li*(sl)*(j-1))-sl, (x)*sl+li*(sl)*(j-1)), col = adjustcolor("#E37222", 0.7), ylim = c(0, 1.36) * y.fctr, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
          
            # add for all except after last row
            if (((x)*sl+li*(sl)*(j-1))-sl < dur)
            axis(2, at = seq(0.2, 1, by = 0.2) * y.fctr, labels = seq(0.2, 1, by = 0.2) * y.fctr, tick = TRUE)  
            
          # add fill polygon
            polygon(cbind(xtime, yscore), col = adjustcolor("#E37222", 0.3), border = NA)
            
          if (!is.null(X$score))
            points(x = X$time[X$sound.files == z], X$score[X$sound.files == z], pch = 21, col = adjustcolor("#E37222", 0.7), cex = 3.3)
            
          # add cutoff line
            if (!is.null(cutoff))
              lines(
                x = c(0, dur),
                y = if (length(cutoff) == 1)
                  rep(cutoff, 2) else
                  rep(cutoff[names(cutoff) == 
                               paste(X$sound.files[X$sound.files == z][1], X$org.selec[X$sound.files == z][1], sep = "-")][1], 2),
                lty = 2,
                col = adjustcolor("#07889B", 0.7),
                lwd = 1.4
              )
            
          abline(v = dur, lwd = 2.5)
          
          #loop for elements boxes
          Q <- X[X$sound.files == z, ]

          if (!is.null(Q))  # if not from xcorr.output
            if (nrow(Q) > 0) # if some detections were found
            for(e in 1:nrow(Q))     #plot polygon
              polygon(x = rep(c(Q$start[e], Q$end[e]), each = 2), y = c(0, 1.4, 1.4, 0) * y.fctr, lty = 2, border = "#07889B", col = adjustcolor("#07889B", alpha.f = 0.12), lwd = 1.2)
          
          }
      }
      dev.off() #reset graphic device
    }
    )
    }
    
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  sp <- pbapply::pblapply(X = files, cl = cl, FUN = function(i) 
  { 
    lspecFUN(z = i, fl = flim, sl = sxrow, li = rows, X = X, W = W)
  })  
}



##############################################################################################################
#' alternative name for \code{\link{lspec}}
#'
#' @keywords internal
#' @details see \code{\link{lspec}} for documentation. \code{\link{lspec}} will be deprecated in future versions.
#' @export

full_spec <- lspec
