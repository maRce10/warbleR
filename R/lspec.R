#' Create long spectrograms of whole sound files
#' 
#' \code{lspec} produces image files with spectrograms of whole sound files split into multiple 
#'   rows.
#' @usage lspec(X = NULL, flim = c(0,22), sxrow = 5, rows = 10, collevels = seq(-40, 0, 1), 
#' ovlp = 50, parallel = 1, wl = 512, gr = FALSE, pal = reverse.gray.colors.2, 
#' cex = 1, it = "jpeg", flist = NULL, redo = TRUE, path = NULL, pb = TRUE, 
#' fast.spec = FALSE, labels = "selec") 
#' @param X 'selection_table' object or data frame with results from \code{\link{manualoc}} or any data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). If given, two red dotted lines are plotted at the 
#' start and end of a selection and the selections are labeled with the selection number 
#' (and selection comment, if available). Default is \code{NULL}.
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
#' @param redo Logical argument. If \code{TRUE} all selections will be analyzed again 
#'   when code is rerun. If \code{FALSE} only the selections that do not have a image 
#'   file in the working directory will be analyzed. Default is \code{FALSE}.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast' spectograms. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}} offer 
#' decreasing darkness levels. 
#' @param labels Character string with the name of the column(s) for selection 
#' labeling. Default is 'selec'. Set to \code{NULL} to remove labels.
#' @return image files with spectrograms of whole sound files in the working directory. Multiple pages
#' can be returned, depending on the length of each sound file. 
#' @export
#' @name lspec
#' @details The function creates spectrograms for complete sound files, printing
#'   the name of the sound files and the "page" number (p1-p2...) at the upper 
#'   right corner of the image files. If results from \code{\link{manualoc}} are 
#'   supplied (or an equivalent data frame), the function delimits and labels the selections. 
#'   This function aims to facilitate visual inspection of multiple files as well as visual classification 
#'   of vocalization units and the analysis of animal vocal sequences.
#' @seealso \code{\link{lspec2pdf}}, \code{\link{catalog2pdf}}, 
#' \href{https://marce10.github.io/2017/01/07/Create_pdf_files_with_spectrograms_of_full_recordings.html}{blog post on spectrogram pdfs}
#' @examples
#' \dontrun{
#' # Set temporary working directory
#' # setwd(tempdir())
#' 
#' # save sound file examples
#' data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
#' writeWave(Phae.long1,"Phae.long1.wav") 
#' writeWave(Phae.long2,"Phae.long2.wav")
#' 
#' lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300)
#' 
#' # including selections
#' lspec(sxrow = 2, rows = 8, X = lbh_selec_table, pal = reverse.heat.colors, redo = TRUE, wl = 300)
#' 
#' #check this floder
#' getwd()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on mar-13-2018 (MAS)

lspec <- function(X = NULL, flim = c(0, 22), sxrow = 5, rows = 10, collevels = seq(-40, 0, 1),  ovlp = 50, parallel = 1, 
                  wl = 512, gr = FALSE, pal = reverse.gray.colors.2, cex = 1, it = "jpeg", flist = NULL, redo = TRUE, path = NULL, pb = TRUE, fast.spec = FALSE, labels = "selec") {
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  
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
  if (is.null(path)) path <- getwd() else {if (!dir.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  #if sel.comment column not found create it
  if (is.null(X$sel.comment) & !is.null(X)) X<-data.frame(X,sel.comment="")
  
  #read files
  files <- list.files(pattern = "\\.wav$", ignore.case = TRUE)  
  
  #stop if files are not in working directory
  if (length(files) == 0) stop("no .wav files in working directory")
  
  #subet based on file list provided (flist)
  if (!is.null(flist)) files <- files[files %in% flist]
  if (length(files) == 0)  stop("selected .wav files are not in working directory")
  
  # if X provided  
  if (!is.null(X)) {

    #list only files in X
    files <- files[files %in% X$sound.files]
    
    #if X is not a data frame
    if (!any(is.data.frame(X), is_selection_table(X))) stop("X is not of a class 'data.frame' or 'selection_table'")
    
  #stop if files are not in working directory
  if (length(files) == 0) stop(".wav files in X are not in working directory")
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end columns")  
  
  #check if all columns are found
  if (any(!(c("sound.files", "selec", "start", "end") %in% colnames(X)))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))

  #if end or start are not numeric stop
  if (all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'start' and 'end' must be numeric")
  
  #if any start higher than end stop
  if (any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))  
  }  else manloc <- NULL
 
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
  
  #wrap img creating function
  if (it == "jpeg") imgfun <- jpeg else imgfun <- tiff
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # redo
  if (!redo) 
    files <- files[!gsub(".wav$","", list.files(pattern = "\\.wav$", ignore.case = TRUE),ignore.case = TRUE) %in% 
      unlist(sapply(strsplit(as.character(list.files(pattern = paste(it, "$", 
                                                                     sep = ""), ignore.case = TRUE)), "-p",fixed = TRUE), "[",1))]
  
  files <- files[!is.na(files)]
  
  #stop if files are not in working directory
  if (length(files) == 0) stop("all .wav files have been processed")
  
  #create function for making spectrograms
  lspecFUN <-function(z, fl, sl, li, ml, X) {
    
    rec <- tuneR::readWave(z) #read wave file 
    
    f <- rec@samp.rate #set sampling rate
    
    #in case flim is higher than can be due to sampling rate
    frli<- fl 
    if (frli[2] > ceiling(f/2000) - 1) frli[2] <- ceiling(f/2000) - 1 
    
    #set duration    
    dur <- seewave::duration(rec)
    
    #if duration is multiple of sl
    if (!length(grep("[^[:digit:]]", as.character(dur/sl))))
  rec <- seewave::cutw(wave = rec, f = f, from = 0, to = dur-0.001, output = "Wave") #cut a 0.001 segment of rec     
    
    dur <- seewave::duration(rec) #set duration    
    
    if (!is.null(X)) Y <- X[X$sound.files == z,] #subset X data
    
    #loop over pages 
    no.out <- lapply(1 : ceiling(dur / (li * sl)), function(j)  
      {
       imgfun(filename = paste0(substring(z, first = 1, last = nchar(z)-4), "-p", j, ".", it),  
           res = 160, units = "in", width = 8.5, height = 11) 
      
      par(mfrow = c(li,  1), cex = 0.6, mar = c(0,  0,  0,  0), oma = c(2, 2, 0.5, 0.5), tcl = -0.25)
      
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
           for(e in 1:nrow(Y))  
             {
            ys <- if (is.null(Y$top.freq)) frli[c(1, 2, 2, 1)] else
               c(Y$bottom.freq[e], Y$top.freq[e], Y$top.freq[e], Y$bottom.freq[e])
             
             polygon(x = rep(c(Y$start[e], Y$end[e]), each = 2), y = ys, lty = 2, border = "#07889B", col = adjustcolor("#07889B", alpha.f = 0.12), lwd = 1.2)
            
            if (!is.null(labels)) 
            text(labels = paste(Y[e, labels], collapse = "-"), x = (Y$end[e] + Y$start[e]) / 2, y = if (is.null(Y$top.freq)) frli[2] - 2*((frli[2] - frli[1])/12) else Y$top.freq[e], font = 4, pos = 3)
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
      # l <- Y$selec

      adjx <- ((x)*sl+li*(sl)*(j-1))-sl
      
      for(e in 1:nrow(Y))  
      {
        ys <- if (is.null(Y$top.freq)) frli[c(1, 2, 2, 1)] else
          c(Y$bottom.freq[e], Y$top.freq[e], Y$top.freq[e], Y$bottom.freq[e])
        
        polygon(x = rep(c(Y$start[e], Y$end[e]), each = 2) - adjx, y = ys, lty = 2, border = "#07889B", col = adjustcolor("#07889B", alpha.f = 0.12), lwd = 1.2)
        
        if (!is.null(labels)) 
          text(labels = paste(Y[e, labels], collapse = "-"), x = (Y$end[e] + Y$start[e]) / 2 - adjx, y = if (is.null(Y$top.freq)) frli[2] - 2*((frli[2] - frli[1])/12) else Y$top.freq[e], font = 4, pos = 3)
      }
      }
                                     
                                     #add axis to last spectro row
                                     axis(1, at = c(0:sl), labels = c((((x)*sl+li*(sl)*(j-1))-sl):((x)*sl+li*(sl)*(j-1))) , tick = TRUE)
                                     
                                     #add text indicating end of sound.files
                                     text(dur-(((x)*sl+li*(sl)*(j-1))-sl), frli[2]-(frli[2]-frli[1])/2, "END OF SOUND FILE", pos = 4, font = 2, cex = 1.1)
                                     
                                     #add line indicating end of sound file
                                     abline(v = dur-(((x)*sl+li*(sl)*(j-1))-sl), lwd = 2.5)} else {plot(1, 1, col = "white", col.axis =  "white", col.lab  =  "white", 
                                                                                                        xaxt = "n", yaxt = "n")
                                     }}
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
    lspecFUN(z = i, fl = flim, sl = sxrow, li = rows, X = X)
  })  
}



##############################################################################################################
#' alternative name for \code{\link{lspec}}
#'
#' @keywords internal
#' @details see \code{\link{lspec}} for documentation. \code{\link{lspec}} will be deprecated in future versions.
#' @export

full_spec <- lspec
