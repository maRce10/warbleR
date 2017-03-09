#' Create long spectrograms of whole sound files
#' 
#' \code{lspec} produces image files with spectrograms of whole sound files split into multiple 
#'   rows.
#' @usage lspec(X = NULL, flim = c(0,22), sxrow = 5, rows = 10, collev = seq(-40, 0, 1), 
#' ovlp = 50, parallel = 1, wl = 512, gr = FALSE, pal = reverse.gray.colors.2, 
#' cex = 1, it = "jpeg", flist = NULL, redo = TRUE, path = NULL, pb = TRUE, 
#' fast = FALSE) 
#' @param X Data frame with results from \code{\link{manualoc}} or any data frame with columns
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
#' @param collev A numeric vector of length 3. Specifies levels to partition the 
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
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @param fast Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}} (which are already imported) seem
#' to work better with 'fast' spectograms. Palette colors  \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}} offer 
#' decreasing darkness levels. THIS IS STILL BEING TESTED.
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
#' @examples
#' \dontrun{
#' # Set temporary working directory
#' setwd(tempdir())
#' # save sound file examples
#' data(list = c("Phae.long1", "Phae.long2","selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav") 
#' writeWave(Phae.long2,"Phae.long2.wav")
#' 
#' lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300)
#' 
#' # including selections
#' lspec(sxrow = 2, rows = 8, X = selec.table, pal = reverse.heat.colors, redo = TRUE, wl = 300)
#' 
#' check this floder
#' getwd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on nov-12-2016 (MAS)

lspec <- function(X = NULL, flim = c(0, 22), sxrow = 5, rows = 10, collev = seq(-40, 0, 1),  ovlp = 50, parallel = 1, 
                  wl = 512, gr = FALSE, pal = reverse.gray.colors.2, cex = 1, it = "jpeg", flist = NULL, redo = TRUE, path = NULL, pb = TRUE, fast = FALSE) {
  
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
  if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
    setwd(path)} #set working directory
  
  #if sel.comment column not found create it
  if(is.null(X$sel.comment) & !is.null(X)) X<-data.frame(X,sel.comment="")
  
  #read files
  files <- list.files(pattern = "\\.wav$", ignore.case = TRUE)  
  
  
  #stop if files are not in working directory
  if(length(files) == 0) stop("no .wav files in working directory")
  
  #subet based on file list provided (flist)
  if (!is.null(flist)) files <- files[files %in% flist]
  if (length(files) == 0)  stop("selected .wav files are not in working directory")
  
  #check that all files are in working directory
  if(!is.null(X)) {manloc <- X
  files<-files[files %in% X$sound.files]
  }  else manloc <- NULL
  
  if(!is.null(X)) {
  
  #stop if files are not in working directory
  if(length(files) == 0) stop(".wav files in X are not in working directory")
  
  
  #if there are NAs in start or end stop
  if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end columns")  
  
  #Check class of X
  if(!class(X) == "data.frame" & !is.null(X)) stop("X is not a data frame")
  
  #check if all columns are found
  if(any(!(c("sound.files", "selec", "start", "end") %in% colnames(X)))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))

  #if end or start are not numeric stop
  if(all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'end' and 'selec' must be numeric")
  
  #if any start higher than end stop
  if(any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))  
  }
 
#if flim is not vector or length!=2 stop
  if(is.null(flim)) stop("'flim' must be a numeric vector of length 2") else {
    if(!is.vector(flim)) stop("'flim' must be a numeric vector of length 2") else{
      if(!length(flim) == 2) stop("'flim' must be a numeric vector of length 2")}}   
  
  #if wl is not vector or length!=1 stop
  if(is.null(wl)) stop("'wl' must be a numeric vector of length 1") else {
    if(!is.vector(wl)) stop("'wl' must be a numeric vector of length 1") else{
      if(!length(wl) == 1) stop("'wl' must be a numeric vector of length 1")}}  
  
  #if sxrow is not vector or length!=1 stop
  if(is.null(sxrow)) stop("'sxrow' must be a numeric vector of length 1") else {
    if(!is.vector(sxrow)) stop("'sxrow' must be a numeric vector of length 1") else{
      if(!length(sxrow) == 1) stop("'sxrow' must be a numeric vector of length 1")}}  
  
  #if rows is not vector or length!=1 stop
  if(is.null(rows)) stop("'rows' must be a numeric vector of length 1") else {
    if(!is.vector(rows)) stop("'rows' must be a numeric vector of length 1") else{
      if(!length(rows) == 1) stop("'rows' must be a numeric vector of length 1")}}  
  
  #if picsize is not vector or length!=1 stop
  if(is.null(cex)) stop("'picsize' must be a numeric vector of length 1") else {
    if(!is.vector(cex)) stop("'picsize' must be a numeric vector of length 1") else{
      if(!length(cex) == 1) stop("'picsize' must be a numeric vector of length 1")}}  
  
  #if it argument is not "jpeg" or "tiff" 
  if(!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
  #wrap img creating function
  if(it == "jpeg") imgfun <- jpeg else imgfun <- tiff
  
  #if parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # redo
  if(!redo) 
    files <- files[!gsub(".wav$","", list.files(pattern = "\\.wav$", ignore.case = TRUE),ignore.case = TRUE) %in% 
      unlist(sapply(strsplit(as.character(list.files(pattern = paste(it, "$", 
                                                                     sep = ""), ignore.case = TRUE)), "-p",fixed = TRUE), "[",1))]
  
  
  #stop if files are not in working directory
  if(length(files) == 0) stop("all .wav files have been processed")
  
    #create function for making spectrograms
   lspecFUN <-function(z, fl, sl, li, ml, malo) {
    
          #loop to print spectros  
    rec <- tuneR::readWave(z) #read wave file 
    f <- rec@samp.rate #set sampling rate
    frli<- fl #in case flim is higher than can be due to sampling rate
    if(frli[2] > ceiling(f/2000) - 1) frli[2] <- ceiling(f/2000) - 1 
    dur <- length(rec@left)/rec@samp.rate #set duration    
    
    if(!length(grep("[^[:digit:]]", as.character(dur/sl))))  #if duration is multiple of sl
      rec <- seewave::cutw(wave = rec, f = f, from = 0, to = dur-0.001, output = "Wave") #cut a 0.001 segment of rec     
    dur <- length(rec@left)/rec@samp.rate #set duration    
    
    if(!is.null(malo)) ml <- ml[ml$sound.files == z,] #subset X data
    
    #loop over pages 
    no.out <-lapply(1:ceiling(dur/(li*sl)), function(j)  
      {
       imgfun(filename = paste0(substring(z, first = 1, last = nchar(z)-4), "-p", j, ".", it),  
           res = 160, units = "in", width = 8.5, height = 11) 
      
      par(mfrow = c(li,  1), cex = 0.6, mar = c(0,  0,  0,  0), oma = c(2, 2, 0.5, 0.5), tcl = -0.25)
      
      #creates spectrogram rows
      x <- 0
      while(x <= li-1){
        x <- x + 1
        if(all(((x)*sl+li*(sl)*(j-1))-sl<dur & (x)*sl+li*(sl)*(j-1)<dur)){  #for rows with complete spectro
          spectroW(rec, f = f, wl = wl, flim = frli, tlim = c(((x)*sl+li*(sl)*(j-1))-sl, (x)*sl+li*(sl)*(j-1)), 
                  ovlp = ovlp, collevels = collev, grid = gr, scale = FALSE, palette = pal, axisX = TRUE, fast = fast)
          if(x == 1) text((sl-0.01*sl) + (li*sl)*(j - 1), frli[2] - (frli[2]-frli[1])/10, paste(substring(z, first = 1, 
                                                                                                          last = nchar(z)-4), "-p", j, sep = ""), pos = 2, font = 2, cex = cex)
          if(!is.null(malo))  {if(any(!is.na(ml$sel.comment))) {
            l <- paste(ml$selec, "-'", ml$sel.comment, "'", sep="") 
           l[is.na(ml$sel.comment)] <- ml$selec[is.na(ml$sel.comment)]} else l <- ml$selec
                               mapply(function(s, e, labels, fli = frli){
                                 abline(v = c(s, e), col = "red", lty = 2)
                                 text((s + e)/2,  fli[2] - 2*((fli[2] - fli[1])/12), labels = labels, font = 4)},
                                 s = ml$start, e = ml$end,labels = l)} } else {
                                   if(all(((x)*sl+li*(sl)*(j-1))-sl < dur & (x)*sl+li*(sl)*(j-1)>dur)){ #for rows with incomplete spectro (final row)
                                     spectroW(seewave::pastew(seewave::noisew(f = f,  d = (x)*sl+li*(sl)*(j-1)-dur+1,  type = "unif",   
                                                           listen = FALSE,  output = "Wave"), seewave::cutw(wave = rec, f = f, from = ((x)*sl+li*(sl)*(j-1))-sl,
                                                                                                   to = dur, output = "Wave"), f =f,  output = "Wave"), f = f, wl = wl, flim = frli, 
                                             tlim = c(0, sl), ovlp = ovlp, collevels = collev, grid = gr, scale = FALSE, palette = pal, axisX = FALSE, fast = fast)
                                     
        if(x == 1) text((sl-0.01*sl) + (li*sl)*(j - 1), frli[2] - (frli[2]-frli[1])/10, paste(substring(z, first = 1, 
      last = nchar(z)-4), "-p", j, sep = ""), pos = 2, font = 2, cex = cex)                             
                                     
                                     #add white polygon add final row on part without signal
                                     usr<-par("usr")    
                                     polygon(x = rep(c(sl - ((x)*sl+li*(sl)*(j-1)-dur), usr[2]), each = 2), y = c(usr[3], usr[4], usr[4], usr[3]), col = "white")
                                     
                                     #add X lines and labels
                                     
                                     if(!is.null(malo)) { if(any(!is.na(ml$sel.comment))) {
                                       l <- paste(ml$selec,"-'",ml$sel.comment,"'",sep="")
                                       l[is.na(ml$sel.comment)] <- ml$selec[is.na(ml$sel.comment)]} else l <- ml$selec
                                                          lise <- ((x)*sl+li*(sl)*(j-1))-sl
                                                          mapply(function(s, e, labels, fli = frli, ls = lise){
                                                            abline(v = c(s, e)-ls, col = "red", lty = 2)
                                                            text(((s + e)/2)-ls, fli[2] - 2*((fli[2] - fli[1])/12), 
                                                                 labels = labels, font = 4)}, 
                                                             s = ml$start, e = ml$end, labels = l)}
                                     
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
    
   #Apply over each sound file
   # Run parallel in windows
   if(parallel > 1) {
     if(Sys.info()[1] == "Windows") {
       
       z <- NULL
       
       cl <- parallel::makeCluster(parallel)
       
       doParallel::registerDoParallel(cl)
       
       sp <- foreach::foreach(z = files) %dopar% {
         lspecFUN(z = z, fl = flim, sl = sxrow, li = rows, ml = manloc, malo = X)
       }
       
       parallel::stopCluster(cl)
       
     } 
     if(Sys.info()[1] == "Linux") {    # Run parallel in Linux
       
       sp <- parallel::mclapply(files, function (z) {
         lspecFUN(z = z, fl = flim, sl = sxrow, li = rows, ml = manloc, malo = X)
       })
     }
     if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
     {
       cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))
       
       doParallel::registerDoParallel(cl)
       
       sp <- foreach::foreach(z = files) %dopar% {
         lspecFUN(z = z, fl = flim, sl = sxrow, li = rows, ml = manloc, malo = X)
       }
       
       parallel::stopCluster(cl)
       
     }
   }
   else {
     if(pb)
     sp <- pbapply::pblapply(files, function(z) 
       lspecFUN(z = z, fl = flim, sl = sxrow, li = rows, ml = manloc, malo = X)) else
         sp <- lapply(files, function(z) 
           lspecFUN(z = z, fl = flim, sl = sxrow, li = rows, ml = manloc, malo = X))
   }
   if(!is.null(path)) on.exit(setwd(wd))       
}

