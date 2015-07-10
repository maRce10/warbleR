#Modified: Hua Mar 6, 2015.
#           i. Add progress bar in searching process using pblapply
#Modified: Hua, Mar 2, 2015. 
#           i. Put the required packages out of the function, R will automatically warn user to install the package.
#           ii. Replace warning() to massage() to show "All done!"


# Modified by G.S. Vidaurre 3-May-15
#           i. added roxygen comments for documentation and namespace

# Modified by G.S. Vidaurre 3-Jun-15
#           i. changed palette and grid arguments to pal and gr


#' Create long spectrograms of whole sound files
#' 
#' \code{lspec} produces spectrograms of whole sound files split into multiple 
#'   rows.
#' @usage lspec(flim = c(0,22), sxrow = 10, rows = 10, collev = seq(-40, 0, 1), gr = FALSE, 
#'   pal = rev.gray.colors.2, manualoc = NULL, cex = 1)  
#' @param flim A numeric vector of length two indicating the highest and lowest 
#'   frequency limits (kHz) of the spectrogram, as in 
#'   \code{\link[seewave]{spectro}}. Default is c(0,22).
#' @param sxrow A numeric vector of length one. Specifies seconds of spectrogram
#'   per row. Default is 10.
#' @param rows A numeric vector of length one. Specifies number of rows per 
#'   image file. Default is 10.
#' @param collev A numeric vector of three. Specifies levels to partition the 
#'   amplitude range of the spectrogram (in dB). The more levels, the higher the
#'   resolution of the spectrogram. Default is seq(-40, 0, 1).
#' @param gr Logical argument to add grid to spectrogram. Default is FALSE.
#' @param pal Color palette function for spectrogram. Default is 
#'   rev.gray.colors.2.
#' @param manualoc data frame with results from manualoc function. If given, two
#'   red dotted lines are plotted at the start and end of a selection and the 
#'   selections are labeled with the selection number (and selection comment, if
#'   available). Default is NULL.
#' @param cex A numeric vector of length one giving the amount by which text 
#'   (including sound file and page number) should be magnified. Default is 1.
#' @return Spectrograms per individual call marked with dominant and fundamental
#'   frequencies.
#' @export
#' @name lspec
#' @details The function creates spectrograms for complete sound files, printing
#'   the name of the sound files and the "page" number (p1-p2...) at the upper 
#'   right corner of the image files. If results from the manualoc function are 
#'   supplied, the function delimits and labels the selections. This function 
#'   aims to facilitate visual classification of vocalization units and the 
#'   analysis of animal vocal sequences.
#' @examples
#' \dontrun{
#' data(list = c("Arre.aura", "Phae.cuvi"))
#' data(manualoc.df)
#' writeWave(Arre.aura,"Arre.aura.wav") #save sound files
#' writeWave(Phae.cuvi,"Phae.cuvi.wav")
#' lspec(sxrow = 2, rows = 8, pal = rev.heat.colors)
#' lspec(sxrow = 2, rows = 8, manualoc = manualoc.df, pal = rev.heat.colors) #including selections
#' }

lspec <- function(flim = c(0, 22), sxrow = 10, rows = 10, collev = seq(-40, 0, 1),  
                  gr = FALSE, pal = rev.gray.colors.2, manualoc = NULL, cex = 1) {
  
  #read files
  files <- list.files(path = getwd(), pattern = ".wav$", ignore.case = TRUE)  
  
  options(show.error.messages = T)
  
  #stop if seewave and tuneR not installed
  if(length(files) == 0) stop("no .wav files in working directory")
  
  #read manualoc files
  if(!is.null(manualoc)) manloc <- manualoc  else manloc <- NULL
  
  #if there are NAs in start or end stop
  if(!is.null(manualoc))
    if(any(is.na(c(manualoc$end, manualoc$start)))) stop("NAs found in start and/or end columns")  
  
  #apply over each sound file
  #   message("Start the job:")
  pblapply(files, function(z, fl = flim, sl = sxrow, li = rows, ml = manloc, malo = manualoc) {
    
    #loop to print psectros  
    rec <- readWave(z) #read wave file 
    f <- rec@samp.rate #set sampling rate
    frli<- fl #in case flim its higher than can be due to samplin rate
    if(frli[2] > ceiling(f/2000) - 1) frli[2] <- ceiling(f/2000) - 1 
    dur <- length(rec@left)/rec@samp.rate #set duration    
    
    if(!length(grep("[^[:digit:]]", as.character(dur/sl))))  #if duration is multiple of sl
      rec <- cutw(wave = rec, f = f, from = 0, to = dur-0.001, output = "Wave") #cut a 0.001 segment of rec     
    dur <- length(rec@left)/rec@samp.rate #set duration    
    
    if(!is.null(malo)) ml <- ml[ml$sound.files == z,] #subset manualoc data
    #loop over pages 
    for (j in 1:ceiling(dur/(li*sl))){
      tiff(filename = paste(substring(z, first = 1, last = nchar(z)-4), "-p", j, ".tiff", sep = ""),  
           res = 160, units = "in", width = 8.5, height = 11)
      par(mfrow = c(li,  1), cex = 0.6, mar = c(0,  0,  0,  0), oma = c(2, 2, 0.5, 0.5), tcl = -0.25)
      
      #creates spectrogram rows
      x <- 0
      while(x <= li-1){
        x <- x + 1
        if(all(((x)*sl+li*(sl)*(j-1))-sl<dur & (x)*sl+li*(sl)*(j-1)<dur)){  #for rows with complete spectro
          spectro(rec, f = f, wl = 512, flim = frli, tlim = c(((x)*sl+li*(sl)*(j-1))-sl, (x)*sl+li*(sl)*(j-1)), 
                  ovlp = 10, collevels = collev, grid = gr, scale = FALSE, palette = pal, axisX = T)
          if(x == 1) text((sl-0.01*sl) + (li*sl)*(j - 1), frli[2] - (frli[2]-frli[1])/10, paste(substring(z, first = 1, 
                                                                                                          last = nchar(z)-4), "-p", j, sep = ""), pos = 2, font = 2, cex = cex)
          if(!is.null(malo))  {if(any(!is.na(ml$sel.comment))) l <- paste(ml$selec,"-'",ml$sel.comment,
                                                                          "'",sep="") else {l <- ml$selec}
                               mapply(function(se, s, e, sc, labels, fli = frli){
                                 abline(v = c(s, e), col = "red", lty = 2)
                                 text((s + e)/2,  fli[2] - 2*((fli[2] - fli[1])/12), labels = labels , font = 4)},
                                 se = ml$selec, s = ml$start, e = ml$end,sc = ml$sel.comment, 
                                 labels = l)} } else {
                                   if(all(((x)*sl+li*(sl)*(j-1))-sl < dur & (x)*sl+li*(sl)*(j-1)>dur)){ #for rows with incomplete spectro (final row)
                                     spectro(pastew(noisew(f = f,  d = (x)*sl+li*(sl)*(j-1)-dur+1,  type = "unif",   
                                                           listen = FALSE,  output = "Wave"), cutw(wave = rec, f = f, from = ((x)*sl+li*(sl)*(j-1))-sl,
                                                                                                   to = dur, output = "Wave"), f =f,  output = "Wave"), f = f, wl = 512, flim = frli, 
                                             tlim = c(0, sl), ovlp = 10, collevels = collev, grid = gr, scale = FALSE, palette = pal, axisX = F)
                                     
                                     #add manualoc lines and labels
                                     
                                     if(!is.null(malo)) { if(any(!is.na(ml$sel.comment))) l <- paste(ml$selec,"-'",ml$sel.comment,
                                                                                                     "'",sep="") else {l <- ml$selec}
                                                          lise <- ((x)*sl+li*(sl)*(j-1))-sl
                                                          mapply(function(se, s, e, sc, labels, fli = frli, ls = lise){
                                                            abline(v = c(s, e)-ls, col = "red", lty = 2)
                                                            text(((s + e)/2)-ls, fli[2] - 2*((fli[2] - fli[1])/12), 
                                                                 labels = labels, font = 4)}, 
                                                            se = ml$selec, s = ml$start, e = ml$end, sc = ml$sel.comment, labels = l)}
                                     
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
  })
  message("all done!")
}

