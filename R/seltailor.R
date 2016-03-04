#' Interactive view of spectrograms to tailor start and end of selections 
#' 
#' \code{seltailor} produces an interactive spectrographic view in which the start and end times of acoustic signals listed in a data frame can be adjusted.
#' @usage seltailor(X = NULL, wl = 512, flim = c(0,12), wn = "hanning", mar = 0.5,
#'  osci = FALSE, pal = reverse.gray.colors.2, ovlp = 70)
#' @param X data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{seltailor}} or \code{\link{autodetec}} can 
#' be used as the input data frame. Required.
#' @param wl A numeric vector of length 1 specifying the spectrogram window length. Default is 512.
#' @param flim A numeric vector of length 2 specifying the frequency limit (in kHz) of 
#'   the spectrogram, as in the function \code{\link[seewave]{spectro}}. 
#'   Default is c(0,12).
#' @param wn A character vector of length 1 specifying the window function (by default "hanning"). See function 
#' \code{\link[seewave]{ftwindow}} for more options.
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the 
#' start and end points of the selections to define spectrogram limits. Default is 0.5.
#' @param osci Logical argument. If \code{TRUE} adds a oscillogram whenever the spectrograms are produced 
#'   with higher resolution (see seltime). Default is \code{FALSE}.
#'   The external program must be closed before resuming analysis. Default is \code{NULL}.
#' @param pal A color palette function to be used to assign colors in the 
#'   plot, as in \code{\link[seewave]{spectro}}. Default is reverse.gray.colors.2. See Details.
#' @param ovlp Numeric vector of length 1 specifying the percent overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70.
#' @return .csv file saved in the working directory with start and end time of 
#'   selections.
#' @export
#' @name seltailor
#' @examples
#' \dontrun{
#' #First create empty folder
#' dir.create(file.path(getwd(),"temp"))
#' setwd(file.path(getwd(),"temp"))
#' 
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "manualoc.df"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' seltailor(X = manualoc.df)
#' # need to use the buttoms to manipulate function
#' # check working directory for .csv file after stopping function
#' 
#' #remove example directory
#' unlink(getwd(),recursive = TRUE)
#' }
#' @details Users can zoom-in a specific sound file segment by clicking at the 
#'   start and end (left side and right side) of the segment. To select the
#'   start and end of a vocalization unit the users need to click at the end and
#'   then at the start (right side and left side) of the unit. In addition, 3 
#'   "buttons" are provided at the upper right side of the spectrogram that
#'   allow to stop the analysis ("Stop"), go to the next sound file ("Next rec"), play the current view using external software, or delete the last manual selection in the
#'   current sound file ("Del-sel"). When a unit has been selected, the function 
#'   plots red dottedd lines in the start and end of the 
#'   selection in the spectrogram.The  lines "disappear" when the 
#'   selection is deleted ("Del-sel" button). Only the last selection is kept for each selection that is adjusted.
#'   
#'   The function produces a .csv file (seltailor_output.csv) with information about the .wav file name,
#'   selection number, start and end time. The file is saved in the working directory and
#'   is updated every time the user moves into the next sound file (Next rec
#'   "button") or stop the process (Stop "button"). When resuming the process
#'   (after "stop" and re-running the function in the same working directory),
#'   the function will keep the previous selections and will only pick up .wav
#'   files that are not present in the .csv file (not previously analyzed). When users 
#'   go to the next sound file (Next rec "button") without making any
#'   selection the file is still included in the .csv file, with NA's in the
#'   "end", "time" and "selec" field. 
#'   
#'   Windows length (wl) controls the temporal and frequency precision of the spectrogram. 
#'   A high "wl" value increases the frequency resolution but reduces the temporal resolution, and vice versa. Any
#'   color palette that comes with the seewave package can be used: temp.colors,
#'   reverse.gray.colors.1, reverse.gray.colors.2, reverse.heat.colors, reverse.terrain.colors,
#'   reverse.topo.colors, reverse.cm.colors, heat.colors, terrain.colors, topo.colors,
#'   cm.colors.
#'   
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/})

seltailor <- function(X = NULL, wl = 512, flim = c(0,12), wn = "hanning", mar = 0.5, osci = FALSE, pal = reverse.gray.colors.2, ovlp = 70)
{
  
  # return warning if not all sound files were found
  fs <- list.files(path = getwd(), pattern = ".wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
    message(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% fs) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
  X <- X[d, ]
  }
  
  if(!file.exists(file.path(getwd(), "seltailor_output.csv")))
  {results <- data.frame(matrix(nrow = 0, ncol = 4))
   colnames(results) <- c("sound.files", "selec", "start", "end")
   write.csv(results, "seltailor_output.csv", row.names = F)} else
   {if(nrow(read.csv("seltailor_output.csv")) == 0)
   {results <- data.frame(matrix(nrow = 0, ncol = 4))
    colnames(results) <- c("sound.files", "selec", "start", "end")} else
   {results <- read.csv("seltailor_output.csv")  
    X <- X[!paste(X$sound.files, X$selec) %in% paste(results$sound.files, results$selec), ]
    }} 
  
  if(nrow(X) == 0) { stop("all .wav files in working directory have been analyzed")}
  
  wavs = 0
  
  
  #this first loop runs over files
  repeat{
    wavs = wavs + 1 # for selecting .wav files
    try(dev.off(), silent = T)
    recs <- vector() #store results
    rec <- tuneR::readWave(as.character(X$sound.files[wavs]), header = TRUE)
    main <- paste(X$sound.files[wavs], X$selec[wavs], sep = "-")  
    f <- rec$sample.rate #for spectro display
    fl<- flim #in case flim its higher than can be due to sampling rate
    if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
    len <- rec$samples/f  #for spectro display 
    # tlim <- c(0, len) 
    start <- numeric() #save results
    end <- numeric() #save results
    prop <- 14.1 # for box size
    marg1 <- 15/prop # for box size
    marg2 <- marg1*prop/14.9 # for box size
    
    tlim <- c(X$start[wavs] - mar, X$end[wavs] + mar)
    if(tlim[1]<0) tlim[1]<-0
    if(tlim[2]>rec$samples/f) tlim[2]<-rec$samples/f
    
    
    #this second run on a single file and breaks when clicking on stop or next
    repeat{
      
      
      #set an undivided window
      if(mean(par("mfrow")) != 1) par(mfrow = c(1, 1))
      
      #create spectrogram
      seewave::spectro(tuneR::readWave(as.character(X$sound.files[wavs])), f = f, wl = wl, ovlp = ovlp, wn = wn, collevels = seq(-40, 0, 0.5), heights = c(3, 2), osc = osci, palette =  pal, 
              main = main, tlim = tlim, axisX = T, grid = F, collab = "black", alab = "", fftw = T, 
              flim = fl, scale = FALSE, axisY = T, cexlab = 1, flab = "Frequency (kHz)", tlab = "Time (s)")
      
      #add the circle and lines of selections on spectrogram
      abline(v = c(X$start[wavs], X$end[wavs]), lty = 3, col = "blue", lwd = 0.6)
      
      #Stop button
      polygon(c((tlim[2] - tlim[1])/marg1, (tlim[2] - tlim[1])/marg1, (tlim[2] - tlim[1])/marg2, 
                (tlim[2] - tlim[1])/marg2) + tlim[1], c((fl[2] - fl[1])/marg1, (fl[2] - fl[1])/marg2, (fl[2] - fl[1])/marg2, 
                                                        (fl[2] - fl[1])/marg1) - (2*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1], 
              col=topo.colors(6, alpha = 0.55)[3], border = topo.colors(6, alpha = 1)[3])      
      
      text(((((tlim[2] - tlim[1])/marg1) + ((tlim[2] - tlim[1])/marg2))/2) + tlim[1], 
           ((((fl[2] - fl[1])/marg1) + ((fl[2] - fl[1])/marg2))/2) + fl[1] - (2*((fl[2] - fl[1])/
                                                                                   marg2 - (fl[2] - fl[1])/marg1)), "Stop", cex = 0.6, font = 2)
      
      #next rec button
      polygon(c((tlim[2] - tlim[1])/marg1, (tlim[2] - tlim[1])/marg1, (tlim[2] - tlim[1])/marg2,
                (tlim[2] - tlim[1])/marg2) + tlim[1], c((fl[2] - fl[1])/marg1, (fl[2] - fl[1])/marg2, (fl[2] - fl[1])/marg2, 
                                                        (fl[2] - fl[1])/marg1) - (3*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1], 
              col = topo.colors(6, alpha = 0.55)[4], border = topo.colors(6, alpha = 1)[4])
      
      text(((((tlim[2] - tlim[1])/marg1) + ((tlim[2] - tlim[1])/marg2))/2) + tlim[1], 
           ((((fl[2] - fl[1])/marg1) + ((fl[2] - fl[1])/marg2))/2) + fl[1] - (3*((fl[2] - fl[1])/
                                                                                   marg2 - (fl[2] - fl[1])/marg1)), "Next rec", cex = 0.6, font = 2)
      

      #delete selection
      polygon(c((tlim[2] - tlim[1])/marg1, (tlim[2] - tlim[1])/marg1, (tlim[2] - tlim[1])/marg2, 
                (tlim[2] - tlim[1])/marg2) + tlim[1], c((fl[2] - fl[1])/marg1, (fl[2] - fl[1])/marg2, (fl[2] - fl[1])/marg2, 
                                                        (fl[2] - fl[1])/marg1) - (5*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1], 
              col = topo.colors(6, alpha = 0.7)[6], border = topo.colors(6, alpha = 1)[6])
      
      text(((((tlim[2] - tlim[1])/marg1) + ((tlim[2] - tlim[1])/marg2))/2) + tlim[1], 
           ((((fl[2] - fl[1])/marg1) + ((fl[2] - fl[1])/marg2))/2) + fl[1] - (5*((fl[2] - fl[1])/
                                                                                   marg2 - (fl[2] - fl[1])/marg1)), "Del-sel", cex = 0.5, font = 2)
      
      #ask users to select what to do next (2 clicks)
      xy <- locator(n = 2, type = "n")
      
      #if selected is lower than 0 make it 
      xy$x[xy$x<0] <- 0  
      
      # the following code chuncks have the following structure: if click on this box or do this 
      #or if click in this way (left-right or right-left) do this  
      
      #measure/play/delete selection
      #this is the most complicated. It keeps running if the users make a selection (right-left),
      #double click on delete or double click on play
      while(any(xy$x[1] > xy$x[2] & xy$x < (((tlim[2] - tlim[1])/marg1) + tlim[1]),  
                all(xy$x > (((tlim[2] - tlim[1])/marg1) + tlim[1])) & all(xy$x < (((tlim[2] - tlim[1])/marg2) + tlim[1])) & 
                  all(xy$y < (fl[2] - fl[1])/marg2 - (4*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]) & 
                  all(xy$y > (fl[2] - fl[1])/marg1 - (4*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]),
                all(xy$x > (((tlim[2] - tlim[1])/marg1) + tlim[1])) & 
                  all(xy$x < (((tlim[2] - tlim[1])/marg2) + tlim[1])) & 
                  all(xy$y < (fl[2] - fl[1])/marg2 - (5*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]) & 
                  all(xy$y > (fl[2] - fl[1])/marg1 - (5*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1])))
      {if(all(xy$x > (((tlim[2] - tlim[1])/marg1) + tlim[1])) & 
            all(xy$x < (((tlim[2] - tlim[1])/marg2) + tlim[1])) &
            all(xy$y < (fl[2] - fl[1])/marg2 - (4*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]) & 
            all(xy$y > (fl[2] - fl[1])/marg1 - (4*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]) & !is.null(NULL))
       ffffff <- 1 else {if(all(xy$x > (((tlim[2] - tlim[1])/marg1) + tlim[1])) & all(xy$x < (((tlim[2] - tlim[1])/marg2) + 
                                                                                     tlim[1])) & #if click on delete
                   all(xy$y < (fl[2] - fl[1])/marg2 - (5*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]) &
                   all(xy$y > (fl[2] - fl[1])/marg1 - (5*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]) &
                   length(start) > 0) {
                                       abline(v = c(start[length(start)], end[length(end)]), lty = 1, col = "white", lwd = 2.3)
                                       
                                       if(length(start) == 1) start <- numeric() else start <- start[1:(length(start) - 1)]
                                       if(length(end) == 1) end <- numeric() else end <- end[1:(length(end) - 1)]
      } else {start[length(start) + 1] <- xy$x[2]
              end[length(end) + 1] <- xy$x[1]
        
              abline(v = c(start, end), lty = 3, col = "red", lwd = 0.8)
              }}
      xy <- locator(n = 2, type = "n")}
      
      #if selected is lower than 0 make it 
      xy$x[xy$x<0] <- 0  
      
      #stop
      if(all(xy$x > (((tlim[2] - tlim[1])/marg1) + tlim[1])) && 
           all(xy$x < (((tlim[2] - tlim[1])/marg2) + tlim[1])) && 
           all(xy$y < (fl[2] - fl[1])/marg2 - (2*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1])
         && all(xy$y > (fl[2] - fl[1])/marg1 - (2*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]))
      {if(length(start) > 0) {results <- rbind(results, data.frame(sound.files = X$sound.files[wavs], selec = X$selec[wavs], start = start[length(start)], end = end[length(end)]))
                                  results$sound.files <- as.character(results$sound.files)
                                  write.csv(results, "seltailor_output.csv", row.names = F)
                                  dev.off()}
       
       stop("Stopped by user")}
      
      #next rec
      if(all(xy$x > (((tlim[2] - tlim[1])/marg1) + tlim[1])) && 
           all(xy$x < (((tlim[2] - tlim[1])/marg2) + tlim[1])) && 
           all(xy$y < (fl[2] - fl[1])/marg2 - (3*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1])
         && all(xy$y > (fl[2] - fl[1])/marg1 - (3*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]))
      {if(length(setdiff(X$sound.files, unique(results$sound.files))) == 0)
      {try(dev.off(), silent = T)
       message("all selections have been analyzed")
       options( show.error.messages = F)
       stop("")}
      if(length(start) > 0) { results <- rbind(results, data.frame(sound.files = X$sound.files[wavs], selec = X$selec[wavs], start = start[length(start)], end = end[length(end)]))
                              results$sound.files <- as.character(results$sound.files)
                              write.csv(results, "seltailor_output.csv", row.names = F)} else {
                                results <- rbind(results, data.frame(sound.files = X$sound.files[wavs], selec = NA, start = NA,
                                                                     end = NA))
                                results$sound.files <- as.character(results$sound.files)
                                write.csv(results, "seltailor_output.csv", row.names = F)}    
      break}
      
      if(abs(tlim[1] - tlim[2]) < 0.01) {tlim <- c(0.1, len - 0.1)}
      dev.off()}
    
    if(!file.exists(file.path(getwd(), X$sound.files[wavs + 1])))
    {try(dev.off(), silent = T)
     message("This was the last selection")
     break}
  }
}
