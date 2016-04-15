#' Interactive view of spectrograms to tailor start and end of selections 
#' 
#' \code{seltailor} produces an interactive spectrographic view (similar to \code{\link{manualoc}}) i
#' n which the start and end times of acoustic signals listed in a data frame can be adjusted.
#' @usage seltailor(X = NULL, wl = 512, flim = c(0,22), wn = "hanning", mar = 0.5,
#'  osci = FALSE, pal = reverse.gray.colors.2, ovlp = 70, auto.next = FALSE, pause = 1,
#'   comments = TRUE)
#' @param X data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{seltailor}} or \code{\link{autodetec}} can 
#' be used as the input data frame. Other data frames can be used as input, but must have at least the 4 columns mentioned above. Required. Notice that, if an output file ("seltailor_output.csv") is found in the working directory it will be given priority over an input data frame.
#' @param wl A numeric vector of length 1 specifying the spectrogram window length. Default is 512.
#' @param flim A numeric vector of length 2 specifying the frequency limit (in kHz) of 
#'   the spectrogram, as in the function \code{\link[seewave]{spectro}}. 
#'   Default is c(0,22).
#' @param wn A character vector of length 1 specifying the window function (by default "hanning"). 
#' See function \code{\link[seewave]{ftwindow}} for more options.
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the 
#' start and end points of the selections to define spectrogram limits. Default is 0.5.
#' @param osci Logical argument. If \code{TRUE} adds a oscillogram whenever the spectrograms are produced 
#'   with higher resolution (see seltime). Default is \code{FALSE}.
#'   The external program must be closed before resuming analysis. Default is \code{NULL}.
#' @param pal A color palette function to be used to assign colors in the 
#'   plot, as in \code{\link[seewave]{spectro}}. Default is reverse.gray.colors.2. See Details.
#' @param ovlp Numeric vector of length 1 specifying the percent overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70.
#' @param auto.next Logical argument to control whether the functions moves automatically to the 
#' next selection. The time interval before moving to the next selection is controled by the 'pause' argument.
#' @param pause Numeric vector of length 1. Controls the duration of the waiting period before 
#' moving to the next selection (in seconds). Default is 1. 
#' @param comments Logical argument specifying if 'sel.comment' (when in data frame) should be included 
#' in the title of the spectrograms. Default is \code{TRUE}.
#' @return .csv file saved in the working directory with start and end time of 
#'   selections.
#' @export
#' @name seltailor
#' @examples
#' \dontrun{
#' #First create empty folder
#' setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "manualoc.df"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' seltailor(X =  manualoc.df, flim = c(1,12), wl = 300, auto.next = FALSE)
#' 
#' # need to use the buttoms to manipulate function
#' 
#' # Read output .csv file
#' seltailor.df <- read.csv("seltailor_output.csv")
#' seltailor.df
#' 
#' # check this directory for .csv file after stopping function
#' getwd()
#' }
#' @details This function produces an interactive spectrographic view (similar to \code{\link{manualoc}}) 
#' in which users can select a new start and end of a vocalization unit (e.g. elements)
#'  by clicking at the end and then at the start of the signal (in any order). In addition, 2
#'   "buttons" are provided at the upper right side of the spectrogram that
#'   allow to stop the analysis ("Stop") or go to the next sound file ("next sel"). When a unit 
#'   has been selected, the function plots red dotted lines in the start and end of the 
#'   selection in the spectrogram. The  lines "disappear" when a new selections is made.
#'   Only the last selection is kept for each selection that is adjusted.
#'   The function produces a .csv file (seltailor_output.csv) with the same information than the input 
#'   data frame, except for the new time coordinates, plus a new column (X$tailored) indicating if the selection 
#'   has been tailored. The file is saved in the working directory  and is updated every time the user
#'    moves into the next sound file (next sel "button") or stop the process 
#'  (Stop "button").  If no selection (by clicking on the 'next' buttom) the 
#'  original time coordinates are kept. When resuming the process (after "stop" and re-running 
#'  the function in the same working directory), the function will continue working on the
#'  selections that have not been analyzed.
#'   
#'   Windows length (wl) controls the temporal and frequency precision of the spectrogram. 
#'   A high "wl" value increases the frequency resolution but reduces the temporal resolution, and vice versa. Any
#'   color palette that comes with the seewave package can be used: temp.colors,
#'   reverse.gray.colors.1, reverse.gray.colors.2, reverse.heat.colors, reverse.terrain.colors,
#'   reverse.topo.colors, reverse.cm.colors, heat.colors, terrain.colors, topo.colors,
#'   cm.colors. Note that, unlike \code{\link{manualoc}}, you cannot zoom in the spectrogram \code{\link{seltailor}}. 
#'   The zoom can be adjusted by setting the \code{mar} argument.
#'  @seealso  \code{\link{manualoc}}
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})

seltailor <- function(X = NULL, wl = 512, flim = c(0,22), wn = "hanning", mar = 0.5,
            osci = FALSE, pal = reverse.gray.colors.2, ovlp = 70, auto.next = FALSE,
            pause = 1, comments = TRUE)
{
  
  # stop if not all sound files were found
  fs <- list.files(path = getwd(), pattern = ".wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
    stop(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                  ".wav file(s) not found"))

  wavs = 0
  
  if(!file.exists(file.path(getwd(), "seltailor_output.csv")))
  {X$tailored <- ""
  X$tailored <- as.character(X$tailored)
    write.csv(X, "seltailor_output.csv", row.names = F)  
    } else {X <- read.csv("seltailor_output.csv", stringsAsFactors = FALSE)  
  if(any(is.na(X$tailored))) X$tailored[is.na(X$tailored)] <-""
  if(all(any(!is.na(X$tailored)),X$tailored[nrow(X)] == "y")) { stop("all selections have been analyzed")}}
  
  if(any(!is.na(X$tailored))) if(length(which(X$tailored == "y"))>0) wavs = max(which(X$tailored == "y"))
    
  
  
  #this first loop runs over files
  repeat{
    wavs = wavs + 1 # for selecting .wav files
    try(dev.off(), silent = T)
    recs <- vector() #store results
    rec <- tuneR::readWave(as.character(X$sound.files[wavs]), header = TRUE)
    main <- paste(X$sound.files[wavs], X$selec[wavs], sep = "-") 
    
    if(all(comments, !is.null(X$sel.comment))) {if(!is.na(X$sel.comment[wavs])) main <- paste(X$sound.files[wavs],"-", X$selec[wavs],"   ",
                                                         "(",X$sel.comment[wavs], ")", sep = "")}
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
    selcount <- 0
    tlim <- c(X$start[wavs] - mar, X$end[wavs] + mar)
    if(tlim[1]<0) tlim[1]<-0
    if(tlim[2]>rec$samples/f) tlim[2]<-rec$samples/f
    org.start <- X$start[wavs] #original start value

    #this second run on a single file and breaks when clicking on stop or next
    repeat{
      
      #set an undivided window
      if(mean(par("mfrow")) != 1) par(mfrow = c(1, 1))
      
      #create spectrogram
      seewave::spectro(tuneR::readWave(as.character(X$sound.files[wavs]),from =  tlim[1], to = tlim[2], units = "seconds"), 
                       f = f, wl = wl, ovlp = ovlp, wn = wn, collevels = seq(-40, 0, 0.5), heights = c(3, 2), 
                       osc = osci, palette =  pal, main = main, axisX = T, grid = F, collab = "black", alab = "", fftw = T, 
              flim = fl, scale = FALSE, axisY = T, cexlab = 1, flab = "Frequency (kHz)", tlab = "Time (s)")
      
      #add lines of selections on spectrogram
      abline(v = c(X$start[wavs], X$end[wavs]) - tlim[1], lty = 3, col = "blue", lwd = 1.2)
      
      #Stop button
      polygon(c((tlim[2] - tlim[1])/marg1, (tlim[2] - tlim[1])/marg1, (tlim[2] - tlim[1])/marg2, 
                (tlim[2] - tlim[1])/marg2), c((fl[2] - fl[1])/marg1, (fl[2] - fl[1])/marg2, (fl[2] - fl[1])/marg2, 
                                              (fl[2] - fl[1])/marg1) - (2*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1], 
              col=topo.colors(6, alpha = 0.55)[3], border = topo.colors(6, alpha = 1)[3])      
      
      text(((((tlim[2] - tlim[1])/marg1) + ((tlim[2] - tlim[1])/marg2))/2), 
           ((((fl[2] - fl[1])/marg1) + ((fl[2] - fl[1])/marg2))/2) + fl[1] - (2*((fl[2] - fl[1])/
                                                                                   marg2 - (fl[2] - fl[1])/marg1)), "Stop", cex = 0.6, font = 2)
      
      #next sel button
      polygon(c((tlim[2] - tlim[1])/marg1, (tlim[2] - tlim[1])/marg1, (tlim[2] - tlim[1])/marg2,
                (tlim[2] - tlim[1])/marg2), c((fl[2] - fl[1])/marg1, (fl[2] - fl[1])/marg2, (fl[2] - fl[1])/marg2, 
                                                        (fl[2] - fl[1])/marg1) - (3*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1], 
              col = topo.colors(6, alpha = 0.55)[4], border = topo.colors(6, alpha = 1)[4])
      
      text(((((tlim[2] - tlim[1])/marg1) + ((tlim[2] - tlim[1])/marg2))/2), 
           ((((fl[2] - fl[1])/marg1) + ((fl[2] - fl[1])/marg2))/2) + fl[1] - (3*((fl[2] - fl[1])/
                                                                                   marg2 - (fl[2] - fl[1])/marg1)), "next sel", cex = 0.6, font = 2)
      
      #ask users to select what to do next (2 clicks)
      xy <- locator(n = 2, type = "n")
      
      #if selected is lower than 0 make it 
      xy$x[xy$x<0] <- 0  
      
      #this keeps runnig as long as next of stop have not been clicked twice
      while(all(!all(all(xy$x > (((tlim[2] - tlim[1])/marg1))), all(xy$x < (((tlim[2] - tlim[1])/marg2))), # not stop
            all(xy$y < (fl[2] - fl[1])/marg2 - (2*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]),
            all(xy$y > (fl[2] - fl[1])/marg1 - (2*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1])),
      !all(all(xy$x > (((tlim[2] - tlim[1])/marg1))), all(xy$x < (((tlim[2] - tlim[1])/marg2))), #not next
           all(xy$y < (fl[2] - fl[1])/marg2 - (3*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]),
           all(xy$y > (fl[2] - fl[1])/marg1 - (3*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]))))
      {
        abline(v = xy$x, lty = 3, col = "red", lwd = 1.2)
        if(selcount > 0) abline(v = c(X$start[wavs], X$end[wavs]), lty = 1, col = "white", lwd = 2.3)
        X$start[wavs] <-  tlim[1] + min(xy$x) 
        X$end[wavs] <-  tlim[1] + max(xy$x)
      selcount <- selcount + 1
      
      #if auto.next was set
      if(auto.next){
        X$tailored[wavs] <- "y"
        write.csv(X, "seltailor_output.csv", row.names = F)  
        if(X$tailored[nrow(X)] == "y") stop("all selections have been analyzed") 
      Sys.sleep(pause) 
        break}   
      
     #ask users to select what to do next (2 clicks)
      xy <- locator(n = 2, type = "n")
      
      #if selected is lower than 0 make it 
      xy$x[xy$x<0] <- 0 
      }

      #stop
      if(all(all(xy$x > (((tlim[2] - tlim[1])/marg1))), all(xy$x < (((tlim[2] - tlim[1])/marg2))),
           all(xy$y < (fl[2] - fl[1])/marg2 - (2*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]),
        all(xy$y > (fl[2] - fl[1])/marg1 - (2*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1])))
      {dev.off()
      if(selcount > 0) X$tailored[wavs] <- "y"
        write.csv(X, "seltailor_output.csv", row.names = F)
       stop("Stopped by user")}
      
      
      #next sel
      if(all(all(xy$x > (((tlim[2] - tlim[1])/marg1))),
           all(xy$x < (((tlim[2] - tlim[1])/marg2))),
           all(xy$y < (fl[2] - fl[1])/marg2 - (3*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1]),
       all(xy$y > (fl[2] - fl[1])/marg1 - (3*((fl[2] - fl[1])/marg2 - (fl[2] - fl[1])/marg1)) + fl[1])))
      {    X$tailored[wavs] <- "y"
       write.csv(X, "seltailor_output.csv", row.names = F)  
       
         if(X$tailored[nrow(X)] == "y") { stop("all selections have been analyzed")}  else break}
    
            if(auto.next){if(X$tailored[nrow(X)] == "y") { stop("all selections have been analyzed")} else 
            {Sys.sleep(pause) 
                break}}   
      }
      }
}
