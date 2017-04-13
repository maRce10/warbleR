#' Interactive view of spectrograms to tailor start and end of selections 
#' 
#' \code{seltailor} produces an interactive spectrographic view (similar to \code{\link{manualoc}}) in 
#' which the start and end times of acoustic signals listed in a data frame can be adjusted.
#' @usage seltailor(X = NULL, wl = 512, flim = c(0,22), wn = "hanning", mar = 0.5,
#'  osci = TRUE, pal = reverse.gray.colors.2, ovlp = 70, auto.next = FALSE, pause = 1,
#'   comments = TRUE, path = NULL, frange = FALSE, fast.spec = FALSE, ext.window = TRUE,
#'   width = 15, height = 5, ...)
#' @param X data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
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
#'   with higher resolution (see seltime). Default is \code{TRUE}.
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
#' @param path Character string containing the directory path where the sound files are located.
#' @param frange Logical argument specifying whether limits on frequency range should be
#'  recorded. 
#' If \code{NULL} (default) then only the time limits are recorded.
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast' spectograms. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}} offer 
#' decreasing darkness levels. THIS IS STILL BEING TESTED.
#' @param ext.window Logical. If \code{TRUE} then and external graphic window is used. Default 
#' dimensions can be set using the 'width' and 'height' arguments. Default is \code{TRUE}.
#' @param width Numeric of length 1 controling the width of the external graphic window. Ignored
#' if \code{ext.window = FALSE}. Default is 15.
#' @param height Numeric of length 1 controling the height of the external graphic window.
#' Ignored if \code{ext.window = FALSE}. Default is 5.
#' @param ... Additional arguments to be passed to the internal spectrogram creating function for customizing graphical output. The function is a modified version of \code{\link[seewave]{spectro}}, so it takes the same arguments. 
#' @return .csv file saved in the working directory with start and end time of 
#'   selections.
#' @export
#' @name seltailor
#' @examples
#' \dontrun{
#' #Set temporary working directory
#' setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' seltailor(X =  selec.table, flim = c(1,12), wl = 300, auto.next = TRUE)
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
#'  by clicking at the end and at the start of the signal (in any order). In addition, 2
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
#' @seealso  \code{\link{manualoc}}
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-5-2016 (MAS)

seltailor <- function(X = NULL, wl = 512, flim = c(0,22), wn = "hanning", mar = 0.5,
            osci = TRUE, pal = reverse.gray.colors.2, ovlp = 70, auto.next = FALSE,
            pause = 1, comments = TRUE, path = NULL, frange = FALSE, fast.spec = FALSE,
            ext.window = TRUE, width = 15, height = 5, ...)
{
 
  #X must be provided
  if(is.null(X)) stop("'X' must be provided (a data frame)")
  
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
  if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
    setwd(path)} #set working directory
  
  #if there are NAs in start or end stop
  if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if(all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'end' and 'selec' must be numeric")
  
  #if any start higher than end stop
  if(any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))
  
  
  # stop if not all sound files were found
  fs <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
    stop(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                  ".wav file(s) not found"))

    if(frange & !all(any(names(X) == "low.f"), any(names(X) == "high.f")))
      X$high.f <- X$low.f <- NA
        
  if(!file.exists(file.path(getwd(), "seltailor_output.csv")))
  {X$tailored <- ""
  X$tailored <- as.character(X$tailored)
    write.csv(X, "seltailor_output.csv", row.names =  FALSE)  
    } else {X <- read.csv("seltailor_output.csv", stringsAsFactors = FALSE)  
  if(any(is.na(X$tailored))) X$tailored[is.na(X$tailored)] <-""
  if(all(any(!is.na(X$tailored)),X$tailored[nrow(X)] == "y")) stop("all selections have been analyzed") 
    }
  
  dn <- 0
  if(any(!is.na(X$tailored))) if(length(which(X$tailored == "y")) > 0) dn = max(which(X$tailored == "y"))
   
  #set external window function
  platform <- sessionInfo()$platform 
  
  if(grepl("linux|pc",platform)) extwin <- grDevices::X11
  if(grepl("apple",platform)) extwin <- grDevices::quartz
  
  #start external graphic device
  if(ext.window)  extwin(width = width, height = height)
  
  #this first loop runs over selections
  for(j in (dn + 1):nrow(X))
    {
    rec <- tuneR::readWave(as.character(X$sound.files[j]), header = TRUE)
    main <- paste(X$sound.files[j], X$selec[j], sep = "-") 
    
    if(all(comments, !is.null(X$sel.comment))) {if(!is.na(X$sel.comment[j])) main <- paste(X$sound.files[j],"-", X$selec[j],"   ",
                                                         "(",X$sel.comment[j], ")", sep = "")}
    f <- rec$sample.rate #for spectro display
    fl<- flim #in case flim its higher than can be due to sampling rate
    if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
    len <- rec$samples/f  #for spectro display 
    start <- numeric() #save results
    end <- numeric() #save results
    selcount <- 0
    tlim <- c(X$start[j] - mar, X$end[j] + mar)
    if(tlim[1]<0) tlim[1]<-0
    if(tlim[2]>rec$samples/f) tlim[2]<-rec$samples/f
    org.start <- X$start[j] #original start value
      
      #set an undivided window
      par(mfrow = c(1,1), mar = c(3, 3, 1.8, 0.1))
      
      #create spectrogram
      spectroW(tuneR::readWave(as.character(X$sound.files[j]),from =  tlim[1], to = tlim[2], units = "seconds"), 
                       f = f, wl = wl, ovlp = ovlp, wn = wn, collevels = seq(-40, 0, 0.5), heights = c(3, 2), 
                       osc = osci, palette =  pal, main = main, axisX= TRUE, grid = FALSE, collab = "black", alab = "", fftw= TRUE, colwave = "blue4",
              flim = fl, scale = FALSE, axisY= TRUE, fast.spec = fast.spec, ...)
     if(!osci)
      {
       mtext("Time (s)", side=1, line= 1.8)
      mtext("Frequency (kHz)", side = 2, line = 1.7,  xpd = NA, las = 0)
      }
      
      #add lines of selections on spectrogram
      abline(v = c(X$start[j], X$end[j]) - tlim[1], lty = 3, col = "blue", lwd = 1.2)
      
      #add buttons
      xs <- grconvertX(x = c(0.92, 0.92, 0.99, 0.99), from = "npc", to = "user")
      #mid position of buttoms in c(0, 1) range
      cpy <- c(0.72, 0.87)
      mrg <- 0.05
      ys <- c(-mrg, mrg, mrg, -mrg)
      labels <- c("stop", "next sel")
      
      ys <- lapply(1:length(cpy), function(x) 
        {
        grY <- grconvertY(y = cpy[x]- ys, from = "npc", to = "user")
        polygon(x = xs, y = grY, border = "cyan3", col = adjustcolor("white", alpha.f = 0.7), lwd = 2)
   text(x = mean(xs), y = mean(grY), labels = labels[x], cex = 1, font = 2)
   return(grY)   
   })
      
      #ask users to select what to do next (1 click)
      xy2 <- xy <- locator(n = 1, type = "n")
    
      #if selected is lower than 0 make it 
      xy$x[xy$x < 0] <- 0  
      xy$y[xy$y < 0] <- 0 
      xy2$x[xy2$x < 0] <- 0  
      xy2$y[xy2$y < 0] <- 0 
     
      
      #if next sel
      if(xy$x > min(xs) & xy$x < max(xs) & xy$y > min(ys[[2]]) & xy$y < max(ys[[2]]))
      {    
        X$tailored[j] <- "y"
        write.csv(X, "seltailor_output.csv", row.names =  FALSE)  
        
        if(X$tailored[nrow(X)] == "y") {
          dev.off()
          stop("all selections have been analyzed")
          } 
      }
      
      # stop
      if(xy$x > min(xs) & xy$x < max(xs) & xy$y > min(ys[[1]]) & xy$y < max(ys[[1]]))
      {dev.off()
        if(selcount > 0) X$tailored[j] <- "y"
        write.csv(X, "seltailor_output.csv", row.names =  FALSE)
        stop("Stopped by user")} 
          
      #while not inside buttons
      while(!all(xy$x > min(xs) & xy$x < max(xs) & xy$y > min(ys[[2]]) & xy$y < max(ys[[2]])) & !all(xy$x > min(xs) & xy$x < max(xs) & xy$y > min(ys[[1]]) & xy$y < max(ys[[1]])))
      {
  
      #select second point
      xy <- locator(n = 1, type = "n")
     
       #if selected is lower than 0 make it 
      xy$x[xy$x < 0] <- 0  
      xy$y[xy$y < 0] <- 0 
      
      #if next sel
      if(xy$x > min(xs) & xy$x < max(xs) & xy$y > min(ys[[2]]) & xy$y < max(ys[[2]]))
      {    
        X$tailored[j] <- "y"
        write.csv(X, "seltailor_output.csv", row.names =  FALSE)  
        
        if(X$tailored[nrow(X)] == "y") {
          dev.off()
          stop("all selections have been analyzed")
          } else  break
      }
      
      # stop
      if(xy$x > min(xs) & xy$x < max(xs) & xy$y > min(ys[[1]]) & xy$y < max(ys[[1]]))
      {dev.off()
        if(selcount > 0) X$tailored[j] <- "y"
        write.csv(X, "seltailor_output.csv", row.names =  FALSE)
        stop("Stopped by user")}
      
      xy3 <- list()
      xy3$x <- c(xy$x, xy2$x)
      xy3$y <- c(xy$y, xy2$y)
      
      
      if(frange) polygon(x = rep(sort(xy3$x), each = 2), y = c(sort(xy3$y),sort(xy3$y, decreasing = TRUE)), lty = 3, border = "red", lwd = 1.2, col = adjustcolor("blue", alpha.f = 0.15)) else
        abline(v = xy3$x, lty = 3, col = "red", lwd = 1.2)
      
      # if(selcount > 0) abline(v = c(X$start[j], X$end[j]), lty = 1, col = "white", lwd = 2.3)
      X$start[j] <-  tlim[1] + min(xy3$x) 
      X$end[j] <-  tlim[1] + max(xy3$x)
      X$tailored[j] <- "y"
      if(frange) {
        X$low.f[j] <- min(xy3$y)  
        if(min(xy3$y) < 0) X$low.f[j] <- 0  
        X$high.f[j] <- max(xy3$y)  
      }
      selcount <- selcount + 1
      
    #auto next
      if(auto.next){
        X$start[j] <-  tlim[1] + min(xy3$x) 
        X$end[j] <-  tlim[1] + max(xy3$x)
        if(frange) {
          X$low.f[j] <- min(xy3$y)  
          if(min(xy3$y) < 0) X$low.f[j] <- 0  
          X$high.f[j] <- max(xy3$y)
        }
     if(X$tailored[nrow(X)] == "y") { dev.off()
       stop("all selections have been analyzed") 
       } else
      {Sys.sleep(pause) 
        break}
        }
      } 
           }
  if(!is.null(path)) setwd(wd)
  }
