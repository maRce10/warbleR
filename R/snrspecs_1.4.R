#' Spectrograms with noise margins
#' 
#' \code{snrspecs} create spectrograms to visualize margins over which noise
#' will be measured by sig2noise().
#' @usage snrspecs(X, wl = 512, flim = c(0, 22), wn = "hanning", pal =
#'   reverse.gray.colors.2, ovlp = 70, inner.mar = c(5, 4, 4, 2) + 0.1, outer.mar =
#'   c(0, 0, 0, 0), picsize = 1, res = 100, cexlab = 1, title = TRUE, trel =
#'   FALSE, propwidth = FALSE, xl=1, osci = FALSE, gr = FALSE, sc = FALSE, mar =
#'   0.2, snrmar = 0.1)
#' @param X Data frame output from manualoc().
#' @param wl A number specifying the spectrogram window length, default is 512.
#' @param flim A numeric vector of length two for the frequency limit in kHz of 
#'   the spectrogram, as in \code{\link[seewave]{spectro}}. Default is c(0, 22).
#' @param wn Character vector of length one specifying window name. Default is 
#'   "hanning", as in \code{\link[seewave]{spectro}}.
#' @param pal Color palette function for spectrogram. Default is 
#'   reverse.gray.colors.2.
#' @param ovlp Numeric vector of length one specifying % overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70.
#' @param inner.mar Numeric vector with 4 elements, default is c(5,4,4,2)+0.1. 
#'   Specifies number of lines in inner plot margins where axis labels fall, 
#'   with form c(bottom, left, top, right). See \code{\link[graphics]{par}}.
#' @param outer.mar Numeric vector with 4 elements, default is c(0,0,0,0). 
#'   Specifies number of lines in outer plot margins beyond axis labels, with 
#'   form c(bottom, left, top, right). See \code{\link[graphics]{par}}.
#' @param picsize Numeric argument of length one, controls relative size of 
#'   spectrogram. Default is 1.
#' @param res Numeric argument of length one, controls resolution of tiff image.
#'   Default is 100 (faster) although 300 - 400 is recommended for publication/ 
#'   presentation quality.
#' @param cexlab Numeric vector of length one, specifies relative size of axis 
#'   labels. See \code{\link[seewave]{spectro}}.
#' @param title Logical argument to add a title to individual spectrograms. 
#'   Default is TRUE.
#' @param trel Logical argument to add a time axis scale relative to the wave. 
#'   Default is FALSE.
#' @param propwidth Logical argument to scale the width of spectrogram 
#'   proportionally to duration of the selected call. Default is FALSE.
#' @param xl Numeric vector of length one, a constant by which to scale 
#'   spectrogram width if propwidth = TRUE. Default is 1.
#' @param osci Logical argument to add an oscillogram underneath spectrogram, as
#'   in \code{\link[seewave]{spectro}}. Default is FALSE.
#' @param gr Logical argument to add grid to spectrogram. Default is FALSE.
#' @param sc Logical argument to add amplitude scale to spectrogram, default is 
#'   FALSE.
#' @param mar Numeric vector of length one. Specifies the margins to subtract 
#'   from/add to start and end points of manualoc() selection, respectively, 
#'   delineating spectrogram limits. Default is 0.2.
#' @param snrmar Numeric vector of length one. Specifies the margins to subtract
#'   from/add to start and end points of manualoc() selection, respectively, 
#'   where noise will be measured. Default is 0.1.
#' @return Spectrograms per individual call marked with margins and arrows where
#'   noise will be measured.
#' @family spectrogram creators
#' @seealso \code{\link{trackfreqs}} for creating spectrograms to visualize 
#'   frequency measurements by \code{specan}, \code{\link{specreator}} for 
#'   creating spectrograms after using \code{manualoc}
#' @export
#' @name snrspecs
#' @details This function can be used to test different margins to facilitate 
#'   accurate SNR measurements when using \code{\link{sig2noise}} down the line.
#'   Setting margins for individual calls that have been previously clipped from
#'   larger files may take some optimization, as will margins for calls within a
#'   larger file that are irregularly separated. Setting inner.mar to 
#'   c(4,4.5,2,1) and outer.mar to c(4,2,2,1) works well when picsize = 2 or 3. 
#'   Title font size, inner.mar and outer.mar (from mar and oma) don't work well
#'   when osci or sc = TRUE, this may take some optimization by the user.
#' @examples
#' \dontrun{
#' data(list = c("Arre.aura", "Phae.cuvi"))
#' data(manualoc.df)
#' writeWave(Arre.aura, "Arre.aura.wav") #save sound.files
#' writeWave(Phae.cuvi, "Phae.cuvi.wav") 
#' 
#' # make Arre.aura and Phae.cuvi spectrograms
#' # snrmar needs to be smaller before moving on to sig2noise()
#' 
#' snrspecs(manualoc.df, flim = c(0, 14), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), 
#' picsize = 2, res = 300, cexlab = 2, mar = 0.2, snrmar = 0.1)
#' 
#' # make only Arre.aura spectrograms
#' # snrmar now doesn't overlap neighboring calls
#' 
#' snrspecs(manualoc.df[grepl(c("Arre"), manualoc.df$sound.files), ], flim = c(3, 14), inner.mar = c(4,4.5,2,1), 
#' outer.mar = c(4,2,2,1), picsize = 2, res = 300, cexlab = 2, mar = 0.2, 
#' snrmar = 0.01)
#' }
#' @author Marcelo Araya-Salas http://marceloarayasalas.weebly.com/ and Grace Smith Vidaurre

snrspecs <- function(X, wl = 512, flim = c(0, 22), wn = "hanning", pal = reverse.gray.colors.2, ovlp = 70,
                     inner.mar = c(5,4,4,2)+0.1, outer.mar = c(0,0,0,0), picsize = 1, res = 100,
                     cexlab = 1, title = TRUE, trel = FALSE, propwidth = FALSE, xl = 1, osci = FALSE, 
                     gr = FALSE, sc = FALSE, mar = 0.2, snrmar = 0.1){

  if(class(X) == "data.frame") {if(all(c("sound.files", "selec", 
                                         "start", "end") %in% colnames(X))){
    start <- as.numeric(unlist(X$start))
    end <- as.numeric(unlist(X$end))
    sound.files <- as.character(unlist(X$sound.files))
    selec <- as.character(unlist(X$selec))
    
    } else stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                       "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  } else  stop("X is not a data frame")
  
  #if there are NAs in start or end stop
  if(any(is.na(c(end, start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if(all(class(end) != "numeric" & class(start) != "numeric")) stop("'end' and 'selec' must be numeric")
  
  #if any start higher than end stop
  if(any(end - start<0)) stop(paste("The start is higher than the end in", length(which(end - start<0)), "case(s)"))  
  
  #if any selections longer than 20 secs stop
  if(any(end - start>20)) stop(paste(length(which(end - start>20)), "selection(s) longer than 20 sec"))  
  options( show.error.messages = TRUE)
  
  #if bp is not vector or length!=2 stop
  #if(!is.vector(bp)) stop("'pb' must be a numeric vector of length 2") else{
    #if(!length(bp) == 2) stop("'pb' must be a numeric vector of length 2")}
  
  #return warning if not all sound files were found
  fs <- list.files(path = getwd(), pattern = ".wav$", ignore.case = TRUE)
  if(length(unique(sound.files[(sound.files %in% fs)])) != length(unique(sound.files))) 
    message(paste(length(unique(sound.files))-length(unique(sound.files[(sound.files %in% fs)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(sound.files %in% fs) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
    start <- start[d]
    end <- end[d]
    selec <- selec[d]
    sound.files <- sound.files[d]
  }
  
  message("Creating spectrograms with signal and noise margins to be used in sig2noise():")
  invisible(pbapply::pbapply(matrix(c(1:length(sound.files)), ncol=1), 1, function(i){
    
    r <- tuneR::readWave(file.path(getwd(), sound.files[i])) 
    f <- r@samp.rate
    
    # Correct start and end time if is smaller than 0 or higer than length of rec
    st <- start[i] - mar
    en <- end[i] + mar
    if (st < 0) st <- 0
    if (en > length(r@left)/r@samp.rate) en <- length(r@left)/r@samp.rate
    
    t <- c(st, en)
    
    # Set mar equals to snrmar if is smaller
    if(mar < snrmar) mar <- snrmar

# Spectrogram width can be proportional to signal duration
    if(propwidth == TRUE){
          
      tiff(filename = paste(sound.files[i],"-", selec[i], "-", "snr", ".tiff", sep = ""), 
           width = (10.16) * ((t[2]-t[1])/0.27) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res)
      
    } else {
      
      
      tiff(filename = paste(sound.files[i],"-", selec[i], "-", "snr", ".tiff", sep = ""), 
           width = (10.16) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res)
    }
    
    # Change relative heights of rows for spectrogram when osci = TRUE
    if(osci == TRUE) hts <- c(3, 2) else hts <- NULL
    
    # Change relative widths of columns for spectrogram when sc = TRUE
    if(sc == TRUE) wts <- c(3, 1) else wts <- NULL

    old.par <- par(no.readonly = TRUE) # par settings which could be changed.
    on.exit(par(old.par)) 
    
    # Change inner and outer plot margins
    par(mar = inner.mar)
    par(oma = outer.mar)

    # Generate spectrogram using seewave
    seewave::spectro(r, f = f, wl = wl, ovlp = ovlp, collevels = seq(-40, 0, 0.5), heights = hts,
            wn = "hanning", widths = wts, palette = pal, osc = osci, grid = gr, scale = sc, 
            collab = "black", cexlab = cexlab, cex.axis = 0.5*picsize, tlab = "Time (s)", 
            flab = "Frequency (kHz)", tlim = t, flim = flim, alab = "", trel = trel)
    
    if(title){
      
      title(paste(sound.files[i], "-", selec[i], "-", "snr", sep = ""), cex.main = cexlab)
      
    }
    
    # Add lines to visualize signal
    if(trel)
    abline(v = c(start[i], end[i]), col = "red", lwd = 1.5, lty = "dashed") else {
      abline(v = c(mar, end[i] - start[i] + mar), col = "red", lwd = 1.5, lty = "dashed")
    }
    
    # Add lines to visualize where noise will be measured in sig2noise
    if(trel)
    abline(v = c(start[i] - snrmar, end[i] + snrmar), col = "red", lwd = 1.5, lty = "dashed") else{
    abline(v = c(mar - snrmar, end[i] - start[i] + mar + snrmar), col = "red", lwd = 1.5, lty = "dashed")  
  }
   
    #add arrows/text indicating noise position
    if(trel) 
    {arrows(x0 =start[i] - snrmar, y0 = flim[1]+flim[2]/6, x1 = start[i], y1 = flim[1]+flim[2]/6, code = 3, 
           col = "red", lty = "solid", lwd = 2, angle = 60)        
  
    arrows(x0 =end[i] + snrmar, y0 = flim[1]+flim[2]/6, x1 = end[i], y1 = flim[1]+flim[2]/6, code = 3, 
           col = "red", lty = "solid", lwd = 2, angle = 60)        
    
    text(x = start[i] - (snrmar * 0.5), y = flim[1]+flim[2]/4, labels = "Noise", col = "red",pos = 3)
    
    text(x = end[i] + (snrmar * 0.5), y = flim[1]+flim[2]/4, labels = "Noise", col = "red",pos = 3)} else {
      
    arrows(x0 =mar - snrmar, y0 = flim[1]+flim[2]/6, x1 = mar, y1 = flim[1]+flim[2]/6, code = 3, 
    col = "red", lty = "solid", lwd = 2, angle = 60)        
      
    arrows(x0 =end[i] - start[i] + mar + snrmar, y0 = flim[1]+flim[2]/6, x1 = end[i] - start[i] + mar, y1 = flim[1]+flim[2]/6, code = 3, 
    col = "red", lty = "solid", lwd = 2, angle = 60)        
      
    text(x = mar - (snrmar * 0.5), y = flim[1]+flim[2]/4, labels = "Noise", col = "red",pos = 3)
      
    text(x = end[i] - start[i] + mar + (snrmar * 0.5), y = flim[1]+flim[2]/4, labels = "Noise", col = "red",pos = 3)  
    }

    dev.off()  
    return (NULL)
    
  })) 
message("all done!")  
}

