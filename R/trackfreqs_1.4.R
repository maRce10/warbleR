#' Spectrograms with frequency measurements
#' 
#' \code{trackfreqs} creates spectrograms to visualize dominant and fundametal frequency measurements.
#' @usage trackfreqs(X, wl = 512, flim = c(0, 22), wn = "hanning", pal =
#'   reverse.gray.colors.2, ovlp = 70, inner.mar = c(5, 4, 4, 2) + 0.1, outer.mar = 
#'   c(0, 0, 0, 0), picsize = 1, res = 100, cexlab = 1, title = TRUE, trel =
#'   FALSE, propwidth = FALSE, xl = 1, osci = FALSE, gr = FALSE, sc = FALSE,
#'   fmax = 12, bp = c(0, 22), cex = c(0.8, 1), threshold = 10, col =
#'   c("dodgerblue", "chartreuse3"), pch = c(16, 17),  mar = 0.05, lpos =
#'   "topright", it = "jpeg")
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
#' @param fmax Numeric vector of length one, specifying the maximum for 
#'   frequency measurements. Default is 12 Hz.
#' @param bp A numeric vector of length two for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param cex Numeric vector of length one, specifies relative size of points 
#'   plotted for frequency measurements and legend font/points, respectively. 
#'   See \code{\link[seewave]{spectro}}.
#' @param threshold \% amplitude threshold for fundamental frequency and 
#'   dominant frequency detection
#' @param col Vector of length two specifying colors of points plotted to mark 
#'   fundamental and dominant frequency measurements. Default is c("dodgerblue",
#'   "chartreuse3").
#' @param pch Numeric vector of length two specifying plotting characters for 
#'   the frequency measurements. Default is c(16, 17).
#' @param mar Numeric vector of length one. Specifies the margins to subtract 
#'   from/add to start and end points of manualoc() selection, respectively, 
#'   dealineating spectrogram limits. Default is 0.05.
#' @param lpos Character vector of length one or numeric vector of length two, 
#'   specifiying position of legend. If the former, any keyword accepted by 
#'   xy.coords can be used. If the latter, the first value will be the x 
#'   coordinate and the second value the y coordinate for the legend's position.
#'   Default is "topright".
#' @param it A character vector of length one giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @return Spectrograms per individual call marked with dominant and fundamental
#'   frequencies.
#' @family spectrogram creators
#' @seealso \code{\link{specreator}} for creating spectrograms after 
#'   \code{manualoc}, \code{\link{snrspecs}} for creating spectrograms to 
#'   optimize noise margins used in \code{sig2noise}
#' @export
#' @name trackfreqs
#' @details This function provides visualization of two frequency measurements 
#'   made by \code{specan}. Arguments that are accepted by xy.coords and can be 
#'   used for lpos are: "bottomright", "bottom", "bottomleft", "left", 
#'   "topleft", "top", "topright", "right" and "center". Setting inner.mar to 
#'   c(4,4.5,2,1) and outer.mar to c(4,2,2,1) works well when picsize = 2 or 3. 
#'   Title font size, inner.mar and outer.mar (from mar and oma) don't work well
#'   when osci or sc = TRUE, this may take some optimization by the user.
#' @examples
#' \dontrun{
#' data(list = c("Arre.aura", "Phae.cuvi"))
#' data(manualoc.df)
#' writeWave(Arre.aura, "Arre.aura.wav") #save sound files 
#' writeWave(Phae.cuvi, "Phae.cuvi.wav")
#' 
#' # make Arre.aura and Phae.cuvi spectrograms  
#' 
#' trackfreqs(manualoc.df, flim = c(0, 14), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), 
#' picsize = 2, res = 300, cexlab = 2, fmax = 14, bp = c(0, 14), cex = c(1.5, 2), 
#' col = c("blue", "red"),  mar = 0.09, lpos = "bottomright", it = "jpeg")
#'                  
#' # make only Arre.aura spectrograms
#' 
#' trackfreqs(manualoc.df[grepl(c("Arre"), manualoc.df$sound.files), ], flim = c(3, 14),
#' inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), picsize = 2, res = 300, cexlab = 2, 
#' fmax = 14, bp = c(3, 14), cex = c(1.5, 2), col = c("blue", "red"),  mar = 0.09, 
#' lpos = "bottomright", it = "tiff")
#' }
#' @author Grace Smith Vidaurre and Marcelo Araya-Salas (http://marceloarayasalas.weebly.com)

trackfreqs <- function(X, wl = 512, flim = c(0, 22), wn = "hanning", pal = reverse.gray.colors.2, ovlp = 70, 
                       inner.mar = c(5,4,4,2)+0.1, outer.mar = c(0,0,0,0), picsize = 1, res = 100, cexlab = 1,
                       title = TRUE, trel = FALSE, propwidth = FALSE, xl = 1, osci = FALSE, gr = FALSE, sc = FALSE, 
                       fmax = 12, bp = c(0, 22), cex = c(0.8, 1), threshold = 10, col = c("dodgerblue", "chartreuse3"),
                       pch = c(16, 17), mar = 0.05, lpos = "topright", it = "jpeg"){     

  if(class(X) == "data.frame") {if(all(c("sound.files", "selec", 
                                         "start", "end") %in% colnames(X))) 
  {
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
  if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}
 
  #if it argument is not "jpeg" or "tiff" 
  if(!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
  #return warning if not all sound files were found
  recs.wd <- list.files(path = getwd(), pattern = ".wav$", ignore.case = T)
  if(length(unique(sound.files[(sound.files %in% recs.wd)])) != length(unique(sound.files))) 
    message(paste(length(unique(sound.files))-length(unique(sound.files[(sound.files %in% recs.wd)])), 
                  ".wav file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(sound.files %in% recs.wd) 
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
    start <- start[d]
    end <- end[d]
    selec <- selec[d]
    sound.files <- sound.files[d]
  }
  
  message("Creating spectrograms overlaid with acoustic measurements:")
  invisible(pbapply::pbapply(matrix(c(1:length(sound.files)), ncol=1), 1, function(i){
    
    r <- tuneR::readWave(file.path(getwd(), sound.files[i]))
    
    #in case bp its higher than can be due to sampling rate
    b<- bp 
    if(b[2] > ceiling(r@samp.rate/2000) - 1) b[2] <- ceiling(r@samp.rate/2000) - 1 
    
    f <- r@samp.rate
    t <- c(start[i] - mar, end[i] + mar)
    cex <- cex
    
    # Spectrogram width can be proportional to signal duration
    if(propwidth == TRUE){
      if(it == "tiff") tiff(filename = paste(sound.files[i],"-", selec[i], "-", "trackfreqs", ".tiff", sep = ""), 
           width = (10.16) * ((t[2]-t[1])/0.27) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res) else
             jpeg(filename = paste(sound.files[i],"-", selec[i], "-", "trackfreqs", ".jpeg", sep = ""), 
                  width = (10.16) * ((t[2]-t[1])/0.27) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res)
      
    } else {
      if(it == "tiff") tiff(filename = paste(sound.files[i],"-", selec[i], "-", "trackfreqs", ".tiff", sep = ""), 
           width = (10.16) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res) else
             jpeg(filename = paste(sound.files[i],"-", selec[i], "-", "trackfreqs", ".jpeg", sep = ""), 
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
    seewave::spectro(r, f = f, wl = wl, ovlp = 70, collevels = seq(-40, 0, 0.5), heights = hts,
            wn = "hanning", widths = wts, palette = pal, osc = osci, grid = gr, scale = sc, collab = "black", 
            cexlab = cexlab, cex.axis = 0.5*picsize, tlim = t, flim = flim, tlab = "Time (s)", 
            flab = "Frequency (kHz)", alab = "", trel = trel)
    
    if(title){
      
      title(paste(sound.files[i], "-", selec[i], "-", "trackfreqs", sep = ""), cex.main = cexlab)
      
    }
    
    # Plot fundamental frequencies at each time point
    ffreq <- seewave::fund(r, f = f, ovlp = 70, threshold = threshold,, fmax = fmax * 1000, from=start[i],
                  to = end[i], plot = FALSE) 
    if(trel)
    points(c(ffreq[,1])+start[i], c(ffreq[,2]), col = col[1], cex = cex[1], pch = pch[1]) else 
        points(c(ffreq[,1])+mar, c(ffreq[,2]), col = col[1], cex = cex[1], pch = pch[1])  
    
    # Plot dominant frequency at each time point     
    dfreq <- seewave::dfreq(r, f = f, wl = wl, ovlp = 70, plot = FALSE, bandpass = b * 1000, fftw = TRUE, 
                   threshold = threshold, tlim = c(start[i], end[i]))
    if(trel)
      points(c(dfreq[,1])+start[i], c(dfreq[,2]), col = col[2], cex = cex[1], pch = pch[1]) else
        points(c(dfreq[,1])+mar, c(dfreq[,2]), col = col[2], cex = cex[1], pch = pch[1])
    
    abline(v = c(mar, end[i] - start[i] + mar), col= "red", lty = "dashed")
    
    # Legend coordinates can be uniquely adjusted 
    legend(lpos, legend = c("Ffreq", "Dfreq"),
           pch = pch, col = col[1:2], bty = "o", cex = cex[2])
    
    invisible() # execute par(old.par)  
    dev.off()
    return(NULL)
    
  }))
message("all done!")
}