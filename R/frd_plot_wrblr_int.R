#internal warbleR function, not to be called by users. Plots detected frequency range.

frd_plot_wrblr_int <- function(wave, detections, wl = 512, threshold = 10, wn = "hanning", flim = c(0, 22), bp = NULL, fast.spec = FALSE, ovlp = 50, pal = reverse.gray.colors.2, widths = c(2, 1), main = NULL, all.detec = F) {
  
  # attach freq and amplitude values
  z <- detections$af.mat[,1]
  zf <- detections$af.mat[,2]
  
  #sampling rate
  f <-  wave@samp.rate
  dur <- seewave::duration(wave)
  
  # detection limits
  if (!all.detec)
  {
    min.start <- detections$frange$bottom.freq
    max.end <- detections$frange$top.freq
  } else {
    min.start <- detections$detections[,1]
    max.end <- detections$detections[,2]
  }
  
  # fix flim
  if (!is.null(flim))
  {if (flim[2] > ceiling(f/2000) - 1) flim[2] <- ceiling(f/2000) - 1} else
    flim <- c(0, ceiling(f/2000) - 1)
  
  # set limits for color rectangles down
  if (is.null(bp)) lims <- flim else lims <- bp
  
    # split screen
    m <- rbind(c(0, widths[1]/sum(widths), 0, 0.93), #1
               c(widths[1]/sum(widths), 1, 0 , 0.93),
               c(0, 1,  0.93 , 1)) #3 
      
    invisible(close.screen(all.screens = TRUE))  
    split.screen(m)
    screen(1)
    par(mar = c(3.4, 3.4, 0.5, 0))
    
    # create spectro
    spectro_wrblr_int2(wave = wave, f = f, flim = flim, fast.spec = fast.spec, palette = pal, ovlp = ovlp, wl = wl, grid = F, tlab = "", flab = "", wn = wn)
  
    #add gray polygon on detected frequency bands
    lapply(seq_len(length(min.start)), function(e)
    {
      rect(xleft = 0, ybottom = ifelse(is.na(min.start[e]), lims[1], min.start[e]), xright = dur, ytop = ifelse(is.na(max.end[e]), lims[2], max.end[e]), col = adjustcolor("#07889B", alpha.f = 0.1), border = adjustcolor("gray", 0.2)) 
      
    })
      
    #add line highlighting freq range
      abline(h = c(min.start, max.end), col = "#80C3FF", lty = 3, lwd = 2) 
    
    # add axis labels
    mtext(side = 1, text = "Time (s)", line = 2.3)
    mtext(side = 2, text = "Frequency (kHz)", line = 2.3)
    
    #second plot
    screen(2)
    par(mar = c(3.4, 0, 0.5, 0.8))
    
    plot(z, zf, type = "l", ylim = flim, yaxs = "i", xaxs = "i", yaxt = "n", xlab = "", col = "white", xaxt = "n")
    
    # add axis& labels
    axis(1, at = seq(0.2, 1, by = 0.4))
    mtext(side = 1, text = "Amplitude (%)", line = 2.3)
    
    # fix amplitude values to close polygon (just for ploting)
    z3 <- c(0, z, 0)
    
    #addd  extremes to make polygon close fine
    zf3 <- c(lims[1], zf, lims[2])
    
    # plot amplitude values curve
    polygon(cbind(z3, zf3), col= adjustcolor("#E37222", 0.8))
    
    # add border line
    points(z3, zf3, type = "l", col = adjustcolor("gray", 0.3))
    
    # add bacground color
    rect(xleft = 0, ybottom = flim[1], xright = 1, ytop = flim[2], col = adjustcolor("#4D69FF", 0.05))

    #add gray polygon on detected frequency bands
    lapply(seq_len(length(min.start)), function(e)
    {
      rect(xleft = 0, ybottom = ifelse(is.na(min.start[e]), lims[1], min.start[e]), xright = 1, ytop = ifelse(is.na(max.end[e]), lims[2], max.end[e]), col = adjustcolor("#07889B", alpha.f = 0.1), border = adjustcolor("gray", 0.2)) 
      
    })
    
        
    # add gray boxes in filtered out freq bands
    if (!is.null(bp))
    {  rect(xleft = 0, ybottom = bp[2], xright = 1, ytop = flim[2], col = adjustcolor("gray", 0.5)) 
      rect(xleft = 0, ybottom = flim[1], xright = 1, ytop = bp[1], col = adjustcolor("gray", 0.5))
    }
    
    # add line to highligh freq range
    abline(v = threshold/100, col = adjustcolor("#07889B", 0.7), lty = 3, lwd = 2)
    abline(h = c(min.start, max.end), col = "#80C3FF", lty = 3, lwd = 2)
    
    
    if (!is.null(main))
    {
      screen(3)
      par( mar = rep(0, 4))
      plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      
      text(x = 0.5, y = 0.35, labels = main, font = 2)  
    }
    
  
  
  # close screens
  on.exit(invisible(close.screen(all.screens = TRUE)))
  
}


