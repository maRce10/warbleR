#internal warbleR function called by catalog. Type argument similar to par("bty") but added _ for only bottom and - for top
#xys must be a 4 element numeric vector c(x1, x2, y1, y2)

boxw_wrblr_int <- function(xys =NULL, bty = "o", col = "black", lwd = 1, lty = 1){
  
  if (is.null(xys))
  {wh <- par("din")
  xs <- c(0, wh[1])
  ys <- c(0, wh[2])} else {
    xys <- c(xys)
    xs <- xys[1:2]
    ys <- xys[3:4]
  }  
  
  xs <- grconvertX(xs, from = "nic", to = "user")
  
  ys <- grconvertY(ys, from = "nic", to = "user")
  
  cmb <- rbind(combn(rep(xs, 2), 2), combn(rep(ys, 2), 2)[,c(2:6,1)])[,-c(3,6)]
  
  if (bty == "c") cmb <- cmb[, -4]
  if (bty == "l") cmb <- cmb[, -(3:4)]
  if (bty == "7") cmb <- cmb[, 3:4]
  if (bty == "u") cmb <- cmb[, -3]
  if (bty == "^") cmb <- cmb[, -1]
  if (bty == "]") cmb <- cmb[, -2]
  if (bty == "_") cmb <- matrix(cmb[, 1], nrow = 4)
  if (bty == "-") cmb <- matrix(cmb[, 3], nrow = 4)
  
  for(i in 1:ncol(cmb)) 
    lines(x = cmb[1:2,i], y = cmb[3:4,i],  xpd = TRUE, col = col, lwd = lwd, lty = lty)
}

# author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on aug-3-2017 (MAS)


#internal warbleR function, not to be called by users. It is a modified version of seewave::fadew
# fades the start and end of an amplitude envelope
#last modification on feb-23-2018 (MAS)
fade_env_wrblr_int <- function(nvlp, fin = 0.1, fout = 0.2, shape = "linear") 
{
  
  if (fin + fout > 1) stop2("'fin' + 'fout' cannot be higher than 1")
  if (fin < 0 | fout < 0) stop2("'fin' and 'fout' cannot be negative")
  
  n <- length(nvlp)
  IN <- seq(0, 1, length.out = fin * n)
  OUT <- seq(0, 1, length.out = fout * n)
  
  # applied exponential
  if (shape == "exp") {
    IN <- exp(IN)
    IN <- IN - 1
    IN <- IN/max(IN)
    OUT <- exp(OUT)
    OUT <- OUT - 1
    OUT <- OUT/max(OUT)
  }
  
  # applied cosine 
  if (shape == "cos") {
    if (fin == 0) 
      IN <- integer(0)  else {
        IN <- cos(rev(IN))
        IN <- IN - min(IN)
        IN <- IN/max(IN)
      }
    
    if (fout == 0) 
      OUT <- integer(0)
    else {
      OUT <- cos(rev(OUT))
      OUT <- OUT - min(OUT)
      OUT <- OUT/max(OUT)
    }
  }
  
  
  MID <- rep(1, n - (length(IN) + length(OUT)))
  return(c(IN, MID, rev(OUT)))
}


# internal warbleR function, not to be called by users. It is a modified version of seewave::filled.contour.modif2
# that allows to plot spectrograms on top of each other. 
filled_contour_color_wrblr_int <- function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
                                            z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE), col.lab, colaxis,
                                            zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels), add = FALSE,
                                            nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) - 
                                                                                                           1), plot.title, plot.axes, key.title, asp = NA, xaxs = "i", 
                                            yaxs = "i", las = 1, axisX = TRUE, axisY = TRUE, bg.col = "white") 
{
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      } else {
        z <- x
        x <- seq(0, 1, len = nrow(z))
      }
    } else stop2("no 'z' matrix specified")
  } else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop2("increasing 'x' and 'y' values expected")
  if (!add) {plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp, bg = bg.col)
    
    
    usr <- par("usr")
    rect(xleft = usr[1], xright = usr[2], ybottom = usr[3], usr[4], col = bg.col, border = bg.col)
    
    
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop2("no proper 'z' matrix specified")}
  if (!is.double(z)) 
    storage.mode(z) <- "double"
  .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                  col = col)
  if (missing(plot.axes)) {
    if (axisX) {
      title(main = "", xlab = "", ylab = "")
      axis(1)
    }
    if (axisY) {
      title(main = "", xlab = "", ylab = "")
      axis(2)
    }
  }
  else plot.axes
  box()
  if (missing(plot.title)) 
    title()
  else plot.title
  invisible()
}


#internal warbleR function, not to be called by users. It is a modified version of seewave::filled.contour.modif2
# that allows to plot spectrograms on top of each other. 
filled_contour_wrblr_int <- function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
                                      z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE), col.lab, colaxis,
                                      zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels), add = FALSE,
                                      nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) - 
                                                                                                     1), plot.title, plot.axes, key.title, asp = NA, xaxs = "i", 
                                      yaxs = "i", las = 1, axisX = TRUE, axisY = TRUE, bg.col = "white") 
{
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      } else {
        z <- x
        x <- seq(0, 1, len = nrow(z))
      }
    } else stop2("no 'z' matrix specified")
  } else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop2("increasing 'x' and 'y' values expected")
  if (!add) plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  usr <- par("usr")
  rect(xleft = usr[1], xright = usr[2], ybottom = usr[3], usr[4], col = bg.col, border = bg.col)
  
  
  if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
    stop2("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"
  .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                  col = col)
  if (missing(plot.axes)) {
    if (axisX) {
      title(main = "", xlab = "", ylab = "")
      axis(1)
    }
    if (axisY) {
      title(main = "", xlab = "", ylab = "")
      axis(2)
    }
  }
  else plot.axes
  box()
  if (missing(plot.title)) 
    title()
  else plot.title
  invisible()
}


#internal warbleR function, not to be called by users. Plots detected frequency range.

frd_plot_wrblr_int <- function(wave, detections, wl = 512, wn = "hanning", flim = NULL, bp = NULL, fast.spec = FALSE, ovlp = 50, pal = reverse.gray.colors.2, widths = c(2, 1), main = NULL, all.detec = F) {
  
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
  {if (flim[2] > floor(f / 2000)) flim[2] <- f / 2000} else
    flim <- c(0, floor(f / 2000))
  
  # set limits for color rectangles down
  if (is.null(bp)) lims <- flim else lims <- bp
  
  # split screen
  m <- rbind(c(0, widths[1]/sum(widths), 0, 0.93), #1
             c(widths[1]/sum(widths), 1, 0 , 0.93),
             c(0, 1,  0.93 , 1)) #3 
  
  try(invisible(close.screen(all.screens = TRUE)), silent = TRUE)  
  split.screen(m)
  screen(1)
  par(mar = c(3.4, 3.4, 0.5, 0))
  
  # create spectro
  spectro_wrblr_int2(wave = wave, f = f, flim = flim, fast.spec = fast.spec, palette = pal, ovlp = ovlp, wl = wl, grid = F, tlab = "", flab = "", wn = wn)
  
  #add gray polygon on detected frequency bands
  out <- lapply(seq_len(length(min.start)), function(e)
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
  
  minz <- min(z)
  maxz <- max(z)
  
  # add axis& labels
  if (detections$type == "percentage")
  {
    axis(1, at = seq(0.2, 1, by = 0.4))
    mtext(side = 1, text = "Amplitude (%)", line = 2.3)
    
  }  else
  {
    axis(1, at = round(seq(0, maxz, by = 10), 0))
    mtext(side = 1, text = "Amplitude (dB)", line = 2.3)
    
  }
  
  # fix amplitude values to close polygon (just for ploting)
  z3 <- c(minz, z, minz)
  
  #addd  extremes to make polygon close fine
  zf3 <- c(lims[1], zf, lims[2])
  
  # plot amplitude values curve
  polygon(cbind(z3, zf3), col= adjustcolor("#E37222", 0.8))
  
  # add border line
  points(z3, zf3, type = "l", col = adjustcolor("gray", 0.3))
  
  # add background color
  rect(xleft = minz, ybottom = flim[1], xright = maxz, ytop = flim[2], col = adjustcolor("#4D69FF", 0.05))
  
  #add gray polygon on detected frequency bands
  lapply(seq_len(length(min.start)), function(e)
  {
    rect(xleft = minz, ybottom = ifelse(is.na(min.start[e]), lims[1], min.start[e]), xright = maxz, ytop = ifelse(is.na(max.end[e]), lims[2], max.end[e]), col = adjustcolor("#07889B", alpha.f = 0.1), border = adjustcolor("gray", 0.2)) 
    
  })
  
  # add gray boxes in filtered out freq bands
  if (!is.null(bp))
  {  rect(xleft = minz, ybottom = bp[2], xright = maxz, ytop = flim[2], col = adjustcolor("gray", 0.5)) 
    rect(xleft = minz, ybottom = flim[1], xright = maxz, ytop = bp[1], col = adjustcolor("gray", 0.5))
  }
  
  # add line to highligh freq range
  abline(v = ifelse(detections$type == "percentage", detections$threshold / 100, detections$threshold), col = adjustcolor("#07889B", 0.7), lty = 3, lwd = 2)
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


# internal warbleR function, not to be called by users. it plots contours, runs locator and return tailored contours 
# @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on mar-12-2016 (MAS)

fix_cntr_wrblr_int <- function(X, j, ending.buttons = 1:4, ncl, tlim, xs, ys, flim, col, alpha, l){
  
  prev.plot <- recordPlot()
  
  if (!l){
    ts.df.t <- seq(X$start[j], X$end[j], length.out = length(ncl)) - tlim[1] 
    ts.df <- X[, ncl]
    ncl2 <- ncl
  } else {
    ts.df.t <- X[j, grep("...TIME", ncl, value = TRUE, fixed = TRUE)] - tlim[1]
    ncl2 <-  grep("...FREQ", ncl, value = TRUE, fixed = TRUE)
    ts.df.f <- X[j, ncl2]
    ts.df <- X[, ncl]
  }
  
  out <- TRUE
  
  x <- 1
  
  while(all(out))
  {
    if (x > 1) replayPlot(prev.plot)
    
    points(x = ts.df.t, 
           y = ts.df[j, ncl2], pch = 20, cex = 1.2, 
           col = adjustcolor(col,  alpha.f = alpha))  
    
    if (any(is.na(ts.df[j, seq_len(which.max(ts.df.t))])))
      points(x = ts.df.t[is.na(ts.df.f[seq_len(which.max(ts.df.t))])], 
             y = ((flim[2] - flim[1]) * 0.02) + flim[1], pch = 20, cex = 1.2,
             col = adjustcolor( "gray",  alpha.f = alpha))  
    
    #select second point
    xy <- locator(n = 1, type = "n")
    
    # if on buttoms
    out <- sapply(ending.buttons, function(w) out  <- !all(xy$x > min(xs) & xy$x < max(xs) & xy$y > min(ys[[w]]) & xy$y < max(ys[[w]])))
    
    if (!all(out)) break  else {
      
      ts.df[j, ncl2[which.min(abs(ts.df.t - xy$x))]] <- xy$y
      
      #if selected is lower than 0 make it 
      xy$x[xy$x < 0] <- 0
      xy$y[xy$y < 0] <- 0 
    }
    
    x <- x + 1
  }
  
  return(list(ts.df = ts.df, xy = xy))  
  
}

#internal warbleR function, not to be called by users. Detects frequency range.

frd_wrblr_int <- function(wave, wl = 512, fsmooth = 0.1, threshold = 10, wn = "hanning", bp = NULL, ovlp = 50, dB.threshold = NULL)
{
  # get sampling rate
  f <-  wave@samp.rate
  
  if(wl >= length(wave@left))  wl <- length(wave@left) - 1 
  if (wl %% 2 != 0) wl <- wl - 1
  
  # mean spectrum
  if (is.null(dB.threshold)){
    
    spc <- meanspec(wave, plot = FALSE, wl = wl, f = f, wn = wn, ovlp = ovlp)
    
    # get frequency windows length for smoothing
    step <- wave@samp.rate/wl/1000
    
    # number of samples
    n <- nrow(spc)
    
    # smoothing parameter
    if (!is.null(fsmooth))
    {
      fsmooth <- fsmooth/step
      
      FWL <- fsmooth - 1
      
      # smooth 
      z <- apply(as.matrix(1:(n - FWL)), 1, function(y) sum(spc[y:(y + FWL), 2]))
      zf <- seq(min(spc[,1]), max(spc[,1]), length.out = length(z))
    } else 
    {
      z <- spc[ , 2]
      zf <- spc[ , 1]
    }
    
    #remove range outside bp
    if (!is.null(bp)) { 
      # if there are complete freq bins within freq range
      if (any(zf > bp[1] & zf < bp[2])) 
        fbins <- which(zf > bp[1] & zf < bp[2]) else # select the one that contains the freq range
          fbins <- which.max(ifelse(zf - bp[1] > 0, NA, zf - bp[1])):which.max(ifelse(zf - bp[2] > 0, NA, zf - bp[1]))
        
        z <- z[fbins]
        zf <- zf[fbins]
    }
    
    # make minimum amplitude 0
    z <- z - min(z)
    z[z < 0] <- 0
    
    # normalize amplitude from 0 to 1
    if(length(z) > 1)
      z <- z/max(z)
    
    #get freqs crossing threshold
    z1 <- rep(0, length(z))
    z1[z > threshold/100] <- 1 
    z2 <- z1[2:length(z1)] - z1[1:(length(z1) - 1)]
    
    # add 0 to get same length than z
    z2 <- c(0, z2)
    
    #determine start and end of amplitude hills  
    strt <- zf[z2 == 1]
    nd <- zf[z2 == -1]
    
    #add NAs when some ends or starts where not found
    if (length(strt) != length(nd))
    {if (z1[1] == 0) nd <- c(nd, NA) else strt <- c(NA, strt)}
    
    if (length(strt) == 1){
      if (z1[1] == 1 & z1[length(z1)] == 1  & strt > nd){    
        strt <- c(NA, strt)
        nd <- c(nd , NA)
      }
    }  
    # substract half a step to calculate mid point between the 2 freq windows in which the theshold has passed
    nd <- nd - (step / 2)
    strt <- strt - (step / 2)
    
    meanpeakf <- zf[which.max(z)] + (step / 2)
  } else {
    
    spc <- meanspec(wave, plot = FALSE, wl = wl, f = f, wn = wn, ovlp = ovlp, dB = "max0", dBref = 2*10e-5)
    
    # get frequency windows length for smoothing
    step <- wave@samp.rate/wl/1000
    
    # number of samples
    n <- nrow(spc)
    
    # smoothing parameter
    if (!is.null(fsmooth))
    {
      fsmooth <- fsmooth/step
      
      FWL <- fsmooth - 1
      
      # smooth 
      z <- apply(as.matrix(1:(n - FWL)), 1, function(y) sum(spc[y:(y + FWL), 2]))
      zf <- seq(min(spc[,1]), max(spc[,1]), length.out = length(z))
      
      z <- (max(spc[ , 2]) - min(spc[ , 2]))/(max(z) - min(z)) * (z - max(z))+max(spc[ , 2])
      
    } else 
    {
      z <- spc[ , 2]
      zf <- spc[ , 1]
    }
    
    if (!is.null(bp)) { 
      #remove range outsde bp
      z <- z[zf > bp[1] & zf < bp[2]]
      zf <- zf[zf > bp[1] & zf < bp[2]]
    }
    
    z1 <- rep(0, length(z))
    z1[z > max(z) - dB.threshold] <- 1 
    z2 <- z1[2:length(z1)] - z1[1:(length(z1) - 1)]
    
    # add 0 to get same length than z
    z2 <- c(0, z2)
    
    #determine start and end of amplitude hills  
    strt <- zf[z2 == 1]
    nd <- zf[z2 == -1]
    
    #add NAs when some ends or starts where not found
    if (length(strt) != length(nd))
    {if (z1[1] == 0) nd <- c(nd, NA) else strt <- c(NA, strt)}
    
    if (length(strt) == 1){
      if (z1[1] == 1 & z1[length(z1)] == 1  & strt > nd){    
        strt <- c(NA, strt)
        nd <- c(nd , NA)
      }
    }  
    
    # step <- mean(zf[-1] - zf[1:(length(zf) - 1)])
    
    # substract half a step to calculate mid point between the 2 freq windows in which the theshold has passed
    nd <- nd - (step / 2)
    strt <- strt - (step / 2)
    meanpeakf <- zf[which.max(z)] + (step / 2)
    
  }
  
  # fix range
  # only start lower than peakf
  strt <- strt[strt <= meanpeakf]
  
  # only ends higher than peakf
  nd <- nd[nd >= meanpeakf]
  
  # get freq range
  min.strt <- ifelse(length(strt) == 1, strt, strt[which.min(meanpeakf - strt)])
  max.nd <- ifelse(length(nd) == 1, nd, nd[which.min(nd - meanpeakf)])
  
  if (!any(is.na(c(min.strt, max.nd)))) {
    if (min.strt > max.nd){
      min.strt <- NA
      max.nd <- NA
    }
  }
  
  # force nd and strt the same length adding NAs
  if(length(nd) > length(strt)) strt <- c(strt, rep(NA, length(nd) - length(strt)))
  if(length(strt) > length(nd)) nd <- c(nd, rep(NA, length(strt) - length(nd)))
  
  # save everything in a list
  rl <- list(frange = data.frame(bottom.freq = min.strt, top.freq = max.nd), af.mat = cbind(z, zf), meanpeakf = meanpeakf, detections = cbind(start.freq = strt, end.freq = nd), threshold = ifelse(is.null(dB.threshold), threshold, max(z) - dB.threshold), type = ifelse(is.null(dB.threshold), "percentage", "dB"))
  
  # return rl list
  return(rl)
}

#internal warbleR function to save image files in jpeg and tiff format, arguments similar to jpeg
# filename must include the image type (jpeg, jpg or tiff)
img_wrlbr_int <- function(filename, path = NULL, res = 160, units = "in", width = 8.5, height = 11, horizontal = FALSE){
  
  if (horizontal & missing(width)) {
    width <- 11
    height <- 8.5
  }
  
  # add path to filename
  flnm <- file.path(path, filename)
  
  # jpeg 
  if (grepl("jpeg$|jpg$", filename)) 
    jpeg(filename = flnm, res = res, units = units, width = width, height = height) else # or tiff
      tiff(filename = flnm, res = res, units = units, width = width, height = height) 
}

# author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on jun-02-2020 (MAS)

# internal warbleR function, not to be called by users. It is a modified version of pbapply::pblapply
# that allows to define internally if progress bar would be used (pbapply::pblapply uses pboptions to do this) 
#last modification on aug-10-2021 (MAS)
#'    
pblapply_wrblr_int <- function(X, FUN, cl = 1, pbar = TRUE, ...) {
  
  # conver parallel 1 to null
  if (!inherits(cl, "cluster"))
    if (cl == 1) cl <- NULL
  
  FUN <- match.fun(FUN)
  if (!is.vector(X) || is.object(X)) 
    X <- as.list(X)
  if (!length(X)) 
    return(lapply(X, FUN, ...))
  if (!is.null(cl)) {
    if (.Platform$OS.type == "windows") {
      if (!inherits(cl, "cluster")) 
        cl <- NULL
    } else {
      if (inherits(cl, "cluster")) {
        if (length(cl) < 2L) 
          cl <- NULL
      } else {
        if (cl < 2) 
          cl <- NULL
      }
    }
  }
  
  if (is.null(cl)) {
    if (!pbar) 
      return(lapply(X, FUN, ...))
    Split <- pbapply::splitpb(length(X), 1L, nout = 100)
    B <- length(Split)
    pb <- pbapply::startpb(0, B)
    on.exit(pbapply::closepb(pb), add = TRUE)
    rval <- vector("list", B)
    for (i in seq_len(B)) {
      rval[i] <- list(lapply(X[Split[[i]]], FUN, ...))
      pbapply::setpb(pb, i)
    }
  } else {
    if (inherits(cl, "cluster")) {
      PAR_FUN <- parallel::parLapply
      if (pbar) 
        return(PAR_FUN(cl, X, FUN, ...))
      Split <- pbapply::splitpb(length(X), length(cl), nout = 100)
      B <- length(Split)
      pb <- pbapply::startpb(0, B)
      on.exit(pbapply::closepb(pb), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(PAR_FUN(cl, X[Split[[i]]], FUN, 
                                ...))
        pbapply::setpb(pb, i)
      }
    } else {
      if (!pbar) 
        return(parallel::mclapply(X, FUN, ..., mc.cores = as.integer(cl)))
      Split <- pbapply::splitpb(length(X), as.integer(cl), nout = 100)
      B <- length(Split)
      pb <- pbapply::startpb(0, B)
      on.exit(pbapply::closepb(pb), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(parallel::mclapply(X[Split[[i]]], 
                                           FUN, ..., mc.cores = as.integer(cl)))
        pbapply::setpb(pb, i)
      }
    }
  }
  rval <- do.call(c, rval, quote = TRUE)
  names(rval) <- names(X)
  rval
}

#internal warbleR function called by catalog

rectw_wrblr_int <- function(xl, yb, xr, yt, bor, cl, ang = NULL, den = NULL, pattern = "no.pattern", lw = 2, lt = 1)
{
  if (pattern == "no.pattern")
    rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt, border = bor, col = cl, angle = ang, density = den, lwd = lw, lty = lt) 
  else {
    
    rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt, border = bor, col = "white", angle = ang, density = den, lwd = lw, lty = lt) 
    
    if (pattern == "diamond")
    {      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor , col = cl, density = den, angle = 45, lwd = lw, lty = lt)
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt ,border = bor , col = cl, density = den, angle = 135, lwd = lw, lty = lt)
    }
    
    if (pattern == "grid")
    {      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 0, lwd = lw, lty = lt)
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 90, lwd = lw, lty = lt)
    }
    
    if (pattern == "forward")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 45, lwd = lw, lty = lt)
    
    if (pattern == "backward")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 315, lwd = lw, lty = lt)
    
    if (pattern == "vertical")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 90, lwd = lw, lty = lt)
    
    if (pattern == "horizontal")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 0, lwd = lw, lty = lt)
  }
  
}
# author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on aug-3-2017 (MAS)


specprop_wrblr_int <- function (spec, f = NULL, flim = NULL, ...) 
{
  fhz <- f
  
  if (is.null(f)) {
    if (is.vector(spec)) 
      stop2("'f' is missing") else if (is.matrix(spec)) 
        f <- spec[nrow(spec), 1] * 2000 * nrow(spec)/(nrow(spec) -                                                       1)
  }
  
  if (is.matrix(spec)) {
    freq <- spec[, 1]
    freq <- freq * 1000
    spec <- spec[, 2]
  }
  L <- length(spec)
  wl <- L * 2
  if (any(spec < 0)) 
    stop2("The frequency spectrum to be analysed should not be in dB")
  if (!is.null(flim)) {
    if (flim[1] < 0 || flim[2] > fhz/2) 
      stop2("'flim' should range between 0 and f/2")
  } else {
    flim <- c(0, (f/2 - f/wl)/1000)
  }
  g <- (1000 * wl/2)/(f/2 - f/wl)
  spec <- spec[(flim[1] * g):(flim[2] * g)]
  spec <- spec[!is.na(spec)]
  L <- length(spec)
  amp <- spec/sum(spec)
  cumamp <- cumsum(amp)
  freq <- seq(from = flim[1] * 1000, to = flim[2] * 1000, 
              length.out = L)
  mean <- sum(amp * freq)
  sd <- sqrt(sum(amp * ((freq - mean)^2)))
  sem <- sd/sqrt(L)
  median <- freq[length(cumamp[cumamp <= 0.5]) + 1]
  mode <- freq[which.max(amp)]
  Q25 <- freq[length(cumamp[cumamp <= 0.25]) + 1]
  Q75 <- freq[length(cumamp[cumamp <= 0.75]) + 1]
  IQR <- Q75 - Q25
  cent <- sum(freq * amp)
  z <- amp - mean(amp)
  w <- sd(amp)
  skew <- (sum(z^3)/(L - 1))/w^3
  kurt <- (sum(z^4)/(L - 1))/w^4
  sfm <- sfm(amp)
  sh <- sh(amp)
  prec <- f/wl
  results <- list(mean = mean, sd = sd, median = median, sem = sem, 
                  mode = mode, Q25 = Q25, Q75 = Q75, IQR = IQR, cent = cent, 
                  skewness = skew, kurtosis = kurt, sfm = sfm, sh = sh, 
                  prec = prec)
  
  return(results)
  
}


#internal warbleR function, not to be called by users. It is a modified version of seewave::spectro 
# that allows to plot spectrograms using image() which substantially increases speed (although makes some options unavailable)
# there is also a soscillo function (soscillo_wrblr_int) down below copied from seewave v2.0.5 (later versions give problems with oscillogram in spectrogram apparently due to fastdisp)
#last modification on feb-09-2018 (MAS)
spectro_wrblr_int <- function(wave, f, wl = 512, wn = "hanning", zp = 0, 
                              ovlp = 0, fast.spec = FALSE,
                              complex = FALSE, norm = TRUE, correction = "none", fftw = FALSE, 
                              dB = "max0", dBref = NULL, plot = TRUE, flog = FALSE, grid = TRUE, 
                              osc = FALSE, scale = TRUE, cont = FALSE, collevels = NULL, 
                              palette = spectro.colors, contlevels = NULL, colcont = "black", 
                              colbg = "white", colgrid = "black", colaxis = "black", collab = "black", 
                              cexlab = 1, cexaxis = 1, tlab = "Time (s)", flab = "Frequency (kHz)", 
                              alab = "Amplitude", scalelab = "Amplitude\n(dB)", main = NULL, 
                              scalefontlab = 1, scalecexlab = 0.75, axisX = TRUE, axisY = TRUE, 
                              tlim = NULL, trel = TRUE, flim = NULL, flimd = NULL, widths = c(6, 1), 
                              heights = c(3, 1), oma = rep(0, 4), rnd = NULL, rm.lwst = FALSE, 
                              colwave =  adjustcolor("#07889B", alpha.f = 0.7), ...) 
{
  
  if(wl >= length(wave@left))  wl <- length(wave@left) - 1 
  if (wl %% 2 != 0) wl <- wl - 1
  
  # remove scale if fast.spec
  if (fast.spec) scale <- FALSE
  
  if (!is.null(dB) && all(dB != c("max0", "A", "B", "C", "D"))) 
    stop2("'dB' has to be one of the following character strings: 'max0', 'A', 'B', 'C' or 'D'")
  
  if (complex) {
    if (plot) {
      plot <- FALSE
      warning("\n'plot' was turned to 'FALSE'")
    }
    if (norm) {
      norm <- FALSE
      warning("\n'norm' was turned to 'FALSE'")
    }
    if (!is.null(dB)) {
      dB <- NULL
      warning("\n'dB' was turned to 'NULL'")
    }
  }
  
  input <- inputw(wave = wave, f = wave@samp.rate)
  
  if (!is.null(tlim) && trel && osc) {
    wave <- wave0 <- input$w
  } else {
    wave <- input$w
  }
  
  f <- input$f
  rm(input)
  
  if (!is.null(tlim)) 
    wave <- cutw(wave, f = f, from = tlim[1], to = tlim[2])
  
  if (!is.null(flimd)) {
    mag <- round((f/2000)/(flimd[2] - flimd[1]))
    wl <- wl * mag
    if (ovlp == 0) 
      ovlp <- 100
    ovlp <- 100 - round(ovlp/mag)
    flim <- flimd
  }
  n <- nrow(wave)
  step <- seq(1, n - wl, wl - (ovlp * wl/100))
  
  # to fix function name change in version 2.0.5
  # if (exists("stdft")) stft <- stdft
  z <- stft_wrblr_int(wave = wave, f = f, wl = wl, zp = zp, step = step, 
                      wn = wn, fftw = fftw, scale = norm, complex = complex, 
                      correction = correction)
  
  if (!is.null(tlim) && trel) {
    X <- seq(tlim[1], tlim[2], length.out = length(step))
  } else {
    X <- seq(0, n/f, length.out = length(step))
  }
  
  pl <- pretty(X)
  if (any(pl < 0)) pl <- pl + abs(min(pl))
  xat <- xlabel <- pl
  
  if (!is.null(rnd)) xlabel <- round(xlabel, rnd)
  
  if (is.null(flim)) {
    Y <- seq(0, (f/2) - (f/(wl + zp)), by = f/(wl + zp))/1000
  } else {
    fl1 <- flim[1] * nrow(z) * 2000/f
    fl2 <- flim[2] * nrow(z) * 2000/f
    z <- z[(fl1:fl2) + 1, , drop = FALSE]
    Y <- seq(flim[1], flim[2], length.out = nrow(z))
  }
  
  yat <- ylabel <- pretty(Y)
  if (rm.lwst) ylabel[1] <- ""
  if (flog) {
    Y <- log(Y + 1)
    yat <- log(yat + 1)
  }
  if (!is.null(dB)) {
    if (is.null(dBref)) {
      z <- 20 * log10(z)
    } else {
      z <- 20 * log10(z/dBref)
    }
    if (dB != "max0") {
      if (dB == "A") 
        z <- dBweight(Y * 1000, dBref = z)$A
      if (dB == "B") 
        z <- dBweight(Y * 1000, dBref = z)$B
      if (dB == "C") 
        z <- dBweight(Y * 1000, dBref = z)$C
      if (dB == "D") 
        z <- dBweight(Y * 1000, dBref = z)$D
    }
  }
  Z <- t(z)
  if (plot) {
    if (!isTRUE(norm) && isTRUE(scale)) 
      stop2("dB colour scale cannot be plot when 'norm' is FALSE")
    maxz <- round(max(z, na.rm = TRUE))
    if (!is.null(dB)) {
      if (is.null(collevels)) 
        collevels <- seq(maxz - 30, maxz, by = 1)
      if (is.null(contlevels)) 
        contlevels <- seq(maxz - 30, maxz, by = 10)
    } else {
      if (is.null(collevels)) 
        collevels <- seq(0, maxz, length = 30)
      if (is.null(contlevels)) 
        contlevels <- seq(0, maxz, length = 3)
    }
    Zlim <- range(Z, finite = TRUE, na.rm = TRUE)
    if (osc & scale) {
      layout(matrix(c(3, 1, 2, 0), ncol = 2, byrow = TRUE), 
             widths = widths, heights = heights)
      par(las = 0, oma = oma, col = "white", col = colaxis, 
          col.lab = collab, cex.lab = cexlab, cex.axis = cexaxis, 
          bg = colbg)
      par(mar = c(0, 1, 4.5, 3))
      seewave::dBscale(collevels = collevels, palette = palette, 
                       fontlab = scalefontlab, cexlab = scalecexlab, 
                       collab = collab, textlab = scalelab, colaxis = colaxis)
      par(mar = c(5, 4.1, 0, 0))
      if (!is.null(tlim) && trel) {
        wave <- wave0
        from <- tlim[1]
        to <- tlim[2]
      } else {
        from <- FALSE
        to <- FALSE
      }
      soscillo_wrblr_int(wave = wave, f = f, bty = "u", from = from, 
                         to = to, collab = collab, colaxis = colaxis, 
                         colline = colaxis, ylim = c(-max(abs(wave)), 
                                                     max(abs(wave))), tickup = max(abs(wave), na.rm = TRUE), 
                         tlab = tlab, alab = alab, cexlab = cexlab, cexaxis = cexaxis, 
                         xaxt = {
                           if (!axisX) {
                             "n"
                           }
                         }, ...)
      par(mar = c(0, 4.1, 1, 0), las = 1, cex.lab = cexlab + 
            0.2)
      
      if (!fast.spec)
        seewave::filled.contour.modif2(x = X, y = Y, z = Z, levels = collevels, 
                                       nlevels = 20, plot.title = title(main = main, 
                                                                        xlab = "", ylab = flab), plot.axes = {
                                                                          if (axisY) {
                                                                            axis(2, at = yat, labels = ylabel)
                                                                          } else {
                                                                            NULL
                                                                          }
                                                                        }, color.palette = palette) else{
                                                                          image(x = X, y = Y, z = Z, col = palette(30), xlab = tlab, ylab = flab)
                                                                          title(main)                                                              }
      
      if (grid) 
        abline(h = yat, col = colgrid, lty = "dotted")
      if (cont) {
        contour(X, Y, Z, add = TRUE, levels = contlevels, 
                nlevels = 5, col = colcont, ...)
      }
      if (colaxis != colgrid) 
        abline(h = 0, col = colaxis)  else abline(h = 0, col = colgrid)
    }
    if (osc == FALSE & scale) {
      layout(matrix(c(2, 1), ncol = 2, byrow = TRUE), 
             widths = widths)
      par(mar = c(5, 1, 4.5, 3), oma = oma, las = 0, bg = colbg)
      seewave::dBscale(collevels = collevels, palette = palette, 
                       fontlab = scalefontlab, cexlab = scalecexlab, 
                       collab = collab, textlab = scalelab, colaxis = colaxis)
      par(mar = c(5, 4.1, 1, 0), las = 1, cex = 1, col = colaxis, 
          col.axis = colaxis, col.lab = collab, bg = colbg, 
          cex.lab = cexlab + 0.2)
      if (!fast.spec)
        seewave::filled.contour.modif2(x = X, y = Y, z = Z, levels = collevels, 
                                       nlevels = 20, plot.title = title(main = main, 
                                                                        xlab = tlab, ylab = flab), plot.axes = {
                                                                          if (axisX) {
                                                                            axis(1, at = xat, labels = xlabel)
                                                                          }
                                                                          if (axisY) {
                                                                            axis(2, at = yat, labels = ylabel)
                                                                          }
                                                                        }, color.palette = palette) 
      else {
        image(x = X, y = Y, z = Z, col = palette(30), xlab = tlab, ylab = flab)
        title(main) 
      }
      if (grid) 
        abline(h = yat, col = colgrid, lty = "dotted")
      if (colaxis != colgrid) 
        abline(h = 0, col = colaxis) else abline(h = 0, col = colgrid)
      if (cont) {
        contour(X, Y, Z, add = TRUE, levels = contlevels, 
                nlevels = 5, col = colcont, ...)
      }
    }
    if (osc & scale == FALSE) {
      layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), 
             heights = heights)
      par(mar = c(5.1, 4.1, 0, 2.1), las = 0, oma = oma, 
          bg = colbg)
      if (!is.null(tlim) && trel) {
        wave <- wave0
        from <- tlim[1]
        to <- tlim[2]
      } else {
        from <- FALSE
        to <- FALSE
      }
      soscillo_wrblr_int(wave = wave, f = f, bty = "u", from = from, 
                         to = to, collab = collab, colaxis = colaxis, 
                         colline = colaxis, tickup = max(abs(wave), na.rm = TRUE), 
                         ylim = c(-max(abs(wave)), max(abs(wave))), tlab = tlab, 
                         alab = alab, cexlab = cexlab, cexaxis = cexaxis, colwave = colwave,
                         xaxt = {
                           if (!axisX) {
                             "n"
                           }
                         }, ...)
      par(mar = c(0, 4.1, 2.1, 2.1), las = 1, cex.lab = cexlab)
      if (!fast.spec)
        filled.contour.modif2(x = X, y = Y, z = Z, levels = collevels,
                              nlevels = 20, plot.title = title(main = main, 
                                                               xlab = "", ylab = flab), color.palette = palette, 
                              plot.axes = {
                                if (axisY) {
                                  axis(2, at = yat, labels = ylabel)
                                } else {
                                  NULL
                                }
                              }, col.lab = collab, colaxis = colaxis, ...) else {
                                image(x = X, y = Y, z = Z, col = palette(30), xlab = tlab, ylab = flab, axes = FALSE)
                                if (axisY) axis(2, at = yat, labels = ylabel)
                                box()
                                if (!is.null(main)) title(main)            
                              }
      if (grid) 
        abline(h = yat, col = colgrid, lty = "dotted")
      if (cont) {
        contour(X, Y, Z, add = TRUE, levels = contlevels, 
                nlevels = 5, col = colcont, ...)
      }
      if (colaxis != colgrid) 
        abline(h = 0, col = colaxis) else abline(h = 0, col = colgrid)
    }
    if (osc == FALSE & scale == FALSE) {
      par(las = 1, col = colaxis, col.axis = colaxis, 
          col.lab = collab, bg = colbg, cex.axis = cexaxis, 
          cex.lab = cexlab, ...)
      
      if (!fast.spec)
        seewave::filled.contour.modif2(x = X, y = Y, z = Z, levels = collevels, 
                                       nlevels = 20, plot.title = title(main = main, 
                                                                        xlab = tlab, ylab = flab), plot.axes = {
                                                                          if (axisX) {
                                                                            axis(1, at = xat, labels = xlabel)
                                                                          }
                                                                          if (axisY) {
                                                                            axis(2, at = yat, labels = ylabel)
                                                                          }
                                                                        }, color.palette = palette, col.lab = collab, 
                                       colaxis = colaxis) else{
                                         image(x = X, y = Y, z = Z, col = palette(30), xlab = tlab, ylab = flab)
                                         title(main) 
                                       }
      if (grid) 
        abline(h = yat, col = colgrid, lty = "dotted")
      if (cont) {
        contour(X, Y, Z, add = TRUE, levels = contlevels, 
                nlevels = 5, col = colcont, ...)
      }
      if (colaxis != colgrid) 
        abline(h = 0, col = colaxis) else abline(h = 0, col = colgrid)
    }
    
    invisible(list(time = X, freq = Y, amp = z))
  } else return(list(time = X, freq = Y, amp = z))
}


soscillo_wrblr_int <- function(
    wave,
    f,
    from = FALSE,
    to =FALSE,
    colwave = "black",
    coltitle = "black",
    collab = "black",
    colline = "black",
    colaxis = "black",
    cexaxis = 1,
    coly0 = "grey47",
    tlab = "Times (s)",
    alab = "Amplitude",
    cexlab = 1,
    fontlab = 1,
    title = FALSE,
    xaxt="s",
    yaxt="n",
    tickup = NULL,
    ... 
)

{
  input<-inputw(wave=wave,f=f) ; wave<-input$w ; f<-input$f ; rm(input)
  
  if(from|to)
  {
    if(from == 0) {a<-1; b<-round(to*f)}
    if(from == FALSE) {a<-1; b<-round(to*f);from<-0}
    if(to == FALSE) {a<-round(from*f); b<-nrow(wave);to<-nrow(wave)/f}
    else {a<-round(from*f); b<-round(to*f)}
    wave<-as.matrix(wave[a:b,])
    n<-nrow(wave)
  }
  else {n<-nrow(wave) ; from<-0 ; to<-n/f}
  
  par(tcl=0.5, col.axis=colaxis, cex.axis = cexaxis, col=colline, col.lab=collab,las=0)
  
  wave<-ts(wave[0:n,], start=from, end=to, frequency = f)
  
  plot(wave,
       col=colwave, type="l",
       xaxs="i", yaxs="i",
       xlab="", ylab="",
       xaxt=xaxt, yaxt=yaxt, bty="l",
       ...)
  axis(side=1, col=colline,labels=FALSE)
  axis(side=2, at=tickup, col=colline,labels=FALSE)
  
  mtext(tlab,col=collab,font=fontlab,cex=cexlab,side=1,line=3)
  mtext(alab,col=collab,font=fontlab,cex=cexlab,side=2,line=3)
  
  abline(h=0,col=coly0,lty=2)
}


#internal warbleR function, not to be called by users. It is a modified version of seewave::spectro 
# that allows to plot spectrograms without resetting the graphic device, which is useful for multipannel figures. It also allow using image() 
# which substantially increases speed (although makes some options unavailable)
#last modification on feb-27-2019 (MAS)
spectro_wrblr_int2 <- function(wave, f, wl = 512, wn = "hanning", zp = 0, ovlp = 0, 
                               complex = FALSE, norm = TRUE, fftw = FALSE, dB = "max0", 
                               dBref = NULL, plot = TRUE, grid = TRUE, 
                               cont = FALSE, collevels = NULL, palette = spectro.colors, 
                               contlevels = NULL, colcont = "black", colbg = "white", colgrid = "gray", 
                               colaxis = "black", collab = "black", cexlab = 1, cexaxis = 1, 
                               tlab = "Time (s)", flab = "Frequency (kHz)", alab = "Amplitude", 
                               scalelab = "Amplitude\n(dB)", main = NULL, scalefontlab = 1, 
                               scalecexlab = 0.75, axisX = TRUE, axisY = TRUE, tlim = NULL, 
                               trel = TRUE, flim = NULL, flimd = NULL, widths = c(6, 1), 
                               heights = c(3, 1), oma = rep(0, 4), listen = FALSE, fast.spec = FALSE, 
                               rm.zero = FALSE, amp.cutoff = NULL, X = NULL, palette.2 = reverse.topo.colors, bx = TRUE, add = FALSE, collev.min = NULL) 
{
  if (!is.null(dB) && all(dB != c("max0", "A", "B", "C", "D"))) 
    stop2("'dB' has to be one of the following character strings: 'max0', 'A', 'B', 'C' or 'D'")
  sel.tab <- X
  
  if (is.list(palette)) palette <- unlist(palette[[1]])
  if (is.null(palette)) palette <- spectro.colors  
  if (!is.function(palette)) palette <- get(palette)
  
  if (is.null(collevels) & !is.null(collev.min))
    collevels <- seq(collev.min, 0, 1)
  
  if (!is.null(sel.tab)) fast.spec <- TRUE 
  
  if (complex & norm) {
    norm <- FALSE
    warning("\n'norm' was turned to 'FALSE'")
  }
  if (complex & !is.null(dB)) {
    dB <- NULL
    warning("\n'dB' was turned to 'NULL'")
  }
  input <- seewave::inputw(wave = wave, f = f)
  
  wave <- input$w
  
  f <- input$f
  rm(input)
  if (!is.null(tlim)) 
    wave <- cutw(wave, f = f, from = tlim[1], to = tlim[2])
  if (!is.null(flimd)) {
    mag <- round((floor(f / 2000))/(flimd[2] - flimd[1]))
    wl <- wl * mag
    if (ovlp == 0) 
      ovlp <- 100
    ovlp <- 100 - round(ovlp/mag)
    flim <- flimd
  }
  n <- nrow(wave)
  step <- seq(1, n - wl, wl - (ovlp * wl/100))
  
  # to fix function name change in after version 2.0.5
  # if (exists("stdft")) stft <- stdft
  z <- stft_wrblr_int(wave = wave, f = f, wl = wl, zp = zp, step = step, 
                      wn = wn, fftw = fftw, scale = norm, complex = complex)
  if (!is.null(tlim) && trel) {
    X <- seq(tlim[1], tlim[2], length.out = length(step))
  }  else {
    X <- seq(0, n/f, length.out = length(step))
  }
  if (is.null(flim)) {
    Y <- seq(0, (f/2) - (f/wl), length.out = nrow(z))/1000
  } else {
    fl1 <- flim[1] * nrow(z) * 2000/f
    fl2 <- flim[2] * nrow(z) * 2000/f
    z <- z[(fl1:fl2) + 1, ]
    Y <- seq(flim[1], flim[2], length.out = nrow(z))
  }
  if (!is.null(dB)) {
    if (is.null(dBref)) {
      z <- 20 * log10(z)
    } else {
      z <- 20 * log10(z/dBref)
    }
    if (dB != "max0") {
      if (dB == "A") 
        z <- dBweight(Y * 1000, dBref = z)$A
      if (dB == "B") 
        z <- dBweight(Y * 1000, dBref = z)$B
      if (dB == "C") 
        z <- dBweight(Y * 1000, dBref = z)$C
      if (dB == "D") 
        z <- dBweight(Y * 1000, dBref = z)$D
    }
  }
  Z <- t(z)
  
  maxz <- round(max(z, na.rm = TRUE))
  if (!is.null(dB)) {
    if (is.null(collevels)) 
      collevels <- seq(maxz - 30, maxz, by = 1)
    if (is.null(contlevels)) 
      contlevels <- seq(maxz - 30, maxz, by = 10)
  } else {
    if (is.null(collevels)) 
      collevels <- seq(0, maxz, length = 30)
    if (is.null(contlevels)) 
      contlevels <- seq(0, maxz, length = 3)
  }
  Zlim <- range(Z, finite = TRUE, na.rm = TRUE)
  
  if (!is.null(amp.cutoff)) Z[Z >= (diff(range(Z)) * amp.cutoff) + min(Z)] <- 0 
  
  if (!fast.spec)
    filled_contour_wrblr_int(x = X, y = Y, z = Z, bg.col = colbg, levels = collevels, 
                             nlevels = 20, plot.title = title(main = main, 
                                                              xlab = tlab, ylab = flab), color.palette = palette, 
                             axisX = FALSE, axisY = axisY, col.lab = collab, 
                             colaxis = colaxis, add = add) else {
                               image(x = X, y = Y, z = Z, col = palette(30), xlab = tlab, ylab = flab, axes = FALSE)
                               if (!is.null(sel.tab))    
                                 out <- lapply(1:nrow(sel.tab), function(i)
                                   image(x = X[X > sel.tab$start[i] & X < sel.tab$end[i]], y = Y[Y > sel.tab$bottom.freq[i] & Y < sel.tab$top.freq[i]], z = Z[X > sel.tab$start[i] & X < sel.tab$end[i], Y > sel.tab$bottom.freq[i] & Y < sel.tab$top.freq[i]], col = palette.2(30), xlab = tlab, ylab = flab, axes = FALSE, xlim = range(X), add = TRUE)      
                                 )
                               
                               
                               if (axisY) axis(2, at = pretty(Y), labels = pretty(Y), cex.axis = cexlab)
                               if (bx)  box()
                               if (!is.null(main)) title(main)       
                             }
  
  if (axisX) {
    if (rm.zero)
      axis(1, at = pretty(X)[-1], labels = pretty(X)[-1], cex.axis = cexaxis)  else
        axis(1, at = pretty(X), labels = pretty(X), cex.axis = cexaxis) 
  }
  
  if (grid) 
    grid(nx = NA, ny = NULL, col = colgrid)
  
}


#internal warbleR function, not to be called by users. It is a modified version of seewave::stft
#last modification on feb-09-2018 (MAS)
stft_wrblr_int <- function (wave, f, wl, zp, step, wn, scale = TRUE, norm = FALSE,                           correction = "none", fftw = FALSE, complex = FALSE) 
{
  if (zp < 0) 
    stop2("zero-padding cannot be negative")
  W <- ftwindow(wl = wl, wn = wn, correction = correction)
  if (fftw) {
    p <- fftw::planFFT(wl + zp)
    z <- apply(as.matrix(step), 1, function(x) fftw::FFT(c(wave[x:(wl + 
                                                                     x - 1), ] * W, rep(0, zp)), plan = p))
  }
  else {
    z <- apply(as.matrix(step), 1, function(x) stats::fft(c(wave[x:(wl + 
                                                                      x - 1), ] * W, rep(0, zp))))
  }
  z <- z[1:((wl + zp)%/%2), , drop = FALSE]
  z <- z/(wl + zp)
  if (complex == FALSE) {
    z <- 2 * Mod(z)
    if (scale) {
      if (norm) {
        z <- z/apply(X = z, MARGIN = 2, FUN = max)
      }
      else {
        z <- z/max(z)
      }
    }
  }
  return(z)
}


# internal warbleR function, not to be called by users. it calculates descriptors of wavelet packet decompositions 
# based on A. Selin, J. Turunen, and J. T. Tanttu, "Wavelets in recognition of bird sounds" EURASIP Journal on Advances in Signal Pro- cessing, vol. 2007, Article ID 51806, 9 pages, 2007.
# @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on oct-7-2019 (MAS)

wpd_feature_wrblr_int <- function(wave, normalize = FALSE, thr1 = 6, thr2 = 0.5){
  
  #force wave object to have a length exp2
  # get target duration
  exp2 <- `^`(2, 1:1000)
  
  # target duration
  trg <- exp2[which.min(!exp2 > length(wave@left))]
  
  # margin to add at both sides
  mar <- rep(0, (trg - length(wave@left)) / 2)
  
  # amplitude vector
  amp.vctr <- c(mar, wave@left, mar)
  
  # fix if still not exp2
  if(length(amp.vctr) < trg)
    amp.vctr <- c(amp.vctr, trg - length(amp.vctr))
  
  # compute wavelet packet decomposition
  out <- wavethresh::wp(amp.vctr, filter.number = 4)
  
  # coefficients
  mat <- out$wp
  bins <- dim(mat)[2]
  num_wpd_coefs <- dim(mat)[1]
  
  # sum of mean of Eb for each bin   
  arr_meanEb <- sapply(1:bins, function(e){
    
    sum(sapply(1:num_wpd_coefs, function(i) mat[i, e]^2)) / num_wpd_coefs
  })
  
  # find max E and position of max E
  mx <- max(arr_meanEb) # max
  ps <- which.max(arr_meanEb) # position
  
  # spread
  # s <- Spread(mat, bins, num_wpd_coefs)
  sprds <- sapply(1:bins, function(w){
    
    # caculate threshold as in equation 6
    umbral <- (sum(sapply(1:num_wpd_coefs, function(i) mat[i, w]^2)) /  num_wpd_coefs) / thr1
    
    value <- mat[, w]^2
    j <- rep(1, length(value))
    
    # make lower values 0
    j[value <= umbral] <- 0
    
    # convert to NA the one lower than value
    value[value <= umbral] <- NA
    
    return(c(sum(value, na.rm = TRUE) / sum(j)))
  })
  
  s <- colSums(t(sprds))
  
  # measure spread
  sprd <- s[1] / s[2]
  
  # add up square coefs by bin
  sum.coefs <- sapply(1:bins, function(e){
    sum(sapply(1:num_wpd_coefs, function(i) mat[i, e]^2))
  }) 
  
  # force to a range 0-1 (proportional to maximum)
  sum.coefs <- sum.coefs / max(sum.coefs)
  
  # get the ones above threshold
  w <- sum(sum.coefs > thr2)
  
  # w <- sum(sapply(1:bins, function(e){
  #   if(sum(sapply(1:num_wpd_coefs, function(i) mat[i, e]^2)) > thr) return(1) else return(0)
  # })) 
  
  result <- c(mx, ps, sprd, w)
  
  if (normalize) {
    
    count <- sum(mat[, result[2]] > (sum(sapply(1:num_wpd_coefs, function(i) mat[i, w]^2)) /  num_wpd_coefs) / 6)
    
    if(count > 0)
      result[1] <- result[1] / count
    
    result <- result * c(1, 1/16, 1/100, 1/20) 
  }
  
  names(result) <- c("max.energy", "position", "spread", "width")
  
  return(result)
}

# stop function that doesn't print call
stop2 <- function (...) 
{
  stop2(..., call. = FALSE)
}
