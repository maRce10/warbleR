# internal warbleR function, not to be called by users. it plots contours, runs locator and return tailored contours 
# @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
# last modification on mar-12-2016 (MAS)

fix_cntr_wrblr_int <- function(X, j, ending.buttons = 1:4, ncl, tlim, xs, ys, flim, col, alpha, l){
  
  prev.plot <- recordPlot()
  
  if(!l){
    ts.df.t <- seq(X$start[j], X$end[j], length.out = length(ncl)) - tlim[1] 
    ts.df <- X[, ncl]
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
    if(x > 1) replayPlot(prev.plot)
    
    points(x = ts.df.t, 
           y = ts.df[j, ncl2], pch = 20, cex = 1.2, 
           col = adjustcolor(col,  alpha.f = alpha))  
    
    if(any(is.na(ts.df[j, seq_len(which.max(ts.df.t))])))
      points(x = ts.df.t[is.na(ts.df.f[seq_len(which.max(ts.df.t))])], 
             y = ((flim[2] - flim[1]) * 0.02) + flim[1], pch = 20, cex = 1.2,
             col = adjustcolor( "gray",  alpha.f = alpha))  
    
    #select second point
    xy <- locator(n = 1, type = "n")
    
    # if(!l)
    # ts.df[j, which.min(abs(ts.df.t - xy$x))] <- xy$y else
        ts.df[j, ncl2[which.min(abs(ts.df.t - xy$x))]] <- xy$y
    
    #if selected is lower than 0 make it 
    xy$x[xy$x < 0] <- 0
    xy$y[xy$y < 0] <- 0 
    
    out <- sapply(ending.buttons, function(w) out  <- !all(xy$x > min(xs) & xy$x < max(xs) & xy$y > min(ys[[w]]) & xy$y < max(ys[[w]])))
    
    if(!all(out)) break
    
    x <- x + 1
  }

return(list(ts.df = ts.df, xy = xy))  
  
}
