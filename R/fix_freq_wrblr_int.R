


fix_freq_wrblr_int <- function(X, j, ending.buttons = 1:4, ncl, tlim, xs, ys){
  
  prev.plot <- recordPlot()
  
  ts.df.t <- seq(X$start[j], X$end[j], length.out = length(ncl)) - tlim[1] 
  ts.df <- X[, ncl]
  out <- TRUE
  
  x <- 1
  
  while(all(out))
  {
    if(x > 1) replayPlot(prev.plot)
    
    points(x = ts.df.t, 
           y = ts.df[j, ], pch = 20, cex = 1.2, 
           col = adjustcolor( "#E37222",  alpha.f = 0.6))  
    
    #select second point
    xy <- locator(n = 1, type = "n")
    
    
    ts.df[j, which.min(abs(ts.df.t - xy$x))] <- xy$y
    
    #if selected is lower than 0 make it 
    # xy$x[xy$x < 0] <- 0  
    xy$y[xy$y < 0] <- 0 
    
    out <- sapply(ending.buttons, function(w) out  <- !all(xy$x > min(xs) & xy$x < max(xs) & xy$y > min(ys[[w]]) & xy$y < max(ys[[w]])))
    
    if(!all(out)) break
    
    x <- x + 1
  } 
  
return(ts.df)  
  
}
