#internal warbleR function, not to be called by users. It is a modified version of seewave::fadew
# fades the start andend of an amplitude envelope
#last modification on feb-23-2018 (MAS)
fade_env_wrblr_int <- function(nvlp, fin = 0.1, fout = 0.2, shape = "linear") 
{

  if(fin + fout > 1) stop("'fin' + 'fout' cannot be higher than 1")
  if(fin < 0 | fout < 0) stop("'fin' and 'fout' cannot be negative")
  
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