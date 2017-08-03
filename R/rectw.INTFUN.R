#internal warbleR function called by catalog

rectw.INTFUN <- function(xl, yb, xr, yt, bor, cl, ang = NULL, den = NULL, pattern = "no.pattern", lw = 2, lt = 1)
{
  if(pattern == "no.pattern")
    rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt, border = bor, col = cl, angle = ang, density = den, lwd = lw, lty = lt) 
  else {
    
    rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt, border = bor, col = "white", angle = ang, density = den, lwd = lw, lty = lt) 
    
    if(pattern == "diamond")
    {      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor , col = cl, density = den, angle = 45, lwd = lw, lty = lt)
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt ,border = bor , col = cl, density = den, angle = 135, lwd = lw, lty = lt)
    }
    
    if(pattern == "grid")
    {      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 0, lwd = lw, lty = lt)
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 90, lwd = lw, lty = lt)
    }
    
    if(pattern == "forward")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 45, lwd = lw, lty = lt)
    
    if(pattern == "backward")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 315, lwd = lw, lty = lt)
    
    if(pattern == "vertical")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 90, lwd = lw, lty = lt)
    
    if(pattern == "horizontal")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 0, lwd = lw, lty = lt)
  }
  
}
# author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on aug-3-2017 (MAS)