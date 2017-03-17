#internal warbleR function called by catalog
# par( mar = rep(0, 4))
# 
# plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
# 
# rect(xleft = -.04, ybottom = -1.04, xright = 0.5, ytop = 0.5, border = "black", col = "red", angle = 180, density = 10)
# par( mar = rep(0, 4))
# 
# plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
# 
# rect.w(xl = -.04, yb = -1.04, xr = 0.5, yt = 0.5, bor = "black", cl = "red", den = 10, ang = NULL, pattern = "diamond", lw = 2)

rectw <- function(xl, yb, xr, yt, bor, cl, ang = NULL, den = NULL, pattern = "no.pattern", lw = 2.5)
  {
 if(pattern == "no.pattern")
   rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt, border = bor, col = cl, angle = ang, density = den, lwd = lw) 
  else {

       if(pattern == "diamond")
    {      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor , col = cl, density = den, angle = 45, lwd = lw)
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt ,border = bor , col = cl, density = den, angle = 135, lwd = lw)
      }

    if(pattern == "grid")
    {      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 0, lwd = lw)
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 90, lwd = lw)
    }

    if(pattern == "forward")
    rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 45, lwd = lw)

    if(pattern == "backward")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 315, lwd = lw)

    if(pattern == "vertical")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 90, lwd = lw)

    if(pattern == "horizontal")
      rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , border = bor ,col = cl, density = den, angle = 0, lwd = lw)
  }
}
