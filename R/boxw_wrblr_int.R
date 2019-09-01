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
  
# author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on aug-3-2017 (MAS)
