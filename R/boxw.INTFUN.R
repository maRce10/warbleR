#internal warbleR function called by catalog type argument similar to par("bty") but added _ for only bottom and - for top
#xys must be a 4 element numeric vector c(x1, x2, y1, y2)

boxw.INTFUN <- function(xys =NULL, bty = "o", col = "black", lwd = 1, lty = 1){
  
  if(is.null(xys))
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
  
  if(bty == "c") cmb <- cmb[, -4]
  if(bty == "l") cmb <- cmb[, -(3:4)]
  if(bty == "7") cmb <- cmb[, 3:4]
  if(bty == "u") cmb <- cmb[, -3]
  if(bty == "^") cmb <- cmb[, -1]
  if(bty == "]") cmb <- cmb[, -2]
  if(bty == "_") cmb <- matrix(cmb[, 1], nrow = 4)
  if(bty == "-") cmb <- matrix(cmb[, 3], nrow = 4)
  
  for(i in 1:ncol(cmb)) 
    lines(x = cmb[1:2,i], y = cmb[3:4,i],  xpd = TRUE, col = col, lwd = lwd, lty = lty)
}
  
# author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on aug-3-2017 (MAS)

# # 
# lines(x = c(0, 0.5), y = c(0,8),  xpd = TRUE, col = col, lwd = lwd, lty = lty)
# 
# # 
# mc <- combn(c("in", "dev", "ndc", "nfc", "npc", "nic", "user", "inches"), 2)
# ys <- lc
# ys
# wh <- par("cra")
# 
# wh <- par("din")
# wh <- par("usr")
# # wh <- wh[c(2, 4)]
# wh <- dev.size()
# 
# 
# xs <- c(0, wh[1])
# ys <- c(0, wh[2])
# 
# m[2,]

# 
# 
# for(i in 26:ncol(mc))
# {
# Sys.sleep(2)
#   
#   # print(c(mc[,i]))
#   # X <- grconvertX(xs, from = mc[2,i], to = mc[1,i])
#   # 
#   # Y <- grconvertY(ys, from = mc[2,i], to = mc[1,i])
#   # 
#   # 
#   # cmb <- rbind(combn(rep(X, 2), 2), combn(rep(Y, 2), 2)[,c(2:6,1)])[,-c(3,6)]
#   # 
#   # 
#   # for(k in 1:ncol(cmb)) 
#   #   lines(x = cmb[1:2,k], y = cmb[3:4,k],  xpd = TRUE, col = "red", lwd = 3, lty = lty)
#   
#   
#   Sys.sleep(2)
#   
#   print(c(mc[2:1,i]))
#   X <- grconvertX(xs,from = mc[1,i], to = mc[2,i])
#   
#   Y <- grconvertY(ys, from = mc[1,i], to = mc[2,i])
# # print(grconvertY(lim[c(1,3)], from = mc[1,i], to = mc[2,i])- lc$y)
# # print(grconvertY(lim[c(1,3)], from = mc[2,i], to = mc[1,i])- lc$y)
# # print(ys$y)
# # print(grconvertX(lim[c(1,3)], from = mc[1,i], to = mc[2,i]) - lc$x)
# # print(grconvertX(lim[c(1,3)], from = mc[2,i], to = mc[1,i])- lc$x)
# # print(ys$x)
#   cmb <- rbind(combn(rep(X, 2), 2), combn(rep(Y, 2), 2)[,c(2:6,1)])[,-c(3,6)]
#   
# 
#   for(j in 1:ncol(cmb)) 
#     lines(x = cmb[1:2,j], y = cmb[3:4,j],  xpd = TRUE, col = "red", lwd = 5, lty = lty)
#   
#   }
# 
