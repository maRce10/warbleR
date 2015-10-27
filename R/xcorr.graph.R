#' Pairwise plots of spectrogram cross-correlation scores 
#' 
#' \code{xcorr.graph} Generates pairwise plots showing the spectrogram cross-correlation scores
#' against the time sliding. 
#' @usage xcorr.graph(X, cex.cor = 1, cex.lab = 1, rel.cex = FALSE)
#' @param  X Output from \code{\link{xcorr}} function.
#' @param cex.cor A numeric vector of length 1 giving the amount by which correlation scores (in 
#' the upper triangle of the multipannel plot) should be magnified. Default is 1.
#' @param cex.lab A numeric vector of length 1 giving the amount by which signal selection labels 
#' (in diagonal of the multipannel plot) should be magnified. Default is 1.
#' @param rel.cex Logical. Controls whether the size of the correlation scores (in 
#' the upper triangle of the multipannel plot) should be relative to the correlation score.
#' Default is \code{FALSE}.
#' @export
#' @name xcorr.graph
#' @details This function generates pairwise plots of the spectrogram cross-correlation scores
#' by sliding step. The function takes the output of \code{\link{xcorr}} as input.  The colors of 
#' the lines in the lower triangle of the plot matrix represent the strenght of the similarity between
#' the two signals. The x axis shows the time difference between the two signals for each sliding step
#' (0 means perfectly centered signals). Note that large number of signals may not display well in the 
#' default graphic device. In such cases saving the plot as and image file is adviced.   
#' @examples
#' \dontrun{
#' #load data
#' #First set temporal working directory
#' setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2", "manualoc.df"))
#' writeWave(Phae.long1, "Phae.long1.wav") #save sound files
#' writeWave(Phae.long2, "Phae.long2.wav")

#'
#'  #run cross correlation first
#'  xcor<-xcorr(X = manualoc.df[1:5,], wl =300, frange= c(2, 9), ovlp=90, 
#'  dens=0.8, wn='hanning', cor.method = "pearson") 
#'  
#'  #plot pairwise scores
#'  xcorr.graph(X = xcor, cex.cor = 2, cex.lab = 1.3, rel.cex = T)
#' }
#' @seealso \code{\link{xcorr}}
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/})
#' @export

xcorr.graph<-function(X, cex.cor = 1, cex.lab = 1, rel.cex = FALSE) {
 
  #if X is not provided or is not a list
  if(!is.list(X))  stop("X is not a list. It should be the output of 'xcorr' function")
  
  #if cex.cor is not vector or length!=1 stop
  if(!is.numeric(cex.cor)) stop("'cex.cor' must be a numeric vector of length 1") else {
    if(!is.vector(cex.cor)) stop("'cex.cor' must be a numeric vector of length 1") else{
      if(!length(cex.cor) == 1) stop("'cex.cor' must be a numeric vector of length 1")}} 
  
  #if cex.lab is not vector or length!=1 stop
  if(!is.numeric(cex.lab)) stop("'cex.lab' must be a numeric vector of length 1") else {
    if(!is.vector(cex.lab)) stop("'cex.lab' must be a numeric vector of length 1") else{
      if(!length(cex.lab) == 1) stop("'cex.lab' must be a numeric vector of length 1")}} 
  
  #rel.cex must be logical
  if(!is.logical(rel.cex)) stop("'rel.cex' must be logical") else
  if(!length(rel.cex) == 1) stop("'rel.cex' must be a logical vector of length 1") 

  y <- X$correlation.data
  y <- y[order(y$sound.file1, y$sound.file2), ]
  w <- y[ave(-y$score, y$dyad, FUN = rank) <= 1, ]
  w <- w[order(w$sound.file1, w$sound.file2), ]
 
#split graph device  
levs <- length(unique(y$sound.file1)) + 1
mar1 <- mar <- rep(2.3/levs, 4)
if(levs > 5) ax.mar <- mar[1] * 35 else ax.mar <- mar[1] * 10
split.screen(c(levs,levs)) 

lvs <- 1 + levs
repeat{  lvs <- c(lvs,lvs[length(lvs)] + levs)
       if(length(lvs) == (levs -1)) break}

z <- x <- 1
updiag <- NULL
for(j in 1:(levs - 1))
{

  #plot scores line plots in lower triangle
  for(i in lvs)
{  screen(i)
if(z == 1) mar1[2] <- mar[2] * ax.mar else mar1 <- mar
    if(i == lvs[length(lvs)]) mar1[1] <- mar[1] * ax.mar
  par(mar = mar1)
  plot(seq(-1, 1, length.out = length(y$time[y$dyad==unique(y$dyad)[x]])), y$score[y$dyad==unique(y$dyad)[x]], 
       type = "l", col = heat.colors(10)[round((1 - w$score[x])*10, 0)], 
     ylim = c(0,1), yaxt = "n", xaxt = "n", xlab = "", ylab = "", lwd = 5, xlim = c(-1,1))
if(z == 1 & x == floor(levs/2)) mtext("Correlation coeff.", side = 2, line = 1, cex = 0.8 * cex.lab)
  if(z == 1) axis(side = 2, line =-1, at = c(0, 1),tick = F)
  if(i == lvs[length(lvs)]) axis(side = 1, line =-1, at = c(-1, 0, 1),tick = F)
  if(i == lvs[length(lvs)] & i == ceiling((levs^2)-levs/2)) mtext("Time diff", 
                  side = 1, line = 1, cex = 0.8 * cex.lab)
  x <- x + 1}

#plot selection name in diagonal
  screen(min(lvs)-levs) 
  mar1 <- mar
  if(z == 1) mar1[2] <- mar[2] * ax.mar 
  par(mar = mar1)
  plot(1, 1, col ="white",  xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  text(1, 1, unique(c(as.character(y$sound.file1), as.character(y$sound.file2) ))[z], srt =45,
       cex =0.5 * cex.lab, col = "blue4")
  z <- z + 1
  
  updiag<-append(updiag,c(lvs, min(lvs)-levs))
  lvs <- lvs[2:length(lvs)]+1
}

#plot last selection
screen(levs * levs) 
mar1[1] <- mar[1] * ax.mar
par(mar = mar1)
plot(1, 1, col ="white",  xaxt = "n", yaxt = "n", xlab = "", ylab = "")
text(1, 1, unique(c(as.character(y$sound.file1), as.character(y$sound.file2) ))[z], srt =45,
     cex =0.5 * cex.lab, col = "blue4")

#plot max scores in upper triangle
a <- 2:((levs * levs)-1)
a <- a[!a %in% updiag]

x <- 1
for(i in a)
{ 
  screen(i)
  par(mar= mar)
  plot(1, 1, col ="white",  xaxt = "n", yaxt = "n", xlab = "", ylab = "")
if(rel.cex) cex <- w$score[x]*cex.cor else cex <- cex.cor
  text(1, 1, round(w$score[x], 2), cex = cex)
x <- x + 1}
on.exit(close.screen(all.screens = TRUE))
}