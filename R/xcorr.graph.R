#' Pairwise plots of spectrogram cross-correlation scores 
#' 
#' \code{xcorr.graph} Generates pairwise plots showing the spectrogram cross-correlation scores
#' against the time sliding. 
#' @usage xcorr.graph(X, cex.cor = 1, cex.lab = 1, cex.axis.lab = 1, rel.cex = FALSE, labs = NULL)
#' @param  X Output from \code{\link{xcorr}} function.
#' @param cex.axis.lab A numeric vector of length 1 giving the amount by which the axis labels should be magnified. Default is 1. 
#' @param cex.cor A numeric vector of length 1 giving the amount by which correlation scores (in 
#' the upper triangle of the multipannel plot) should be magnified. Default is 1.
#' @param cex.lab A numeric vector of length 1 giving the amount by which signal selection labels 
#' (in diagonal of the multipannel plot) should be magnified. Default is 1.
#' @param rel.cex Logical. Controls whether the size of the correlation scores (in 
#' the upper triangle of the multipannel plot) should be relative to the correlation score.
#' @param labs Alternative selection labels. If not provided the combined name of sound files and selection numbers are used as labels.
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
#' #First set temporal working directory]
#' setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2", "manualoc.df"))
#' writeWave(Phae.long1, "Phae.long1.wav") #save sound files
#' writeWave(Phae.long2, "Phae.long2.wav")
#'
#'  #run cross correlation first
#'  xcor<-xcorr(X = manualoc.df[1:5,], wl =300, frange= c(2, 9), ovlp=90, dens=0.8, wn='hanning', 
#'  cor.method = "pearson") 
#'  
#'  #plot pairwise scores
#'   xcorr.graph(X = xcor, cex.cor = 2, cex.lab = 1, rel.cex = FALSE)
#' }
#' @seealso \code{\link{xcorr}}
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#' @export


xcorr.graph<-function(X, cex.cor = 1, cex.lab = 1,  cex.axis.lab=1, rel.cex = FALSE, labs = NULL) {
  
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
  
  #labs right length
  if(!is.null(labs)) if(length(labs)!=length(unique(c(as.character(y$sound.file1), as.character(y$sound.file2)))))
    stop("'labs' is not the same length as the number of selections")
  
  #create labels
  if(is.null(labs)) labs <- unique(c(as.character(y$sound.file1), as.character(y$sound.file2)))
  
  #split graph device  
  levs <- length(unique(y$sound.file1)) + 1
  mar <- rep(2.3/levs, 4)
  
  s<-seq(0.08,1,length.out = levs+1)
  rws<-lapply(1:(levs),function(x)
  {c(s[x:(x+1)])})
  
  m<-do.call("rbind",lapply(1:(length(rws)), function(x){
    y<-0
    a<-matrix(ncol = 4, nrow = levs)
    repeat{a[y+1,]<-c(rws[[x]],rws[[y+1]])
    y=y+1
    if(y==length(rws)) break}
    return(a)    
  }))
  
  m<-m[order(m[,1],-m[,3]),]
  
  mat<-matrix(1:nrow(m),nrow=levs,ncol=levs)
  
  split.screen(m)
  sidepan<-which(m[,1]==min(m[,1]))
  bottompan<-which(m[,3]==min(m[,3]))
  x <- 1
  
  # plot line plots in lower triangle
  for(i in mat[lower.tri(mat)])
  {  screen(i)
    par(mar = mar)
    plot(1, 1, col ="white", ylim = c(0,1), xlab = "", ylab = "", yaxt = "n", xaxt = "n", lwd = 5, xlim = c(-1,1))
    if(i %in% sidepan) 
    {      axis(side = 2, line =0, at = seq(0, 1,length.out = 5),tick = T, cex=0.2,labels = F)
      axis(side = 2, line =-0.6, at = seq(0, 1,length.out = 5),tick = F, cex.axis=0.7,labels = round(seq(0, 1,length.out = 5),1))
    }
    if(i %in% bottompan) 
    {      axis(side = 1, line =0, at = seq(-1, 1,length.out = 5),tick = T, cex=0.2,labels = F)
      axis(side = 1, line =-0.6, at = seq(-1, 1,length.out = 5),tick = F, cex.axis=0.7,labels = round(seq(-1, 1,length.out = 5),1))
    }
    
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "ivory")
    abline(h = seq(0, 1, length.out = 2), col = "gray90", lwd = 1.1)
    abline(h = seq(0, 1, length.out = 4), col = "gray90", lwd = 0.9)
    abline(v = seq(-1, 1, length.out = 7) , col = "gray90", lwd = 0.9)
    abline(v = seq(-1, 1, length.out = 4), col = "gray90", lwd = 1.1)
    box()
    lines(seq(-1, 1, length.out = length(y$time[y$dyad==unique(y$dyad)[x]])), y$score[y$dyad==unique(y$dyad)[x]], lwd = 2.5,
          col = "black")
    x <- x + 1
  }
  
  z <-1
  
  #plot selection name in diagonal
  for(i in mat[diag(mat)]){
    screen(i) 
    par(mar = rep(0.2,4))
    plot(1, 1, col ="white",   xlab = "", ylab = "", tck=0.1,  yaxt = "n", xaxt = "n")
    # axis(side = 1, line =-1, at = c(-1, 0, 1),tick = F)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "ivory")
    text(1, 1, labs[z], srt =45, cex =0.5 * cex.lab )
    z <- z + 1
    box(lwd = 2)}
  
  x <- 1
  
  #plot correlation coefficients in upper triangle
  
  v<-m[mat[upper.tri(mat)],]
  
  for(i in mat[upper.tri(mat)][order(-v[,3],v[,1])]){
    screen(i)
    par(mar= mar)
    plot(1, 1, col ="white",  xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = c("#67001F", "#B2182B", "#D6604D", "#F4A582","#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")[round((1 - w$score[x])*10, 0)], border = "black")
    if(rel.cex) cex <- w$score[x]*cex.cor else cex <- cex.cor
    text(1, 1, round(w$score[x], 2), cex = cex)
    x <- x + 1}
  
  title(ylab = "Correlation coefficient",
        outer = TRUE, line = -1.3, cex.lab= cex.axis.lab)
  title(xlab = "Sliding time difference",
        outer = TRUE, line = -0.9, cex.lab= cex.axis.lab)
  on.exit(close.screen(all.screens = TRUE))
}
