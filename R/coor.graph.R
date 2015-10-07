#' Coordinated singing graphs
#' 
#' \code{coor.graph} creates graphs of coordinated singing and highlights the signals that overlap 
#' in time. The signals are represented by polygons of different colors.
#' @usage coor.graph(X, only.coor = FALSE, ovlp = TRUE, xl = 1,  res= 80, it = "jpeg")
#' @param  X Data frame with results containing columns for singing event (sing.event), 
#' individual (indiv), and start and end time of signal (start and end).
#' @param only.coor Logical. If \code{TRUE} only the segment in which both individuals are singing is 
#' included (solo singing is removed). Default is \code{FALSE}.
#' @param ovlp Logical. If \code{TRUE} the vocalizations that overlap in time are highlighted. 
#' Default is \code{TRUE}.
#' @param xl Numeric vector of length 1, a constant by which to scale 
#'   spectrogram width. Default is 1.
#' @param res Numeric argument of length 1. Controls image resolution. Default is 80.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @return Graphs of the singing events in the input data frame are saved in the working directory.
#' @export
#' @name coor.graph
#' @details This function provides visualization for coordination of acoustic signals. Signals are shown as
#' polygon across a time axis. It also shows which signals overlap, the amount of overlap, and 
#' highlights the individual responsible for the overlap using a color code. The width of the polygons 
#' depicting the time of overlap. 
#' @examples
#' \dontrun{
#' #load data
#' data(coor.sing)
#' 
#' # make coor.graphs in jpeg format
#' coor.graph(X = coor.sing, ovlp = T,only.coor = F, xl =2, res =80)
#' 
#' # make coor.graphs in tiff format
#' coor.graph(X = coor.sing, ovlp = T,only.coor = F, xl =2, res =80, it = "jpeg")                  
#' }
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/})

coor.graph <- function(X = NULL, only.coor = FALSE, ovlp = TRUE, xl = 1,  res= 80, it = "jpeg") { 
  
  #if xl is not vector or length!=1 stop
  if(is.null(xl)) stop("'xl' must be a numeric vector of length 1") else {
    if(!is.vector(xl)) stop("'xl' must be a numeric vector of length 1") else{
      if(!length(xl) == 1) stop("'xl' must be a numeric vector of length 1")}}  
  
  #if res is not vector or length==1 stop
  if(!is.vector(res)) stop("'res' must be a numeric vector of length 1") else{
    if(!length(res) == 1) stop("'res' must be a numeric vector of length 1")}
  
  X$sing.event <- as.character(X$sing.event)
  
  invisible(pblapply(unique(X$sing.event), function(x)
  {
    
    y <- X[X$sing.event == x, ]
    y <- y[!is.na(y$start), ]
    y <- y[!is.na(y$end), ]
    y <- y[order(y$start), ]
    
    
    if(only.coor){
      fst <- max(c(which(y$start==min(y$start[y$indiv==unique(y$indiv)[1]])),
                   which(y$start==min(y$start[y$indiv==unique(y$indiv)[2]])))) - 1
      
      lst <- min(c(which(y$start==max(y$start[y$indiv==unique(y$indiv)[1]])),
                   which(y$start==max(y$start[y$indiv==unique(y$indiv)[2]])))) + 1
      
      y <- y[fst:lst, ]
    }
    
    #data for indiv 1
    y1<-y[y$indiv == unique(y$indiv)[1],]
    
    #data for indiv 2
    y2<-y[y$indiv == unique(y$indiv)[2],]
    
    if(any(nrow(y1) == 0, nrow(y2) == 0)) paste("in", x,"singing bouts do not overlap in time") else
    {
      df1 <- data.frame(xmin = y1$start,ymin = rep(0.8,nrow(y1)),xmax = y1$end,
                        ymax = rep(1.1,nrow(y1)), id = unique(y$indiv)[1], col = "#F9766E")
      
      df2 <- data.frame(xmin = y2$start,ymin = rep(1.4,nrow(y2)),xmax = y2$end,
                        ymax = rep(1.7,nrow(y2)), id = unique(y$indiv)[2], col = "#00BFC4")
      
      df<-rbind(df1, df2)
      
      #dtermine which ones overlap
      if(ovlp) {
        btc<-max(c(min(y1$start),min(y2$start)))
        z<-y[c((which(y$start==btc)-1):nrow(y)),] 
        
        et1<-max(z$start[z$indiv == unique(z$indiv)[1]])
        et2<-max(z$start[z$indiv == unique(z$indiv)[2]])
        etc<-min(c(et1,et2))
        z<-z[1:c((which(z$start==etc)+1)),]
        
        ov<-sapply(2:nrow(z),function(i) {
          if(z$start[i] > z$start[i-1] & z$start[i] < z$end[i-1])  
            "ovlp" else "No ovlp"})
        
        if(length(ov[ov == "ovlp"])>0)
        {z1<-data.frame(z,ovl=c("No ovlp", ov))
        
        recdf <- NULL
        for(i in 1:nrow(z1))
        {if(z1$ovl[i]=="ovlp") {
          if(z1$indiv[z1$start==min(z1$start[i],z1$start[i-1])] == unique(z$indiv)[1]) 
          {col<-"#00BFC496"
          id <-"#!"}  else {col <-"#F9766E96"
          id <- "^%"}
          if(is.null(recdf)) recdf <- data.frame(xmin = mean(z1$start[i],z1$end[i]), xmax = min(z1$end[i],z1$end[i-1]),
                                                 ymin = 1.1, ymax = 1.4, id,  col) else
                                                   recdf <- rbind(recdf, data.frame(xmin = mean(z1$start[i],z1$end[i]), xmax = min(z1$end[i],z1$end[i-1]),
                                                                                    ymin = 1.1, ymax = 1.4, id,  col))
        }}
        
        df<-rbind(df,recdf)
        
        }
      }
      
      cols <- c("#F9766E66", "#00BFC466")
      
      if(all(exists("recdf"), ovlp)) if(nrow(recdf) > 0) {if(suppressWarnings(min(which(df$id == "#!"))) < suppressWarnings(min(which(df$id == "^%"))))
      {  cols <- c("#00BFC466", "#F9766E66") 
      ids <- c(unique(y$indiv)[2], unique(y$indiv)[1])} else {cols <- c("#F9766E66", "#00BFC466")
      ids <- c(unique(y$indiv)[1], unique(y$indiv)[2])}} else {cols <- c("#F9766E66", "#00BFC466")
      ids <- c(unique(y$indiv)[1], unique(y$indiv)[2])}
      
      ggp <- ggplot2::ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = id, fill = col)) +
        ggplot2::geom_rect() +
        ggplot2::scale_x_continuous("Time (s)") +
        ggplot2::scale_y_continuous(name = NULL, breaks= c(0.95, 1.55), labels = c(unique(y$indiv)[1],unique(y$indiv)[2])) +
        ggplot2::scale_fill_manual(values=c("#F9766E","#00BFC4",cols), name = "", 
                          labels=c(unique(y$indiv)[1],unique(y$indiv)[2], paste("overlap from", ids[1]),
                                   paste("overlap from", ids[2]))) + 
        ggplot2::theme(legend.position="top")
      
      if(it == "jpeg") ite <- "coor.singing.jpeg" else ite <- "coor.singing.tiff"
      ggplot2::ggsave(plot = ggp, filename = paste(x, ite, sep = "-"),
             dpi= 160,units = "in", width = 9 * xl,height = 5.5)
    }
  }))
}