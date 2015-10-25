#' Randomization test for singing coordination 
#' 
#' \code{coor.test} applies a Monte Carlo randomization test to assess the statistical significance of singing coordination
#' @usage coor.test(X, iterations = 1000, less.than.chance = TRUE, parallel = FALSE)
#' @param  X Data frame containing columns for singing event (sing.event), 
#' individual (indiv), and start and end time of signal (start and end).
#' @param iterations number of iterations for shuffling and calculation of the expected number of overlaps. Default is 1000.
#' @param less.than.chance Logical. If \code{TRUE} the test evaluates whether overlaps occur less often than expected by chance.
#' If \code{FALSE} the opposite pattern is evaluted (whether overlaps occur more often than expected by chance). 
#' Default is  \code{TRUE}.
#' @param parallel Either logical or numeric. Controls wehther parallel computing is applied.
#'  If \code{TRUE} 2 cores are employed. If numeric, it specifies the number of cores to be used. Not available for windows OS.
#' @return A data frame with the observed number of overlaps (obs.overlaps), mean number of overlaps expected by chance,
#' and p value.  
#' @export
#' @name coor.test
#' @details This function calculates the probability of finding and equal or lower number 
#' (of higher if \code{less.than.chance} is \code{TRUE}) of song overlaps in a coordinated singing event. 
#' The function shuffles the sequences of signals and silence-between-signals for both individuals to produce 
#' a null distribution of expected number of overlaps by chance.  The observed number of overlaps is compared to this
#' expected values. The p-values are calculated as the proportion of random expected values that were lower (or higher) 
#' than the observed value. The function runs one test for each singing event in the input data frame.  
#' @examples
#' \dontrun{
#' #######simulate singing events########
#' # create two sequences at different rates (not synchronize)
#' durs1 <- cumsum(rnorm(90,0.2, 0.01))
#' durs2 <- cumsum(rnorm(30,0.7, 0.01))
#' st.en1<-as.data.frame(matrix(durs1, ncol = 2, byrow = T))
#' st.en2<-as.data.frame(matrix(durs2, ncol = 2, byrow = T))
#' s1 <- data.frame(indiv = "a", st.en1)
#' s2 <- data.frame(indiv = "b", st.en2)
#' 
#' notsync<-data.frame(sing.event = "notsync", rbind(s1,s2))
#' 
#' # create two sequences at that overlap most of the time
#' 
#' durs1 <- cumsum(rnorm(90,c(0.4, 0.2), 0.01))
#' st.en1<-matrix(durs1, ncol = 2, byrow = T)
#' st2<-st.en1[,1]+rnorm(nrow(st.en1),0.1,0.05)
#' en2<-st2+rnorm(nrow(st.en1),0.2,0.01)
#' st.en2 <- cbind(st2, en2)
#' colnames(st.en2) <- colnames(st.en1)
#' s1 <- data.frame(indiv = "a", st.en1)
#' s2 <- data.frame(indiv = "b", st.en2)
#' 
#' ovlp<-data.frame(sing.event = "ovlp", rbind(s1,s2))
#' 
#' 
#' # create two sequences at that do not overlap most of the time
#' 
#' durs1 <- cumsum(rnorm(90,c(0.4, 0.2), 0.01))
#' st.en1<-matrix(durs1, ncol = 2, byrow = T)
#' st2<-st.en1[,1]+rnorm(nrow(st.en1), 0.25, 0.1)
#' en2<-st2+rnorm(nrow(st.en1), 0.2, 0.01)
#' st.en2 <- cbind(st2, en2)
#' colnames(st.en2) <- colnames(st.en1)
#' s1 <- data.frame(indiv = "a", st.en1)
#' s2 <- data.frame(indiv = "b", st.en2)
#' 
#' no.ovlp<-data.frame(sing.event = "no.ovlp", rbind(s1,s2))
#' 
#' 
#' #put all events together in a single data frame
#' colnames(ovlp) <- colnames(no.ovlp) <- colnames(notsync)
#' td<-rbind(ovlp, notsync, no.ovlp)
#' colnames(td)[3:4] <-c("start", "end")
#' 
#' #run test
#' coor.test(X = td, iterations = 10, less.than.chance = T, parallel = F)
#' 
#'  
#' # now try with some real data  
#' #load data
#' data(coor.sing)
#' 
#' # testing if coordination happens less than expected by chance
#' coor.test(coor.sing, iterations = 1000, less.than.chance = T)
#' 
#' # testing if coordination happens more than expected by chance
#' coor.test(coor.sing, iterations = 1000, less.than.chance = F)
#' }
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/})

coor.test <- function(X = NULL, iterations = 1000, less.than.chance = TRUE, parallel = FALSE)
{
  if(!is.data.frame(X))  stop("X is not a data frame")
  
  #stop if some events have less than 10 observations
  if(any(table(X$sing.event) < 10)) stop("At least one singing event with less than 10 vocalizations")
  
  #if iterations is not vector or length==1 stop
  if(any(!is.vector(iterations),!is.numeric(iterations))) stop("'interations' must be a numeric vector of length 1") else{
    if(!length(iterations) == 1) stop("'interations' must be a numeric vector of length 1")}
  
  #if there are NAs in start or end stop
  if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  interations <- round(iterations)
  
  #interations should be positive
  if(iterations < 1) stop("'iterations' must be a positive integer")
  
  #if parallel was called
  if (parallel) {lapp <- function(X, FUN) parallel::mclapply(X, 
  FUN, mc.cores = 2)} else    
    if(is.numeric(parallel)) lapp <- function(X, FUN) parallel::mclapply(X, 
        FUN, mc.cores = parallel) else lapp <- pbapply::pblapply
  
  tovlp<-lapp(unique(X$sing.event),function(h)
{
  sub<-X[X$sing.event==h,]
  
  #remove solo singing
  sub<-sub[order(sub$start),]
  
  fst <- max(c(which(sub$start==min(sub$start[sub$indiv==unique(sub$indiv)[1]])),
               which(sub$start==min(sub$start[sub$indiv==unique(sub$indiv)[2]])))) - 1
  
  lst <- min(c(which(sub$start==max(sub$start[sub$indiv==unique(sub$indiv)[1]])),
               which(sub$start==max(sub$start[sub$indiv==unique(sub$indiv)[2]])))) + 1
  
  sub <- sub[fst:lst, ]
  
  sub1<-sub[sub$indiv==unique(sub$indiv)[1], ]
  sub2<-sub[sub$indiv==unique(sub$indiv)[2], ]
  
  
  #determine which ones overlap (observed)
  ovlp<-sapply(2:nrow(sub),function(i) {
    if(sub$start[i]>sub$start[i-1] & sub$start[i]<sub$end[i-1])  
      "ovlp" else "No ovlp"})
  
  
  #null model
  #duration of signals
  dur1<-sub1$end-sub1$start
  dur2<-sub2$end-sub2$start
  
  #duration of gaps
  gap1<-sapply(1:(nrow(sub1)-1),function(x) {sub1$start[x+1]-sub1$end[x]})
  gap2<-sapply(1:(nrow(sub2)-1),function(x) {sub2$start[x+1]-sub2$end[x]})
  
  rov<-sapply(1:iterations,function(x){
    gap1<-gap1[sample(1:length(gap1),length(gap1))]
    gap2<-gap2[sample(1:length(gap2),length(gap2))]
    dur1<-dur1[sample(1:length(dur1),length(dur1))]
    dur2<-dur2[sample(1:length(dur2),length(dur2))]
    
    nbt1<-NULL
    for(i in 1:(length(dur1)-1))
    {nbt1[i]<-dur1[i]+gap1[i]
    if(i!=1) nbt1[i]<-nbt1[i]+nbt1[i-1]}  
    nbt1<-c(0,nbt1)
    nbt1<-nbt1+min(sub1$start)
    net1<-nbt1+dur1
    
    nbt2<-NULL
    for(i in 1:(length(dur2)-1))
    {nbt2[i]<-dur2[i]+gap2[i]
    if(i!=1) nbt2[i]<-nbt2[i]+nbt2[i-1]}  
    nbt2<-c(0,nbt2)
    nbt2<-nbt2+min(sub2$start)
    net2<-nbt2+dur2
    
    ndf<-data.frame(chan=c(rep(1,length(nbt1)),rep(2,length(nbt2))),nbt=c(nbt1,nbt2),net=c(net1,net2))
    ndf<-ndf[order(ndf$nbt),]
    rownames(ndf)<-1:nrow(ndf)
    
    rovlp<-sapply(2:nrow(ndf),function(i) {
      if(ndf$nbt[i]>ndf$nbt[i-1] & ndf$nbt[i]<ndf$net[i-1])  
        "ovlp" else "No ovlp"})
    return(length(rovlp[rovlp=="ovlp"]))
  })
  
  #resutls
  obs.overlaps <- length(ovlp[ovlp=="ovlp"])
  mean.random.ovlps <- mean(rov)
  if(less.than.chance) p <- length(rov[rov <= obs.overlaps])/iterations else p <- length(rov[rov >= obs.overlaps])/iterations
  l <- c(obs.overlaps, mean.random.ovlps, p)
  
  return(l)})

df <- data.frame(unique(X$sing.event), matrix(unlist(tovlp), ncol = 3, byrow = T))
colnames(df) <- c("sing.event", "obs.ovlps", "mean.random.ovlps", "p.value")

return(df)}