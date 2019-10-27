#' Randomization test for singing coordination 
#' 
#' Monte Carlo randomization test to assess the statistical significance of overlapping or alternating singing (or any other simultaneously occurring behavior).
#' @usage coor.test(X, iterations = 1000, ovlp.method = "count",
#' randomization = "keep.gaps", less.than.chance = TRUE, parallel = 1, pb = TRUE, 
#' rm.incomp = FALSE, cutoff = 2, rm.solo = FALSE)
#' @param  X Data frame containing columns for singing event (sing.event), 
#' individual (indiv), and start and end time of signal (start and end).
#' @param iterations number of iterations for shuffling and calculation of the expected number of overlaps. Default is 1000.
#' @param ovlp.method Character string defining the method to measure the amount of overlap. Two methods are accepted: 'count' and 'duration'. As the name suggests, the 'count' method will count the number of overlapping signals while 'duration' will measure the total duration (in s) in which signals overlap. Default is 'count'.
#' @param randomization Character string defining the procedure for signal randomization. Three methods are available:
#' \itemize{
#'  \item \code{keep.gaps} the position of both signals and gaps (i.e. intervals between signals) are randomized. Default.
#'  \item \code{sample.gaps} gaps are simulated using a lognormal distribution with
#'  mean and standard deviation derived from the observed gaps. Signal position is randomized.
#'  \item \code{keep.song.order} only the position of gaps is randomized.
#' }
#' More details in Masco et al. (2015).
#' @param less.than.chance Logical. If \code{TRUE} the test evaluates whether overlaps occur less often than expected by chance.
#' If \code{FALSE} the opposite pattern is evaluated (whether overlaps occur more often than expected by chance). 
#' Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param rm.incomp Logical. If \code{TRUE} removes the events that don't have 2 interacting individuals. Default is
#'  \code{FALSE}.
#' @param cutoff Numeric. Determines the minimum number of signals per individual in a singing event. Events not meeting 
#' this criterium are removed. Default is 2. 
#' Note that randomization tests are not reliable with very small sample sizes. Ideally 10 or more signals per individual 
#' should be available in each singing event.
#' @param rm.solo Logical. Controls if signals that are not alternated at the start or end of the 
#' sequence are removed (if \code{TRUE}). For instance, the sequence of signals A-A-A-B-A-B-A-B-B-B (in which A and B represent different individuals, as in the 'indiv' column) would be subset to 
#' A-B-A-B-A-B. Default is  \code{FALSE}.
#' @return A data frame with the following columns:
#' \itemize{
#'    \item \code{sing.event}: singing event ID
#'    \item \code{obs.overlap}: observed amount of overlap (counts or total duration, depending on overlap method, see 'ovlp.method' argument)
#'    \item \code{mean.random.ovlp}: mean amount of overlap expected by chance
#'    \item \code{p.value}: p value 
#'    \item \code{coor.score}: coordination score (\emph{sensu} Araya-Salas et al. 2017), 
#'    calculated as: 
#'    \deqn{(obs.overlap - mean.random.ovlp) / mean.random.ovlp} 
#'    Positive values indicate a tendency to overlap while negative values indicate a tendency to alternate. NA values will be returned when events cannot be randomized (e.g. too few signals). 
#'    }
#' @export
#' @name coor.test
#' @details This function calculates the probability of finding an equal or more extreme amount of song overlap (higher or lower) in a coordinated singing event (or any pair-coordinated behavior). 
#' The function shuffles the sequences of signals and silence-between-signals for both individuals to produce 
#' a null distribution of overlaps expected by chance. The observed overlaps is compared to this
#' expected values. The p-values are calculated as the proportion of random expected values that were lower (or higher) 
#' than the observed value. All procedures described in Masco et al. (2015) are implemented. In addition, either the number (\code{ovlp.method = "count"}) or the total duration (\code{ovlp.method = "duration"}) in which signals overlap can be used for estimating the overall degree of overlap. The function runs one test for each singing event in the input data frame. This function assumes that there are no overlaps between signals belonging to the same individual. See Masco et al. (2015) for recommendations on randomization procedures for specific signal structures.
#' @examples{
#' #load  simulated singing data (see data documentation)
#' data(sim_coor_sing)
#' 
#' # set global options
#' # this can also be set within the function call
#' warbleR_options(iterations = 100, pb = FALSE)
#' 
#' # testing if coordination happens less than expected by chance
#' coor.test(sim_coor_sing)
#' 
#' # testing if coordination happens more than expected by chance
#' coor.test(sim_coor_sing, less.than.chance = FALSE)
#' 
#' # using "duration" method and "keep.song.order" as randomization procedure
#' coor.test(sim_coor_sing, ovlp.method =  "duration", 
#' randomization = "keep.song.order")
#' }
#' 
#' @references 
#' {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals.
#'  Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' Araya-Salas M., Wojczulanis-Jakubas K., Phillips E.M., Mennill D.J., Wright T.F.
#'  (2017) To overlap or not to overlap: context-dependent coordinated singing in 
#'  lekking long-billed hermits. Animal Behavior  124, 57-65.
#' 
#' Masco, C., Allesina, S., Mennill, D. J., and Pruett-Jones, S. (2015). The Song 
#' Overlap Null model Generator (SONG): a new tool for distinguishing between random
#' and non-random song overlap. Bioacoustics.
#' 
#' Rivera-Caceres K, E Quiros-Guerrero E, M Araya-Salas, C Templeton & W Searcy. (2018). Early development of vocal interaction rules in a duetting songbird. Royal Society Open Science. 5, 171791.
#' 
#' Rivera-Caceres K, E Quiros-Guerrero, M Araya-Salas & W Searcy. (2016). Neotropical wrens learn new duet as adults. Proceedings of the Royal Society B. 285, 20161819
#' } 
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on apr-11-2018 (MAS)

coor.test <- function(X = NULL, iterations = 1000, ovlp.method = "count",
                      randomization = "keep.gaps", less.than.chance = TRUE, parallel = 1, pb = TRUE, 
                      rm.incomp = FALSE, cutoff = 2, rm.solo = FALSE)
{
  on.exit(pbapply::pboptions(type = .Options$pboptions$type))
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(coor.test)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
  # rename path for sound files
  names(opt.argms)[names(opt.argms) == "wav.path"] <- "path"
  
  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]
  
  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]
  
  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]
  
  # set options left
  if (length(opt.argms) > 0)
    for (q in 1:length(opt.argms))
      assign(names(opt.argms)[q], opt.argms[[q]])
  
  if (!is.data.frame(X))  stop("X is not a data frame")
  
  #stop if some cells are not labeled
  if (any(is.na(X$sing.event))) stop("NA's in singing event names ('sing.event' column) not allowed")
  
  if (any(is.na(X$indiv))) stop("NA's in individual names ('indiv' column) not allowed")  
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #remove hidden levels
  X <- droplevels(X)

  #remove solo singing
  if (rm.solo)
   {
    rmslX <- lapply(unique(X$sing.event), function(x)
     {  
    Y <- X[X$sing.event == x, ]
     
    Y <- Y[order(Y$start), ]
    
     fst <- max(c(which(Y$start==min(Y$start[Y$indiv==unique(Y$indiv)[1]])),
                 which(Y$start==min(Y$start[Y$indiv==unique(Y$indiv)[2]])))) - 1
    
    lst <- min(c(which(Y$start==max(Y$start[Y$indiv==unique(Y$indiv)[1]])),
                 which(Y$start==max(Y$start[Y$indiv==unique(Y$indiv)[2]])))) + 1
    
    Y <- Y[fst:lst, ]
  })
  
    X <- do.call(rbind, rmslX)
   
  }
  
  #stop if some events do not have 2 individuals 
  qw <- as.data.frame(tapply(X$sing.event, list(X$sing.event, X$indiv), length))
  qw2 <- qw 
  qw2[qw2 > 0] <- 1
  indiv.cnt <- apply(qw2, 1, sum, na.rm = TRUE)
  sng.cnt <- apply(qw, 1, function(x) any(na.omit(x) < cutoff))
   
  #complete singing events
    if (any(indiv.cnt != 2))
      if (rm.incomp){
      X <- X[X$sing.event %in% names(indiv.cnt)[indiv.cnt == 2], ]
      warning("Some events didn't have 2 interacting individuals and were excluded")
      } else warning("Some singing events don't have 2 interacting individuals ('indiv' column)")
  

# deal with cutoff value    
  if (any(sng.cnt))
    {
    X <- X[X$sing.event %in% names(indiv.cnt)[!sng.cnt], ]
    warning("Some individuals didn't have more songs that the 'cutoff' and the events were excluded")
  } 

    #if nothing was left
    if (nrow(X) == 0) stop("No events left after removing incomplete events")
    
  #if iterations is not vector or length==1 stop
  if (any(!is.vector(iterations),!is.numeric(iterations))) stop("'interations' must be a numeric vector of length 1") else{
    if (!length(iterations) == 1) stop("'interations' must be a numeric vector of length 1")}
  
  iterations <- round(iterations)
  
  #interations should be positive
  if (iterations < 1) stop("'iterations' must be a positive integer")
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # randomization function
  rndmFUN <- function(Y){
    
    Y <- Y[order(Y$start), ]
  
    #remove solo singing
    # if (rm.solo)
    # {  
    #   fst <- max(c(which(Y$start==min(Y$start[Y$indiv==unique(Y$indiv)[1]])),
    #                which(Y$start==min(Y$start[Y$indiv==unique(Y$indiv)[2]])))) - 1
    #   
    #   lst <- min(c(which(Y$start==max(Y$start[Y$indiv==unique(Y$indiv)[1]])),
    #                which(Y$start==max(Y$start[Y$indiv==unique(Y$indiv)[2]])))) + 1
    #   
    #   Y <- Y[fst:lst, ]
    # }
   
    Y1 <- Y[Y$indiv==unique(Y$indiv)[1], ]
    Y2 <- Y[Y$indiv==unique(Y$indiv)[2], ]
    
    #null model
    #duration of signals
    dur1<-Y1$end-Y1$start
    dur2<-Y2$end-Y2$start
    
    #duration of gaps
    gap1 <- sapply(1:(nrow(Y1) - 1),function(x) {Y1$start[x + 1] - Y1$end[x]})
    gap2 <- sapply(1:(nrow(Y2) - 1),function(x) {Y2$start[x + 1] - Y2$end[x]})   
    
    # randomize
    rnd.dfs <- lapply(1:iterations, function(x){
      
      # randomize gaps
      if (randomization %in% c("keep.gaps", "keep.song.order"))
      {
        gap1 <- sample(gap1)
        gap2 <- sample(gap2)
      }  
      
      if (randomization == "sample.gaps")
      {
        # generate gaps from lognormal distribution
        gap1 <- stats::rlnorm(n = length(gap1), meanlog = mean(log(c(gap1, gap2))), sdlog = stats::sd(log(c(gap1, gap2))))

        gap2 <- stats::rlnorm(n = length(gap2), meanlog = mean(log(c(gap1, gap2))), sdlog = stats::sd(log(c(gap1, gap2))))
      }
       
      #randomize signals
      if (randomization %in% c("keep.gaps", "sample.gaps"))
      {
        dur1 <- sample(dur1)
        dur2 <- sample(dur2)
      }
      
      # put all back together as a sequence of signals and gaps
      nbt1 <- NULL
      for(i in 1:(length(dur1) - 1))
      {nbt1[i] <- dur1[i] + gap1[i]
      if (i!=1) nbt1[i]<-nbt1[i]+nbt1[i-1]}  
      nbt1<-c(0,nbt1)
      nbt1<-nbt1+min(Y1$start)
      net1<-nbt1+dur1
      
      nbt2<-NULL
      for(i in 1:(length(dur2)-1))
      {nbt2[i]<-dur2[i]+gap2[i]
      if (i!=1) nbt2[i]<-nbt2[i]+nbt2[i-1]}  
      nbt2<-c(0,nbt2)
      nbt2<-nbt2+min(Y2$start)
      net2<-nbt2+dur2
      
      ndf <- data.frame(indiv = c(rep(1,length(nbt1)),rep(2,length(nbt2))),
                        start = c(nbt1,nbt2), 
                        end = c(net1,net2))
      
      ndf <- ndf[order(ndf$start), ]
      
      rownames(ndf) <- 1:nrow(ndf)
      
      return(ndf)

    })
    

    # add observed as the first element of list
    dfs <- c(list(Y), rnd.dfs)
    
    return(dfs)
    }
  
  # counting ovlp.method
  countFUN <- function(Z){
    
    # order by time and add duration
    Z <- Z[order(Z$start),]

    Z1 <- Z[Z$indiv == unique(Z$indiv)[1],]
    
    Z2 <- Z[Z$indiv == unique(Z$indiv)[2],]
    
    out <- sapply(1:nrow(Z1), function(i) {
      
      # target start and end
      trg.strt <- Z1$start[i]
      trg.end <- Z1$end[i]
      
      # get the ones that overlap
      return(sum(Z2$end > trg.strt & Z2$start < trg.end))
        })
  
    return(sum(out))
    
    }
  
  # duration ovlp.method
  durFUN <- function(Z){
    
    # order by time and add duration
    Z <- Z[order(Z$start),]
    Z$duration <- Z$end - Z$start
    
    Z1 <- Z[Z$indiv == unique(Z$indiv)[1],]
    
    Z2 <- Z[Z$indiv == unique(Z$indiv)[2],]
    
    out <- sapply(1:nrow(Z1), function(i) {
      
      # target start and end
      trg.strt <- Z1$start[i]
      trg.end <- Z1$end[i]
      
      # get the ones that overlap
      Z2 <- Z2[Z2$end > trg.strt & Z2$start < trg.end, , drop = FALSE]
      
      if (nrow(Z2) > 0) #set new start and end at edges of overlaping signals
      {
        if (any(Z2$start < trg.strt))
      trg.strt <- max(Z2$end[Z2$start < trg.strt])
      
      if (any(Z2$end > trg.end))
        trg.end <- min(Z2$start[Z2$end > trg.end])
      
      # new duration    
      no.ovlp.dur <- trg.end - trg.strt
      
      ovlp <- if (no.ovlp.dur > 0) Z1$duration[i] - no.ovlp.dur else Z1$duration[i]
      
      return(ovlp)
      }  else 
        return(0)
      
    })
  
    return(sum(out))
    }
  
  # select function/ovlp.method
  coortestFUN <- if (ovlp.method == 'count') countFUN else durFUN
      
      # set pb options 
      pbapply::pboptions(type = ifelse(pb, "timer", "none"))
      
      # set clusters for windows OS
      if (Sys.info()[1] == "Windows" & parallel > 1)
        cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
      
      # run loop apply function
      cote <- pbapply::pblapply(X = unique(X$sing.event), cl = cl, FUN = function(h) 
      { 
        ovlp <- try(sapply(rndmFUN(X[X$sing.event == h, ]), coortestFUN))
        
        if(class(ovlp) != "try-error")
        {  
        # get observed overlap (first element)
        obs.overlaps <- ovlp[1]
      
        # get random overlap (all except the first element)
        rov <- ovlp[-1]
        mean.random.ovlps <- mean(rov)
        
        # calculate p-value
        if (less.than.chance) p <- length(rov[rov <= obs.overlaps])/iterations else p <- length(rov[rov >= obs.overlaps])/iterations
        
        # coordination score
        if (obs.overlaps == 0 & mean.random.ovlps == 0)
          coor.score <- 0 else
          coor.score <- round((obs.overlaps - mean.random.ovlps) / mean.random.ovlps, 3)
        
        l <- data.frame(sing.event = h, obs.ovlp = obs.overlaps, mean.random.ovlp = mean.random.ovlps, p.value = p, coor.score)
        } else l <- data.frame(sing.event = h, obs.ovlp = NA, mean.random.ovlp = NA, p.value = NA, coor.score = NA)
        
        return(l)
        }
      )
    
  df <- do.call(rbind, cote)
    
return(df)
}


##############################################################################################################
#' alternative name for \code{\link{coor.test}}
#'
#' @keywords internal
#' @details see \code{\link{coor.test}} for documentation. \code{\link{coor.test}} will be deprecated in future versions.
#' @export

coor_test <- coor.test
