#' Count number of inflections in a frequency contour
#'
#' \code{inflections} counts the number of inflections in a frequency contour (or any time series)
#' @usage inflections(X = NULL, parallel = 1, pb = TRUE)
#' @param X data frame with the columns for "sound.files" (sound file name), "selec" (unique identifier for each selection) and columns for each of the frequency values of the contours. No other columns should be included
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}. 
#' @return A data frame with 3 columns: "sound.files", "selec" and "infls" (number of inflections).
#' @export
#' @name inflections
#' @details The function counts the number of inflections in a frequency contour.
#' @seealso \code{\link{dfts}}, \code{\link{trackfreqs}}, 
#' @examples{
#' # Set temporary working directory
# setwd(tempdir())
#' 
#' # get warbleR sound file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav") 
#' 
#' # measure frequency contours
#' dom.freq.ts <- dfts(X = selec.table)
#' 
#' # get number of inflections
#' inflections(X = dom.freq.ts)
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on mar-27-2018 (MAS)

inflections <- function(X = NULL, parallel = 1, pb = TRUE)
{

  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
infls.FUN <- function(Y, l) {
    
    if (l) ts <- Y$frequency else ts <- Y[ , !names(Y) %in% c("sound.files", "selec")]
    if(is.data.frame(ts)) ts <- unlist(ts)
    
    infls <- length(which(c(FALSE, diff(diff(ts) > 0) != 0)))
    
    Y$inflections <- infls  

    Y <- Y[1 , colnames(Y) %in% c("sound.files", "selec", "inflections"), drop = FALSE]
    
    return(Y)  
  }
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  if (is.data.frame(X)) lvs <- 1:nrow(X) else lvs <- 1:length(X)
  
  # run loop apply function
  out <- pbapply::pblapply(X = lvs, cl = cl, function(i) 
  { 
    is.df <- is.data.frame(X)
    
    if (is.df) Y <- X[i, , drop = FALSE] else Y <- X[[i]]
    
    return(infls.FUN(Y, l = !is.df)) 
  }) 

  return(do.call(rbind, out))

  }
