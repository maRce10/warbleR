#' Measure acoustic parameters at the song level
#'
#' \code{song_param} measures average or extreme values of acoustic parameters of 
#' elements in a song 
#' @usage song_param(X = NULL, weight = NULL, song_colm = "song",
#' mean_indx = NULL, min_indx = NULL, max_indx = NULL, parallel = 1, pb = TRUE,
#' path = NULL)
#' @param X 'selection.table' object or data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input data frame. Other data frames can be used as input, but must have at least the 4 columns mentioned above.
#' @param weight Character vector defining 1 or more numeric vectors to weight average
#' measurements (i.e. song parameters). Default is \code{NULL}. Names of numeric columns in 'X' can also be used. See \code{\link[stats]{weighted.mean}}. 
#'  for more details. To use unweighted average set 'weight' to \code{NULL}.
#' @param song_colm Character string with the column name containing song labels. Note that 
#' the function assumes that song labels are not repeated within a sound file.
#' @param mean_indx Numeric vector with the index of the columns that will be averaged. If \code{NULL} the mean of all numeric columns in 'X' is returned.
#' @param min_indx Numeric vector with the index of the columns for which the minimum 
#' value is needed. Default is \code{NULL}.
#' @param max_indx Numeric vector with the index of the columns for which the maximum 
#' value is needed. Default is \code{NULL}.
#' If  \code{NULL} the mean of all numeric columns in 'X' is returned. 
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}. 
#' @param path Character string containing the directory path where the sound file are located. 
#' @return A data frame similar to the input 'X' data frame, containing the mean
#'  values for numeric acoustic parameters. Parameters to average can be defined with
#'  'mean_indx' (otherwhise all numeric parameters are used). Parameters can be 
#'  weighted by other parameters in the data set (e.g. duration, frequency range). Note
#'  that the functions works by default on songs, but can be used at other hierarchical
#'  levels (e.g. syllables, singing bouts). This function assumes that song labels are
#'   not repeated within a sound file.  
#' 
#' @export
#' @name song_param
#' @details The function removes silence segments (i.e. segments with very low amplitude values) from wave files. 
#' @seealso \code{\link{fixwavs}}, \code{\link{autodetec}}, 
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
#' # add a 'song' column
#' selec.table$song <- rep(1:4, each = 3)[1:11]
#' 
#' # measure acoustic parameters
#' sp <- specan(selec.table, bp = c(1, 11), 300, fast = TRUE)
#' 
#' # add song data
#' sp <- merge(sp, selec.table, by = c("sound.files", "selec"))
#' 
#' # caculate song-level parameters for all numeric parameters
#' song_param(X = sp, song_colm = "song", parallel = 1, pb = TRUE)
#' 
#' # caculate song-level parameters selecting parameters with mean_indx
#' song_param(X = sp, song_colm = "song",mean_indx = 5:10, parallel = 1, pb = TRUE)
#' 
#' # caculate song-level parameters for selecting parameters with mean_indx, max_indx
#' # and min_indx and weighted by duration
#' song_param(X = sp, weight = "duration", song_colm = "song",
#' mean_indx = 5:6, min_indx = 7:8, max_indx = 9:10, parallel = 1, pb = TRUE)
#' 
#'# with two weights 
#'song_param(X = sp, weight = c("duration", "dfrange"), song_colm = "song",
#'mean_indx = 5:9, parallel = 1, pb = TRUE)
#'
#'# with two weights no progress bar
#'song_param(X = sp, weight = c("duration", "dfrange"), song_colm = "song",
#'mean_indx = 5:9, parallel = 1, pb = FALSE)
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on mar-24-2018 (MAS)

song_param <- function(X = NULL, weight = NULL, song_colm = "song",
                       mean_indx = NULL, min_indx = NULL, max_indx = NULL, 
                       parallel = 1, pb = TRUE, path = NULL)
{

  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else {if (!file.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  if (!any(names(X) == song_colm)) stop("'song_colm' not found")

  if (is.null(X$duration)) X$duration <- X$end - X$start

  if (!any(names(X) %in% weight) & !is.null(weight)) stop("'weight' column not found")
  
  if(!all(sapply(X[, mean_indx], is.numeric))) stop("not all columns in 'mean_indx' are numeric")
  
  songparam.FUN <- function(Y, song_colm, mean_indx, min_indx, max_indx, weight) {
    
    Z <- Y[1, , drop = FALSE]
    
    # set weight and calculated mean parameters
    if (!is.null(weight)  & nrow(Y) > 1){
    wts <-  Y[, weight,  drop = FALSE]
    
    if (length(weight) > 1) {
      wts <- apply(wts, 2, function(e) e/max(e, na.rm = TRUE))
      wts <- apply(wts, 1, prod)
        }
    wts <- as.vector(unlist(wts))
    } else wts <- rep(1, nrow(Y))
    
    if (is.null(mean_indx)) {
      mean_indx <- which(sapply(X, is.numeric))
      mean_indx <- as.vector(mean_indx[!names(mean_indx) %in% c("selec", "channel", "song", "start", "end", "top.freq", "bottom.freq", min_indx, max_indx)])
      mean_indx <- mean_indx[!is.na(mean_indx)]
      }
    
    for(u in mean_indx) Z[ ,u] <- stats::weighted.mean(x = as.vector(c(Y[, u, drop = TRUE])), w = wts, na.rm = TRUE)
    
    # set weight and calculated mean parameters
    if (!is.null(min_indx)) for(u in min_indx)  Z[ ,u]  <- min(x = as.vector(c(Y[, u, drop = TRUE])), na.rm = TRUE)
    
    if (!is.null(max_indx)) for(u in max_indx) Z[ ,u]  <- max(x = as.vector(c(Y[, u, drop = TRUE])), na.rm = TRUE)
    
    indx <- c(mean_indx, min_indx, max_indx)
    
    Z <- Z[, c(names(Z) %in% c("sound.files", song_colm, names(Z)[indx]))]
    Z <- Z[, match(c("sound.files", "song", names(Y)[indx]), names(Z))]
    
    Z$num.elemts <- nrow(Y)
    Z$start <- min(Y$start)
    Z$end <- max(Y$end)
    try(Z$bottom.freq <- min(Y$bottom.freq), silent = TRUE)
    try(Z$top.freq <- max(Y$top.freq), silent = TRUE)
    try(Z$freq.range <- Z$top.freq - Z$bottom.freq, silent = TRUE)
    Z$duration <- Z$end - Z$start
    Z$note.rate <- Z$num.elemts/Z$duration
    
    return(Z)  
  }
  
  X$.....SONGX... <- paste(X$sound.files, X$song) 
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  out <- pbapply::pblapply(X = unique(X$.....SONGX...), cl = cl, function(i) 
  { 
    Y <- X[X$.....SONGX... == i, , drop = FALSE]
    
    return(songparam.FUN(Y, song_colm, mean_indx, min_indx, max_indx, weight)) 
  }) 

return(do.call(rbind, out))
}
