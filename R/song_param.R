#' Measure acoustic parameters at the song level
#'
#' \code{song_param} measures average or extreme values of acoustic parameters of 
#' elements in a song 
#' @usage song_param(X = NULL, weight = NULL, song_colm = "song",
#' mean_indx = NULL, min_indx = NULL, max_indx = NULL, sd = FALSE, 
#' parallel = 1, pb = TRUE, na.rm = FALSE)
#' @param X 'selection_table', 'extended_selection_table' (created 'by.song') or data frame with the following columns: 1) "sound.files": name of the .wav 
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
#' @param sd Logical value indicating whether standard deviaton is also returned for
#' variables in which averages are reported. Default is \code{FALSE}.
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}. 
#' @param na.rm Logical value indicating whether 'NA' values should be ignored for calculations.
#' @return A data frame similar to the input 'X' data frame, containing the mean
#'  values for numeric acoustic parameters. Parameters that will be averaged can be defined with
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
#' 
#' # add a 'song' column
#' selec.table$song <- rep(1:4, each = 3)[1:11]
#' 
#' # measure acoustic parameters
#' sp <- specan(selec.table[1:8, ], bp = c(1, 11), 300, fast = TRUE)
#' 
#' # add song data
#' sp <- merge(sp, selec.table[1:8, ], by = c("sound.files", "selec"))
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
#last modification on may-8-2018 (MAS)

song_param <- function(X = NULL, weight = NULL, song_colm = "song",
                       mean_indx = NULL, min_indx = NULL, max_indx = NULL, 
                       sd = FALSE, parallel = 1, pb = TRUE, na.rm = FALSE)
{
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(song_param)
  
  # get warbleR options
  opt.argms <- .Options$warbleR
  
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
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  # if extended only by song
  if (is_extended_selection_table(X))
    if (!attributes(X)$by.song$by.song) stop("extended selection tables must be created 'by.song' to be used in song.param()")

  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  if (!any(names(X) == song_colm)) stop("'song_colm' not found")
  
  if (song_colm == "sound.files") {
    X$song <- X$sound.files
    song_colm <- "song"
    }
  
  
  if (!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  if (is.null(X$duration)) X$duration <- X$end - X$start

  if (!any(names(X) %in% weight) & !is.null(weight)) stop("'weight' column not found")
  
  if (!is.null(mean_indx))
  if (!all(sapply(X[, mean_indx], is.numeric))) stop("not all columns in 'mean_indx' are numeric")
  
  songparam.FUN <- function(Y, song_colm, mean_indx, min_indx, max_indx, weight, na.rm) {
    
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
      mean_indx <- as.vector(mean_indx[!names(mean_indx) %in% c("selec", "channel", song_colm, "start", "end", "top.freq", "bottom.freq", min_indx, max_indx)])
      mean_indx <- mean_indx[!is.na(mean_indx)]
      }
    
    for(u in mean_indx) Z[ , u] <- stats::weighted.mean(x = as.vector(c(Y[, u, drop = TRUE])), w = wts, na.rm = na.rm)
    
    if (sd){
      W <- Z
      
      for(u in mean_indx) W[ , u] <- stats::sd(x = as.vector(c(Y[, u, drop = TRUE])), na.rm = na.rm)
     
      names(W) <- paste0("sd.", names(W))
       }
    
    # minimums
    if (!is.null(min_indx)) for(u in min_indx)  Z[ ,u]  <- min(x = as.vector(c(Y[, u, drop = TRUE])), na.rm = na.rm)
    
    # maximums
    if (!is.null(max_indx)) for(u in max_indx) Z[ ,u]  <- max(x = as.vector(c(Y[, u, drop = TRUE])), na.rm = na.rm)
    
    indx <- c(mean_indx, min_indx, max_indx)
    
    Z <- Z[, c(names(Z) %in% c("sound.files", song_colm, names(Z)[indx]))]
    Z <- Z[, match(c("sound.files", song_colm, names(Y)[indx]), names(Z))]
    
    if (sd)
    Z <- cbind(Z, W[, mean_indx, drop = FALSE])
      
    Z$num.elemts <- nrow(Y)
    Z$start <- min(Y$start)
    Z$end <- max(Y$end)
    try(Z$bottom.freq <- min(Y$bottom.freq, na.rm = na.rm), silent = TRUE)
    try(Z$top.freq <- max(Y$top.freq, na.rm = na.rm), silent = TRUE)
    try(Z$freq.range <- Z$top.freq - Z$bottom.freq, silent = TRUE)
    Z$duration <- Z$end - Z$start
    Z$note.rate <- Z$num.elemts/Z$duration
    
    return(Z)  
  }
  
  X$.....SONGX... <- paste(X$sound.files, X[, song_colm]) 
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  out <- pbapply::pblapply(unique(X$.....SONGX...), cl = cl, function(i) 
  { 
    Y <- X[X$.....SONGX... == i, , drop = FALSE]
    Y <- as.data.frame(Y)
    return(songparam.FUN(Y, song_colm, mean_indx, min_indx, max_indx, weight, na.rm)) 
  }) 

  df <- do.call(rbind, out)
  df <- as.data.frame(df)
  
  return(df)
}
