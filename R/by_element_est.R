#' Convert a by-song extended selection table to by-element
#' 
#' \code{by_element_est} converts a by-song extended selection table to by-element.
#' @usage by_element_est(X, mar = 0.1, pb = FALSE, parallel = 1)
#' @param X object of class 'extended_selection_table' (see \code{\link{selection_table}}).
#' @param mar Numeric vector of length 1 specifying the margins (in seconds) 
#' adjacent to the start and end points of the selections when creating the ''by element' extended 
#' selection table. Default is 0.1.
#' @param pb Logical argument to control progress bar. Default is \code{FALSE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @return  A 'by element' extended selection table (see \code{\link{selection_table}}). 
#' @export
#' @name by_element_est
#' @details This function converts extended selection tables in 'by song' format (several selection per wave object) to a 'by element' format (one wave object per selection).
#' @examples
#' \dontrun{
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav")) 
#' 
#' # create extended selection table
#' by_song_est <- selection_table(lbh_selec_table, path = tempdir(),
#'  extended = TRUE, by.song = "song", confirm.extended = FALSE)
#'  
#'  # conver o by element
#'  by_element_est <- by_element_est(by_song_est, mar = 0.05)
#' }
#' @family extended selection table manipulation
#' @seealso \code{\link{mp32wav}}, \code{\link{fix_wavs}}
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#' #last modification on nov-9-2022 (MAS)

by_element_est <- function(X, mar = 0.1, pb = FALSE, parallel = 1)
{
  
  #if X is not a data frame
  if (!is_extended_selection_table(X)) stop2("X is not of class 'extended_selection_table'")
  
  # if extended only by song
    if (!attributes(X)$by.song$by.song) stop2("extended selection tables must be created 'by.song' to be used in song.param()")
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(by_element_est)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
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
  
  # set clusters for windows OS and no soz
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # extract single wave object per row
  wavs <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(X), FUN = function(x){
    
    wav <- read_sound_file(X = X, index = x, from = X$start[x] - mar, to = X$end[x] + mar)
    
    return(wav)
  })
  
  # fix check results
  check_res_list <- lapply(1:nrow(X), function(x){
    
    check_res <- attributes(X)$check.res[attributes(X)$check.res$sound.files == X$sound.files[x] & attributes(X)$check.res$selec == X$selec[x], ]
    
    check_res$start <- if(check_res$mar.before < mar) check_res$mar.before else mar
    check_res$end <- check_res$start + check_res$duration
    check_res$mar.before <- check_res$start
    check_res$mar.after <- duration(wavs[[x]]) - check_res$end
    check_res$sound.files <- paste(X$sound.files[x], X$selec[x], sep = "-")
    check_res$selec <- 1
    return(check_res)
  })

  # fix attributes
  attributes(X)$wave.objects <- wavs
  names(attributes(X)$wave.objects)  <- X$sound.files <- paste(X$sound.files, X$selec, sep = "-")
  attributes(X)$check.results <- do.call(rbind, check_res_list)
  attributes(X)$by.song <- c(by.song = FALSE, song.column = NULL)
  attributes(X)$call <- base::match.call()
  attributes(X)$warbleR.version <- packageVersion("warbleR")
  
  #fix selec, start and end in X
  X$selec <- 1
  X$start <- attributes(X)$check.results$start
  X$end <- attributes(X)$check.results$end
  
  return(X)
}