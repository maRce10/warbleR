#' Splits sound files
#' 
#' \code{split_sound_files} splits sound files in shorter segments
#' @usage split_sound_files(path = NULL, sgmt.dur = 10, sgmts = NULL, files = NULL,
#'  parallel = 1, pb = TRUE, only.sels  = FALSE, X = NULL)
#' @param path Directory path where sound files are found. 
#'  If \code{NULL} (default) then the current working directory is used.
#' @param sgmt.dur Numeric. Duration (in s) of segments in which sound files would be split. Sound files shorter than 'sgmt.dur' won't be split. Ignored if 'sgmts' is supplied.
#' @param sgmts Numeric. Number of segments in which to split each sound file. If supplied 'sgmt.dur' is ignored.
#' @param files Character vector indicating the subset of files that will be split.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param only.sels Logical argument to control if only the data frame is return (no wave files are saved). Default is \code{FALSE}.
#' @param X 'selection_table' object or a data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). If provided selections in 'X' will be saved as individual sound files (and 'sgmt.dur'/'sgmts'/'files' arguments will be ignored. Default is \code{NULL}.
#' @family data manipulation
#' @seealso \code{\link{cut_sels}} 
#' @export
#' @name split_sound_files
#' @return Wave files for each segment in the working directory (named as 'sound.file.name-#.wav') and a data frame in the R environment containing the name of the original sound files (org.sound.files), the name of the cuts (sound.files) and the start and end of cuts in the original files. Cuts are saved in .wav format.
#' @details This function aims to reduce the size of sound files in order to simplify some processes that are limited by sound file size (big files can be manipulated, e.g. \code{\link{auto_detec}} ).
#' @examples
#' {
#' #load data and save to temporary working directory
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' 
#' #split files in 1 s files
#' split_sound_files(sgmt.dur = 1, path = tempdir())
#' 
#' # Check this folder
#' tempdir()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on mar-21-2020 (MAS)
split_sound_files <- function(path = NULL, sgmt.dur = 10, sgmts = NULL, files = NULL, parallel = 1, pb = TRUE, only.sels  = FALSE, X = NULL){
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(split_sound_files)
  
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
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) 
      stop("'path' provided does not exist") else
        path <- normalizePath(path)
  
  #stop if files is not a character vector
  if (!is.null(files) & !is.character(files)) stop("'files' must be a character vector")
  
  if (is.null(files))
    files <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE) #list .wav files in working director    
  
  #stop if no wav files are found
  if (length(files) == 0) stop("no sound files in working directory") 
  
  if (!is.null(X)){
    
    #if X is not a data frame
    if (!any(is.data.frame(X), is_selection_table(X))) stop("X is not of a class 'data.frame' or 'selection_table'")
    
    if (is_extended_selection_table(X)) stop("This function cannot take extended selection tables ('X' argument)")
    
    #check if all columns are found
    if (any(!(c("sound.files", "selec", "start", "end") %in% colnames(X)))) 
      stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                     "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
    
    #if there are NAs in start or end stop
    if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end columns")  
    
    #if end or start are not numeric stop
    if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop("'start' and 'end' must be numeric")
    
    #if any start higher than end stop
    if (any(X$end - X$start <= 0)) stop(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)")) 
  }
  
  
  # check sgmnt duration
  if (is.null(sgmts))
  {
    if (!is.numeric(sgmt.dur)) stop("'sgmt.dur' must be numeric")
    } else 
    if (!is.numeric(sgmts)) stop("'sgmts' must be numeric")
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # if X has not
  if (is.null(X)){    
  # measure wav duration
  wvdr <- duration_wavs(path = path, files = files)

  # calculate start and end of segments and output data frame
  split.dfs <- lapply(files, function(x){

    # calculate segment limits
    if (is.null(sgmts)){
      # if sgmnts duration is shorter than file
      if (sgmt.dur < wvdr$duration[wvdr$sound.files == x])
      {
        # get start and end of segments
        sq <- seq(from = 0, to = wvdr$duration[wvdr$sound.files == x], by = sgmt.dur)
    
    # add end if last sq != duration
    if (sq[length(sq)] != wvdr$duration[wvdr$sound.files == x])
      sq <- c(sq, wvdr$duration[wvdr$sound.files == x])
    
    out <- data.frame(org.sound.files = x, sound.files = paste0(gsub("\\.wav$|\\.wac$|\\.mp3$|\\.flac$", "", x, ignore.case = TRUE), "-", 1:(length(sq) - 1), ".wav"), start = sq[-length(sq)], end = sq[-1], stringsAsFactors = FALSE)
      } else # if segment duration is longer or equal
        out <- data.frame(org.sound.files = x, sound.files = x, start = 0, end = wvdr$duration[wvdr$sound.files == x], stringsAsFactors = FALSE)
    } else { 
      # get start and end of segments
      sq <- seq(from = 0, to = wvdr$duration[wvdr$sound.files == x], length.out = sgmts + 1)
      
      # put in data frame
      out <- data.frame(org.sound.files = x, sound.files = paste0(gsub("\\.wav$|\\.wac$|\\.mp3$|\\.flac$", "", x, ignore.case = TRUE), "-", 1:(length(sq) - 1), ".wav"), start = sq[-length(sq)], end = sq[-1], stringsAsFactors = FALSE)
      }
    
    return(out)
  })
  
  # put together in a single data frame
  split.df <- do.call(rbind, split.dfs)
  } else {
    
    split.df <- X
    split.df$org.sound.files <- X$sound.files
     
    split.df$sound.files <- paste0(gsub("\\.wav$|\\.wac$|\\.mp3$|\\.flac$", "", X$sound.files, ignore.case = TRUE), "-", X$selec, ".wav")
  }
  
  # if no sound files are produced
  if (!only.sels){
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
    # split using a loop only the ones that are shorter than segments
  a <- pbapply::pbsapply(which(split.df$org.sound.files != split.df$sound.files), cl =  cl, function(x) {
  
  # read clip    
  clip <- warbleR::read_sound_file(X = split.df$org.sound.files[x], from = split.df$start[x], to = split.df$end[x], path = path)
  
  # save   
  tuneR::writeWave(extensible = FALSE, object = clip, filename = file.path(path, split.df$sound.files[x]))
    
    return(NULL)  
  })
  }
  
  return(split.df)
} 


##############################################################################################################
#' alternative name for \code{\link{split_sound_files}}
#'
#' @keywords internal
#' @details see \code{\link{split_sound_files}} for documentation. \code{\link{split_wavs}} will be deprecated in future versions.
#' @export

split_wavs <- split_sound_files