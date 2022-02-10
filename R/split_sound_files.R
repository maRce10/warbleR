#' Splits sound files
#' 
#' \code{split_sound_files} splits sound files in shorter segments
#' @usage split_sound_files(path = NULL, sgmt.dur = 10, sgmts = NULL, files = NULL,
#'  parallel = 1, pb = TRUE, only.sels = FALSE, X = NULL)
#' @param path Directory path where sound files are found. 
#'  If \code{NULL} (default) then the current working directory is used.
#' @param sgmt.dur Numeric. Duration (in s) of segments in which sound files would be split. Sound files shorter than 'sgmt.dur' won't be split. Ignored if 'sgmts' is supplied.
#' @param sgmts Numeric. Number of segments in which to split each sound file. If supplied 'sgmt.dur' is ignored.
#' @param files Character vector indicating the subset of files that will be split.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Only used when 
#' @param only.sels Logical argument to control if only the data frame is returned (no wave files are saved). Default is \code{FALSE}.
#' @param X 'selection_table' object or a data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). If supplied the data frame/selection table is modified to reflect the position of the selections in the new sound files. Note that some selections could split between 2 segments. To deal with this, a 'split.sels' column is added to the data frame in which those selection are labeled as 'split'. Default is \code{NULL}.
#' @family data manipulation
#' @seealso \code{\link{cut_sels}} 
#' @export
#' @name split_sound_files
#' @return Wave files for each segment in the working directory (if \code{only.sels = FALSE}, named as 'sound.file.name-#.wav') and a data frame in the R environment containing the name of the original sound files (org.sound.files), the name of the clips (sound.files) and the start and end of clips in the original files. Clips are saved in .wav format. If 'X' is supplied then a data frame with the position of the selections in the newly created clips is returned instead.
#' @details This function aims to reduce the size of sound files in order to simplify some processes that are limited by sound file size (big files can be manipulated, e.g. \code{\link{auto_detec}}). The function keeps the original number of channels in the output clips only for 1- and 2-channel files.
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
  
  # measure wav duration
  wvdr <- duration_wavs(path = path, files = files)

  # calculate start and end of segments and output data frame
  split.df_l <- lapply(files, function(x){

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
  split.df <- do.call(rbind, split.df_l)

  # if no sound files are produced
  if (!only.sels){

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
    # split using a loop only those shorter than segments
    a_l <- pblapply_wrblr_int(pbar = pb, X = which(split.df$org.sound.files != split.df$sound.files), cl =  cl, FUN = function(x) {
  
  # read clip    
  clip <- warbleR::read_sound_file(X = split.df$org.sound.files[x], from = split.df$start[x], to = split.df$end[x], path = path, channel = 1)
  
  
  # add second channel if stereo
  if (warbleR::read_sound_file(X = split.df$org.sound.files[x], path = path, header = TRUE)$channels > 1){
    clip_ch2 <- warbleR::read_sound_file(X = split.df$org.sound.files[x], from = split.df$start[x], to = split.df$end[x], path = path, channel = 2)
    clip <- Wave(left = clip@left, right = clip_ch2@left, samp.rate = clip@samp.rate, bit = clip@bit)
  }
  
  
  # save   
  tuneR::writeWave(extensible = FALSE, object = clip, filename = file.path(path, split.df$sound.files[x]))
    
    return(NULL)  
  })
  
  # make it a vector
  a <- unlist(a_l)
  }
  
  # calculate position of selection in newly created clips
  if (!is.null(X)) {
    
    ## cbind new file data and X to get overlapping sels 
    # make analogous columns on both data frames
    split.df$new.sound.files <- split.df$sound.files
    split.df$sound.files <- split.df$org.sound.files
    split.df$bottom.freq <- split.df$top.freq <- NA
    X$new.sound.files <- NA
    
    # ad unique selec ID to new files
    split.df$selec <- paste0("new", 1:nrow(split.df))
    
    # select columns to bind
    clms <- if (!is.null(X$bottom.freq) & !is.null(X$top.freq)) c("sound.files", "new.sound.files", "selec", "start", "end", "bottom.freq", "top.freq") else c("sound.files", "new.sound.files", "selec", "start", "end")
    
    # bind together
    ovlp.df <- rbind(X[, clms], split.df[, clms])
    
    # add unique id for each selection
    ovlp.df$sel.id <- paste(ovlp.df$sound.files, ovlp.df$selec, sep = "-")
    X$sel.id <- paste(X$sound.files, X$selec, sep = "-")
    
    # get which selection are found in which new files
    ovlp.df <- warbleR::overlapping_sels(ovlp.df, indx.row = TRUE, max.ovlp = 0.0000001, pb = pb, parallel = parallel, verbose = FALSE)
    
    ovlp.df$..row <- 1:nrow(ovlp.df)
    
    # split in new files rows and selection rows
    new.sf.df <- ovlp.df[!is.na(ovlp.df$new.sound.files) & !is.na(ovlp.df$ovlp.sels), ]
    org.sls.df <- ovlp.df[is.na(ovlp.df$new.sound.files), ]
    
    # re-add other columns
    X$org.sound.files <- X$sound.files
    org.sls.df <- merge(org.sls.df, X[, setdiff(names(X), clms)], by = "sel.id")
    
    # order columns
    org.sls.df <- sort_colms(org.sls.df)
    
    # find time positions in new files 
    new.sels_l <- lapply(1:nrow(new.sf.df), function(x){
      
      Y <- new.sf.df[x, , drop = FALSE]
      
      # get those selection found within Y
      contained.sls <- org.sls.df[org.sls.df$..row %in% strsplit(Y$indx.row, "/")[[1]], ]
      
      # if selection were found within Y
      if (nrow(contained.sls) > 0)
{      contained.sls$sound.files <- Y$new.sound.files
      
      # get new start and end
      contained.sls$start <- contained.sls$start - Y$start
      contained.sls$end <- contained.sls$end - Y$start
      contained.sls$start[contained.sls$start < 0] <- 0
      contained.sls$end[contained.sls$end > Y$end] <- Y$end
      
      return(contained.sls)
      } else
        return(NULL)
    })
    
    new.sels <- do.call(rbind, new.sels_l)
    new.sels$..row <- new.sels$indx.row <- new.sels$ovlp.sels <- new.sels$new.sound.files <- NULL
    
    # find which selection were split in 2 or more new files
    new.sels$split.sels <- NA
    cnt.sels <- table(new.sels$sel.id) 
    new.sels$split.sels[new.sels$sel.id %in% names(cnt.sels[cnt.sels > 1])] <- "split"
    new.sels$sel.id <- NULL
    row.names(new.sels) <- 1:nrow(new.sels)
    
  return(new.sels)  
  } else
  return(split.df)
} 


##############################################################################################################
#' alternative name for \code{\link{split_sound_files}}
#'
#' @keywords internal
#' @details see \code{\link{split_sound_files}} for documentation. \code{\link{split_wavs}} will be deprecated in future versions.
#' @export

split_wavs <- split_sound_files
