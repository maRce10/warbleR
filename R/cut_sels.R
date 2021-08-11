#' Cut selections into individual sound files
#' 
#' \code{cut_sels} cuts selections from a selection table into individual sound files.
#' @export cut_sels
#' @usage cut_sels(X, mar = 0.05, parallel = 1, path = NULL, dest.path = NULL, pb = TRUE,
#' labels = c("sound.files", "selec"), overwrite = FALSE, norm = FALSE, ...)
#' @param X object of class 'selection_table', 'extended_selection_table' or data frame containing columns for sound file name (sound.files), 
#' selection number (selec), and start and end time of signals (start and end).
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the start and end points of selections,
#' delineating spectrogram limits. Default is 0.05.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param dest.path Character string containing the directory path where the cut sound files will be saved.
#' If \code{NULL} (default) then the directory containing the sound files will be used instead.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param labels String vector. Provides the column names that will be used as labels to
#'  create sound file names. Note that they should provide unique names (otherwise 
#'  sound files will be overwritten). Default is \code{c("sound.files", "selec")}. 
#' @param overwrite Logical. If \code{TRUE} sound files with the same name will be 
#' overwritten. Default is \code{FALSE}.
#' @param norm Logical indicating whether wave objects must be normalized first using the function \code{\link[tuneR]{normalize}}. Additional arguments can be passed to \code{\link[tuneR]{normalize}} using `...`.` Default is \code{FALSE}. See \code{\link[tuneR]{normalize}} for available options.
#' @param ... Additional arguments to be passed to the internal \code{\link[tuneR]{normalize}} function for customizing sound file output. Ignored if  \code{norm = FALSE}. 
#' @return Sound files of the signals listed in the input data frame.
#' @family selection manipulation
#' @seealso \code{\link{seltailor}} for tailoring selections 
#'  \href{https://marce10.github.io/2017/06/06/Individual_sound_files_for_each_selection.html}{blog post on cutting sound files}
#' @name cut_sels
#' @details This function allow users to produce individual sound files from the selections
#' listed in a selection table as in \code{\link{lbh_selec_table}}. Note that wave objects with a bit depth of 32 might not be readable by some programs after exporting. In this case they should be "normalized" (argument 'norm") with a lower bit depth.  
#' @examples{ 
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#' 
#' # cut selections
#' cut_sels(lbh_selec_table, path = tempdir())
#'  
#' #check this folder!!
#' tempdir()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) and Grace Smith Vidaurre
#last modification on mar-12-2018 (MAS)

cut_sels <- function(X, mar = 0.05, parallel = 1, path = NULL, dest.path = NULL, pb = TRUE,
                     labels = c("sound.files", "selec"), overwrite = FALSE, norm = FALSE, ...){
  
  # reset pb
  
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(cut_sels)
  
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
  
  #check path to destiny directory
  if (!is.null(dest.path))
  {if (!dir.exists(dest.path)) stop("'dest.path' provided does not exist") else
    dest.path <- normalizePath(dest.path)
    } else dest.path <- path
   
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  if (!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop("'start' and 'end' must be numeric")
  
  #if any start higher than end stop
  if (any(X$end - X$start <= 0)) stop(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))  
  
  #missing label columns
  if (!all(labels %in% colnames(X)))
    stop(paste(paste(labels[!(labels %in% colnames(X))], collapse=", "), "label column(s) not found in data frame"))
  
  if (!is_extended_selection_table(X))
  {
    #return warning if not all sound files were found
  recs.wd <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)
  if (length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files))) 
    (paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])), 
           "sound file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% recs.wd) 
  if (length(d) == 0){
    stop("The sound files are not in the working directory")
  }  else {
    X <- X[d, ]
  }
}  else X.orig <- X
  
  #convert factors to characters
  X[,sapply(X, is.factor)] <- apply(X[,sapply(X, is.factor), drop = FALSE], 2, as.character)
  
  #remove .wav from sound file names
  X2 <- X
  X2$sound.files <- gsub("\\.wav$|\\.wac$|\\.mp3$|\\.flac$", "", X2$sound.files, ignore.case = TRUE)
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")

  #create function to run within Xapply functions downstream     
  cutFUN <- function(X, i, mar, labels, dest.path){
    
    # Read sound files, initialize frequency and time limits for spectrogram
    r <- warbleR::read_sound_file(X = X, index = i, header = TRUE, path = path)
    f <- r$sample.rate
    t <- c(X$start[i] - mar, X$end[i] + mar)
    
    # fix margins if below 0 or length of recordings
    mar1 <- mar
    mar2 <- mar1 + X$end[i] - X$start[i]
    
    if (t[1] < 0)  t[1] <- 0
    if (t[2] > r$samples/f) t[2] <- r$samples/f
    
    # Cut wave
    wvcut <- warbleR::read_sound_file(X = X, path = path, index = i, from = t[1], to = t[2])

    # save cut
    if (overwrite) unlink(file.path(dest.path, paste0(paste(X2[i, labels], collapse = "-"), ".wav")))

  if (norm)  wvcut <- normalize(object = wvcut, ...)
    
    suppressWarnings(tuneR::writeWave(extensible = FALSE, object = wvcut, filename = file.path(dest.path, paste0(paste(X2[i, labels], collapse = "-"), ".wav"))))
       
  }
  
  
  
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  out <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(X), cl = cl, FUN = function(i) 
  { 
    cutFUN(X = X, i = i, mar = mar, labels = labels, dest.path = dest.path)
  }) 
}


##############################################################################################################
#' alternative name for \code{\link{cut_sels}}
#'
#' @keywords internal
#' @details see \code{\link{cut_sels}} for documentation. \code{\link{cut_sels}} will be deprecated in future versions.
#' @export

cut_sels <- cut_sels
