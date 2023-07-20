#' Create 'selection_table' and 'extended_selection_table' objects
#'
#' \code{selection_table} converts data frames into an object of classes 'selection_table' or 'extended_selection_table'.
#' @usage selection_table(X, max.dur = 10, path = NULL, whole.recs = FALSE,
#' extended = FALSE, confirm.extended = FALSE, mar = 0.1, by.song = NULL,
#' pb = TRUE, parallel = 1, verbose = TRUE, skip.error = FALSE,
#' file.format = "\\\.wav$|\\\.wac$|\\\.mp3$|\\\.flac$", ...)
#' @param X data frame with the following columns: 1) "sound.files": name of the .wav
#' files, 2) "selec": unique selection identifier (within a sound file), 3) "start": start time and 4) "end":
#' end time of selections. Columns for 'top.freq', 'bottom.freq' and 'channel' are optional. Note that, when 'channel' is
#' not provided the first channel (i.e. left channel) would be used by default.
#' Frequency parameters (including top and bottom frequency) should be provided in kHz. Alternatively, a 'selection_table' class object can be input.
#' @param max.dur the maximum duration of expected for a selection  (ie. end - start). If surpassed then an error message
#' will be generated. Useful for detecting errors in selection tables.
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param whole.recs Logical. If \code{TRUE} the function will create a selection
#' table for all sound files in the working directory (or "path") with `start = 0`
#' and `end = duration_wavs()`. Default is if \code{FALSE}. Note that this will not create
#' a extended selection table. If provided 'X' is ignored.
#' @param extended Logical. If \code{TRUE}, the function will create an object of class 'extended_selection_table'
#' which included the wave objects of the selections as an additional attribute ('wave.objects') to the data set. This is
#' and self-contained format that does not require the original sound files for running most acoustic analysis in
#' \code{\link{warbleR}}. This can largely facilitate the storing and sharing of (bio)acoustic data. Default
#' is if \code{FALSE}. An extended selection table won't be created if there is any issue with the selection. See
#' 'details'.
#' @param mar Numeric vector of length 1 specifying the margins (in seconds)
#' adjacent to the start and end points of the selections when creating extended
#' selection tables. Default is 0.1. Ignored if 'extended' is \code{FALSE}.
#' @param confirm.extended Logical. If \code{TRUE} then the size of the 'extended_selection_table'
#' will be estimated and the user will be asked for confirmation (in the console)
#' before proceeding. Ignored if 'extended' is \code{FALSE}. This is used to prevent
#' generating objects too big to be dealt with by R. See 'details' for more information about extended selection table size. THIS ARGUMENT WILL BE DEPRECATED IN FUTURE VERSIONS.
#' @param by.song Character string with the column name containing song labels. If provided a wave object containing for
#' all selection belonging to a single song would be saved in the extended selection table (hence only applicable for
#' extended selection tables). Note that the function assumes that song labels are not repeated within a sound file.
#' If \code{NULL} (default), wave objects are created for each selection (e.g. by selection).
#' Ignored if \code{extended = FALSE}.
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param verbose Logical argument to control if summary messages are printed to the console. Default is \code{TRUE}.
#' @param skip.error Logical to control if errors are omitted. If so, files that could not be read will be excluded and their name printed in the console. Default is \code{FALSE}, which will return an error if some files are problematic.
#' @param file.format Character string with the format of sound files. By default all sound file formats supported by warbleR are included ("\\.wav$|\\.wac$|\\.mp3$|\\.flac$"). Note that several formats can be included using regular expression syntax as in \code{\link[base]{grep}}. For instance \code{"\\.wav$|\\.mp3$"} will only include .wav and .mp3 files. Ignored if \code{whole.recs = FALSE}.
#' @param ... Additional arguments to be passed to \code{\link{check_sels}} for customizing
#' checking routine.
#' @return An object of class selection_table which includes the original data frame plus the following additional attributes:
#' \itemize{
#'    \item 1) A data frame with the output of \code{\link{check_sels}} run on the input data frame. If a extended selection table is created it will also include the original values in the input data frame for each selections. This are used by downstream warbleR functions to improve efficiency and avoid
#' errors due to missing or mislabeled data, or selection out of the ranges of the original sound files.
#'    \item 2) A list indicating if the selection table has been created by song (see 'by.song argument).
#'    \item 3) If a extended selection table is created a list containing the wave objects for each selection (or song if 'by.song').
#'    }
#' @details This function creates and object of class 'selection_table' or 'extended_selection_table' (if \code{extended = TRUE}, see below). First, the function checks:
#' \itemize{
#'    \item 1) if the selections listed in the data frame correspond to .wav files
#' in the working directory
#'    \item 2) if the sound files can be read and if so,
#'    \item 3) if the start and end time of the selections are found within the duration of the sound files
#'    }
#' If no errors are found the a selection table or extended selection table will be generated.
#' Note that the sound files should be in the working directory (or the directory provided in 'path').
#' This is useful for avoiding errors in downstream functions (e.g. \code{\link{spectro_analysis}}, \code{\link{cross_correlation}},  \code{\link{catalog}}, \code{\link{freq_DTW}}). Note also that corrupt files can be
#' fixed using \code{\link{fix_wavs}} ('sox' must be installed to be able to run this function).
#' The 'selection_table' class can be input in subsequent functions.
#'
#' When \code{extended = TRUE} the function will generate an object of class 'extended_selection_table' which
#' will also contains the wave objects for each of the selections in the data frame.
#' This transforms selection tables into self-contained objects as they no longer need the original
#' sound files to run acoustic analysis. This can largely facilitate the storing and sharing of (bio)acoustic data.
#' Extended selection table size will be a function of the number of selections \code{nrow(X)}, sampling rate, selection
#' duration and margin duration. As a guide, a selection table
#' with 1000 selections similar to the ones in 'lbh_selec_table' (mean duration ~0.15
#'  seconds) at 22.5 kHz sampling rate and the default margin (\code{mar = 0.1})
#'  will generate a extended selection table of ~31 MB (~310 MB for a 10000 rows
#'  selection table). You can check the size of the output extended selection table
#'  with the \code{\link[utils]{object.size}} function. Note that extended selection table created 'by.song' could be
#'  considerable larger.
#' @seealso \code{\link{check_wavs}}, \href{https://marce10.github.io/2018/05/15/Extended_selection_tables.html}{blog post on extended selection tables}
#' @export
#' @name selection_table
#' @examples
#' {
#'   data(list = c(
#'     "Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4",
#'     "lbh_selec_table"
#'   ))
#'   writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#'   writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#'   writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#'   writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#'   # make selection table
#'   st <- selection_table(X = lbh_selec_table, path = tempdir())
#'
#'   is_selection_table(st)
#'
#'   #' # make extended selection table
#'   st <- selection_table(
#'     X = lbh_selec_table, extended = TRUE,
#'     path = tempdir()
#'   )
#'
#'   is_extended_selection_table(st)
#'
#'   ### make extended selection by song
#'   # create a song variable
#'   lbh_selec_table$song <- as.numeric(as.factor(lbh_selec_table$sound.files))
#'
#'   st <- selection_table(
#'     X = lbh_selec_table, extended = TRUE,
#'     by.song = "song", path = tempdir()
#'   )
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on may-9-2018 (MAS)

selection_table <- function(X, max.dur = 10, path = NULL, whole.recs = FALSE,
                            extended = FALSE, confirm.extended = FALSE, mar = 0.1, by.song = NULL, pb = TRUE, parallel = 1, verbose = TRUE, skip.error = FALSE, file.format = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ...) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(selection_table)
  
  # get warbleR options
  opt.argms <- if (!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]
  
  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]
  
  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]
  
  # set options left
  if (length(opt.argms) > 0) {
    for (q in seq_len(length(opt.argms))) {
      assign(names(opt.argms)[q], opt.argms[[q]])
    }
  }
  
  # check path if not provided set to working directory
  if (is.null(path)) path <- getwd() else if (!dir.exists(path)) stop2("'path' provided does not exist")
  
  # if by song but column not found
  if (!is.null(by.song)) {
    if (!any(names(X) == by.song)) stop2("'by.song' column not found")
  }
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop2("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop2("'parallel' should be a positive integer")
  
  # create a selection table for a row for each full length recording
  if (whole.recs) {
    sound.files <- list.files(path = path, pattern = file.format, ignore.case = TRUE)
    
    if (length(sound.files) == 0) stop2("No sound files were found")
    
    X <- data.frame(sound.files, selec = 1, channel = 1, start = 0, end = duration_sound_files(files = sound.files, path = path, skip.error = skip.error)$duration)
    
    if (skip.error) {
      # get name of problematic files
      error_files <- X$sound.files[is.na(X$end)]
      
      # remove them from output X
      X <- X[!is.na(X$end), ]
    }
  }
  
  # create error_files if not created
  if (!exists("error_files")) {
    error_files <- vector()
  }
  
  if (pb & verbose) {
    if (!extended) 
      message2(x = "checking selections (step 1 of 1):") else
        
        message2(x = "checking selections (step 1 of 2):")
  }
  
  check.results <- warbleR::check_sels(X, path = path, wav.size = TRUE, pb = pb, verbose = FALSE, ...)
  
  if (any(check.results$check.res != "OK")) stop2("Not all selections can be read (use check_sels() to locate problematic selections)")
  
  X <- check.results[, names(check.results) %in% names(X)]
  
  ## Set the name for the class
  class(X) <- unique(append("selection_table", class(X)))
  
  check.results <- check.results[, names(check.results) %in% c("sound.files", "selec", by.song, "check.res", "duration", "min.n.sample", "sample.rate", "channels", "bits", "wav.size", "sound.file.samples")]
  
  # add wave object to extended file
  if (extended) {
    if (confirm.extended) {
      exp.size <- sum(round(check.results$bits * check.results$sample.rate * (check.results$duration + (mar * 2)) / 4) / 1024)
      
      message2(x = paste0("Expected 'extended_selection_table' size is ~", ifelse(round(exp.size) == 0, round(exp.size, 2), round(exp.size)), "MB (~", round(exp.size / 1024, 5), " GB) \n Do you want to proceed? (y/n): "), color = "magenta")
      answer <- readline(prompt = "")
    } else {
      answer <- "yeah dude!"
    }
    
    if (substr(answer, 1, 1) %in% c("y", "Y")) # if yes
    {
      check.results$orig.start <- X$start
      check.results$orig.end <- X$end
      
      check.results$mar.after <- check.results$mar.before <- rep(mar, nrow(X))
      
      # get sound file duration
      dur <- duration_wavs(files = as.character(X$sound.files), path = path)$duration
      
      # reset margin signals if lower than 0 or higher than duration
      for (i in 1:nrow(X))
      {
        if (X$start[i] < mar) check.results$mar.before[i] <- X$start[i]
        if (X$end[i] + mar > dur[i]) check.results$mar.after[i] <- dur[i] - X$end[i]
      }
      
      if (!is.null(by.song)) {
        Y <- song_analysis(X = as.data.frame(X), song_colm = by.song, pb = FALSE)
        Y <- Y[, names(Y) %in% c("sound.files", by.song, "start", "end")]
        
        check.results$song <- X[, by.song]
        
        # temporal column to match songs by sound file
        check.results$song.TEMP <- paste(X$sound.files, X[, by.song, drop = TRUE], sep = "-")
        Y$song.TEMP <- paste(Y$sound.files, Y[, by.song], sep = "-")
        
        Y$mar.before <- sapply(unique(Y$song.TEMP), function(x) check.results$mar.before[which.min(check.results$orig.start[check.results$song.TEMP == x])])
        
        Y$mar.after <- sapply(unique(Y$song.TEMP), function(x) check.results$mar.after[which.max(check.results$orig.end[check.results$song.TEMP == x])])
        
        # remove temporal column
        check.results$song.TEMP <- NULL
      } else {
        Y <- X
        Y$mar.before <- check.results$mar.before
        Y$mar.after <- check.results$mar.after
      }
      
      # save wave objects as a list attributes
      # set clusters for windows OS
      
      if (Sys.info()[1] == "Windows" & parallel > 1) {
        cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
      } else {
        cl <- parallel
      }
      
      if (pb) {
        message2(x = "saving wave objects into extended selection table (step 2 of 2):")
      }
      
      attributes(X)$wave.objects <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(Y), cl = cl, FUN = function(x) warbleR::read_sound_file(X = Y, index = x, from = Y$start[x] - Y$mar.before[x], to = Y$end[x] + Y$mar.after[x], path = path, channel = if (!is.null(X$channel)) X$channel[x] else 1))
      
      # reset for new dimensions
      check.results$start <- X$start <- check.results$mar.before
      check.results$end <- X$end <- check.results$mar.before + check.results$duration
      
      names(check.results)[names(check.results) == "sound.files"] <- "orig.sound.files"
      names(check.results)[names(check.results) == "selec"] <- "orig.selec"
      
      if (!is.null(by.song)) {
        names(attributes(X)$wave.objects) <- paste0(Y$sound.files, "-song_", Y[, by.song])
        X$sound.files <- check.results$sound.files <- paste0(X$sound.files, "-song_", as.data.frame(X)[, names(X) == by.song, ])
        
        for (i in unique(X$sound.files)) {
          check.results$selec[check.results$sound.files == i] <- X$selec[X$sound.files == i] <- 1:nrow(X[X$sound.files == i, drop = FALSE])
        }
        
        check.results$n.samples <- NA
        
        durs <- X$end - X$start
        
        for (w in unique(X$sound.files))
        {
          check.results$start[check.results$sound.files == w] <- X$start[X$sound.files == w] <- X$start[X$sound.files == w][which.min(check.results$orig.start[check.results$sound.files == w])] + (check.results$orig.start[check.results$sound.files == w] - min(check.results$orig.start[check.results$sound.files == w]))
          
          # add n.samples for header info
          check.results$n.samples[check.results$sound.files == w] <- length(attr(X, "wave.objects")[[which(names(attr(X, "wave.objects")) == w)]]@left)
        }
        check.results$end <- X$end <- X$start + durs
      } else {
        names(attributes(X)$wave.objects) <- check.results$sound.files <- X$sound.files <- paste(basename(as.character(X$sound.files)), X$selec, sep = "_")
        check.results$selec <- X$selec <- 1
        
        check.results$n.samples <- as.vector(sapply(attr(X, "wave.objects"), function(x) length(x@left))) # add n.samples for header info
      }
      
      ## Set the name for the class
      class(X)[class(X) == "selection_table"] <- "extended_selection_table"
    }
  } else {
    check.results$n.samples <- check.results$sound.file.samples
  }
  
  # order check results columns
  check.results <- check.results[, na.omit(match(c("orig.sound.files", "orig.selec", "orig.start", "orig.end", "sound.files", "selec", "start", "end", "check.results", "duration", "sample.rate", "channels", "bits", "wav.size", "mar.before", "mar.after", "n.samples"), names(check.results)))]
  
  attributes(X)$check.results <- check.results
  
  # recalculate file size
  if (whole.recs) {
    attributes(X)$check.results$wav.size <- file.size(file.path(path, attributes(X)$check.results$sound.files)) / 1000000
  }
  
  if (is_extended_selection_table(X) & !is.null(by.song)) attributes(X)$by.song <- list(by.song = TRUE, song.column = by.song) else attributes(X)$by.song <- list(by.song = FALSE, song.column = by.song)
  
  attributes(X)$call <- base::match.call()
  
  attributes(X)$warbleR.version <- packageVersion("warbleR")
  
  if (extended & confirm.extended & !is_extended_selection_table(X))
    message2(color = "silver", x = cli::style_bold("'extended_selection_table' was not created"))
  
  if (skip.error & length(error_files) > 0) 
    message2(color = "silver", x = paste("\nthe following file(s) couldn't be read and were not included:", cli::style_bold(paste(error_files, collapse = ","))))
  
  return(X)
}


##############################################################################################################
#' Old name for \code{\link{selection_table}}
#'
#' @keywords internal
#' @details see \code{\link{selection_table}} for documentation
#' @export

make.selection.table <- selection_table


##############################################################################################################


#' Class 'selection_table': double-checked frequency/time coordinates of selections
#'
#' Class for selections of signals in sound files
#' @export
#' @details An object of class \code{selection_table} created by \code{\link{selection_table}} is a list with the following elements:
#'  \itemize{
#'  \item\code{selections}: data frame containing the frequency/time coordinates of the selections, sound file names, and any  additional information
#'  \item \code{check.resutls}: results of the checks on data consistency using \link{check_sels}
#' }
#' @seealso \code{\link{selection_table}}

##############################################################################################################

#' Check if object is of class "selection_table"
#'
#' \code{is_selection_table} Check if the object belongs to the class "selection_table"
#' @usage is_selection_table(x)
#' @param x R object.
#' @return A logical argument indicating whether the object class is 'selection_table'
#' @seealso \code{\link{selection_table}}
#' @export
#' @name is_selection_table
#' @examples
#' {
#'   # load data
#'   data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#'
#'   is_selection_table(lbh_selec_table)
#'
#'   # save wave files in temporary directory
#'   writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#'   writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#'   writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#'   writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#'   st <- selection_table(lbh_selec_table, path = tempdir())
#'
#'   is_selection_table(st)
#'
#'   class(st)
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on may-9-2018 (MAS)

is_selection_table <- function(x) inherits(x, "selection_table")


##############################################################################################################

#' Class 'extended_selection_table': selection table containing wave objects
#'
#' Class for selections of signals in sound files and corresponding wave objects
#' @export
#' @details An object of class \code{extended_selection_table} created by \code{\link{selection_table}} is a list with the following elements:
#'  \itemize{
#'  \item \code{selections}: data frame containing the frequency/time coordinates of the selections, sound file names, and any  additional information
#'  \item \code{check.resutls}: results of the checks on data consistency using \link{check_sels}
#'  \item \code{wave.objects}: list of wave objects corresponding to each selection
#'  \item \code{by.song}: a list with 1) a logical argument defining if the 'extended_selection_table' was created 'by song'
#'  and 2) the name of the song column (see \code{\link{selection_table}})
#'  }
#' @seealso \code{\link{selection_table}}, \code{\link{selection_table}}

##############################################################################################################


#' Check if object is of class "extended_selection_table"
#'
#' \code{is_extended_selection_table} Check if the object belongs to the class "extended_selection_table"
#' @usage is_extended_selection_table(x)
#' @param x R object
#' @return A logical argument indicating whether the object class is 'extended_selection_table'
#' @seealso \code{\link{selection_table}}; \code{\link{is_selection_table}}
#' @export
#' @name is_extended_selection_table
#' @examples
#' {
#'   data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#'
#'   is_extended_selection_table(lbh_selec_table)
#'
#'   writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#'   writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#'   writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#'   writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#'   st <- selection_table(lbh_selec_table, extended = TRUE, 
#'   path = tempdir())
#'
#'   is_extended_selection_table(st)
#'
#'   class(st)
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on may-9-2018 (MAS)

is_extended_selection_table <- function(x) inherits(x, "extended_selection_table")

##############################################################################################################
#' extract method for class \code{selection_table}
#'
#' @param X Object of class \code{selection_table}, generated by \code{\link{selection_table}}.
#' @param i, j,	indices specifying elements to extract or replace. Indices are numeric or character vectors or empty
#' (missing) or NULL.
#' @keywords internal
#'
#' @export
#'

# function to subset extended selection table with [
`[.selection_table` <- function(X, i = NULL, j = NULL, drop = FALSE) {
  if (is.character(i)) i <- which(row.names(X) %in% i)
  if (is.character(j)) j <- which(names(X) %in% j)
  if (is.null(i)) i <- 1:nrow(X)
  if (is.null(j)) j <- seq_len(ncol(X))
  
  Y <- as.data.frame(X)[i, j, drop = drop]
  
  if (is.data.frame(Y)) {
    attributes(Y)$check.results <- attributes(X)$check.results[i, , ]
    
    attributes(Y)$by.song <- attributes(X)$by.song
    
    attributes(Y)$call <- base::match.call()
    
    class(Y) <- class(X)
  }
  return(Y)
}

##############################################################################################################
#' extract method for class \code{extended_selection_table}
#'
#' @param X Object of class \code{extended_selection_table}, generated by \code{\link{selection_table}}.
#' @param i, j,	indices specifying elements to extract or replace. Indices are numeric or character vectors or empty
#' (missing) or NULL.
#' @keywords internal
#'
#' @export
#'

# function to subset extended selection table with [
`[.extended_selection_table` <- function(X, i = NULL, j = NULL, drop = FALSE) {
  if (is.character(i)) i <- which(row.names(X) %in% i)
  if (is.character(j)) j <- which(names(X) %in% j)
  if (is.null(i)) i <- 1:nrow(X)
  if (is.null(j)) j <- seq_len(ncol(X))
  
  Y <- as.data.frame(X)[i, j, drop = drop]
  
  if (is.data.frame(Y)) {
    # subset wave objects in attributes
    attributes(Y)$wave.objects <- attributes(X)$wave.objects[names(attributes(X)$wave.objects) %in% Y$sound.files]
    
    attributes(Y)$check.results <-
      attributes(X)$check.results[paste(attributes(X)$check.results$sound.files, attributes(X)$check.results$selec) %in% paste(Y$sound.files, Y$selec), ]
    
    # attributes(Y)$check.results <- attributes(X)$check.results[attributes(X)$check.results$sound.files %in% Y$sound.files, ]
    
    attributes(Y)$by.song <- attributes(X)$by.song
    
    attributes(Y)$call <- base::match.call()
    
    class(Y) <- class(X)
  }
  
  return(Y)
}

#############################################################################################################

#' print method for class \code{extended_selection_table}
#'
#' @param x Object of class \code{extended_selection_table}, generated by \code{\link{selection_table}}.
#' @keywords internal
#' @param ...	 further arguments passed to or from other methods. Ignored when printing extended selection tables.
#' @export
#'

print.extended_selection_table <- function(x, ...) {
  message2(paste("Object of class", cli::style_bold("'extended_selection_table'")))
  
  # print call
  if (!is.null(attributes(x)$call)) {
    message2(color = "silver", x = paste("* The output of the following", "call:"))
    
    cll <- deparse(attributes(x)$call)
    if (length(cll) > 1) cll <- paste(cll, collapse = " ")
    if (nchar(as.character(cll)) > 250) {
      cll <- paste(substr(x = as.character(cll), start = 0, stop = 250), "...")
    }
    message2(color = "silver", x = cli::style_italic(gsub("    ", "", cll)))
  }
  
  message2(color = "silver", x = paste(cli::style_bold("\nContains:"), "\n*  A selection table data frame with", nrow(x), "row(s) and", ncol(x), "columns:"))
  
  # define columns to show
  cols <- if (ncol(x) > 6) 1:6 else seq_len(ncol(x))
  
  kntr_tab <- knitr::kable(head(x[, cols, drop = FALSE]), escape = FALSE, digits = 4, justify = "centre", format = "pipe")
  
  for (i in seq_len(length(kntr_tab)))
    message2(color = "silver", x = paste0(kntr_tab[i]))
  
  if (ncol(x) > 6)
    message2(color = "silver", x = paste0("... ", ncol(x) - 6, " more column(s) (", paste(colnames(x)[7:ncol(x)], collapse = ", "), ")"))
  if (nrow(x) > 6)
    message2(color = "silver", x = paste0(if (ncol(x) <= 6) "..." else "", " and ", nrow(x) - 6, " more row(s)"))
  
  message2(color = "silver", x = paste0("\n* ", length(attr(x, "wave.objects")), " wave object(s) (as attributes): "))
  
  message2(color = "silver", x = head(names(attr(x, "wave.objects"))))
  
  if (length(attr(x, "wave.objects")) > 6)message2(color = "silver", x = paste0("... and ", length(attr(x, "wave.objects")) - 6, " more"))
  
  message2(color = "silver", x = "\n* A data frame (check.results) generated by check_sels() (as attribute)")
  
  if (attr(x, "by.song")[[1]]) {
    message2(color = "silver", x = paste0(cli::style_bold("\nAdditional information:"), "\n* The selection table was created", cli::style_italic(cli::style_bold(" by song ")), "(see 'class_extended_selection_table')"))
  } else {
    message2(color = "silver", x = paste0("\nThe selection table was created", cli::style_italic(cli::style_bold(" by element ")), "(see 'class_extended_selection_table')"))
  }
  
  # print number of sampling rates
  smp.rts <- unique(attr(x, "check.results")$sample.rate)
  if (length(smp.rts) == 1) {
    message2(color = "silver", x = paste0("* ", length(smp.rts), " sampling rate(s) (in kHz): ", paste(cli::style_bold(smp.rts), collapse = "/")))
  } else {
    message2(paste0("* ", length(smp.rts), " sampling rate(s): ", paste(cli::style_bold(smp.rts), collapse = "/")), color = "red")
  }
  
  # print number of sampling rates
  bt.dps <- unique(attr(x, "check.results")$bits)
  if (length(bt.dps) == 1) {
    message2(color = "silver", x = paste0("* ", length(bt.dps), " bit depth(s): ", paste(cli::style_bold(bt.dps), collapse = "/")))
  } else {
    message2(paste0("* ", length(bt.dps), " bit depth(s): ", paste(cli::style_bold(bt.dps), collapse = "/")),  color = "red")
  }
  
  # print warbleR version
  if (!is.null(attr(x, "warbleR.version"))) {
    message2(color = "silver", x = paste0("* Created by warbleR ", attr(x, "warbleR.version")))
  } else {
    message2(color = "silver", x = "* Created by warbleR < 1.1.21")
  }
}

##############################################################################################################

#' print method for class \code{selection_table}
#'
#' @param x Object of class \code{selection_table}, generated by \code{\link{selection_table}}.
#' @param ...	 further arguments passed to or from other methods. Ignored when printing selection tables.
#' @keywords internal
#'
#' @export
#'

print.selection_table <- function(x, ...) {
  message2(paste("Object of class", cli::style_bold("'selection_table'")))
  
  # print call
  if (!is.null(attributes(x)$call)) {
    message2(color = "silver", x = paste("* The output of the following", "call:"))
    
    cll <- paste0(deparse(attributes(x)$call))
    message2(color = "silver", x = cli::style_italic(gsub("    ", "", cll)))
  }
  
  message2(color = "silver", x = paste(cli::style_bold("\nContains:"), "*  A selection table data frame with", nrow(x), "rows and", ncol(x), "columns:"))
  
  # print data frame
  # define columns to show
  cols <- if (ncol(x) > 6) 1:6 else seq_len(ncol(x))
  
  kntr_tab <- knitr::kable(head(x[, cols]), escape = FALSE, digits = 4, justify = "centre", format = "pipe")
  
  for (i in seq_len(length(kntr_tab))) 
    message2(color = "silver", x = paste0(kntr_tab[i], ""))
  
  if (ncol(x) > 6)
    message2(color = "silver", x = paste0("... ", ncol(x) - 6, " more column(s) (", paste(colnames(x)[7:ncol(x)], collapse = ", "), ")"))
  if (nrow(x) > 6)message2(color = "silver", x = paste0(if (ncol(x) <= 6) "..." else "", " and ", nrow(x) - 6, " more row(s)"))
  
  
  message2(color = "silver", x = "\n * A data frame (check.results) generated by check_sels() (as attribute) ")
  
  # print warbleR version
  if (!is.null(attr(x, "warbleR.version"))) {
    message2(color = "silver", x = paste0("created by warbleR ", attr(x, "warbleR.version")))
  } else {
    message2(color = "silver", x = "created by warbleR < 1.1.21")
  }
}

##############################################################################################################

#' Fix extended selection tables
#'
#' \code{fix_extended_selection_table} fixes extended selection tables that have lost their attributes
#' @usage fix_extended_selection_table(X, Y, to.by.song = FALSE)
#' @param X an object of class 'selection_table' or data frame that contains columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end).
#' @param Y an object of class 'extended_selection_table'
#' @param to.by.song Logical argument to control if the attributes are formatted to a match a 'by.song' extended selection table. This is required when 'X' is created by collapsing an Y by song (see 'by.song' argument in \code{\link{selection_table}}). Mostly needed internally by some warbleR functions.
#' @return An extended selection table. 
#' @export
#' @name fix_extended_selection_table
#' @examples{
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#'
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' # create extended selection table
#' ext_st <- selection_table(lbh_selec_table, extended = TRUE,
#' path = tempdir())
#'
#' # remove attributes
#' st <- as.data.frame(ext_st)
#'
#' # check class
#' class(st)
#'
#' # fix selection table
#' st <- fix_extended_selection_table(X = st, Y = ext_st)
#'
#' # check class
#' class(st)
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on may-14-2018 (MAS)

fix_extended_selection_table <- function(X, Y, to.by.song = FALSE) {
  # X is new data frame and Y the original one
  # add wave objects
  if (!is_extended_selection_table(Y)) stop2("Y must be a extended selection table")
  attributes(X)$wave.objects <- attributes(Y)$wave.objects[names(attributes(Y)$wave.objects) %in% X$sound.files]
  
  attributes(X)$check.results <- attributes(Y)$check.results[attributes(Y)$check.results$sound.files %in% X$sound.files, ]
  
  # add additional attributes
  xtr.attr <- setdiff(names(attributes(Y)[!sapply(attributes(Y), is.null)]), c("names", "row.names", "check.results", "wave.objects", "dim", "dimnames"))
  
  if (length(xtr.attr) > 0) {
    for (i in xtr.attr) attr(X, i) <- attr(Y, i)
  }
  
  attributes(X)$call <- base::match.call()
  
  # fix if by.song (used internally by spectrograms())
  if (to.by.song) {
    new_X_attr_list <- lapply(unique(X$sound.files), function(x) {
      sub.Y.check <- attr(Y, "check.results")[attr(Y, "check.results")$sound.files == x, ]
      sub.Y.check.1 <- sub.Y.check[1, ]
      sub.Y.check.1$selec <- X$selec[X$sound.files == x]
      sub.Y.check.1$start <- min(sub.Y.check$start)
      sub.Y.check.1$orig.start <- min(sub.Y.check$orig.start)
      sub.Y.check.1$end <- max(sub.Y.check$end)
      sub.Y.check.1$orig.end <- max(sub.Y.check$orig.end)
      wave <- attr(Y, "wave.objects")[[which(names(attr(Y, "wave.objects")) == x)]]
      sub.Y.check.1$duration <- duration(wave)
      sub.Y.check.1$wav.size <- round(wave@bit * ifelse(wave@stereo, 2, 1) * wave@samp.rate * duration(wave) / 4) / 1024
      sub.Y.check.1$mar.before <- sub.Y.check$mar.before[which.min(sub.Y.check$start)]
      sub.Y.check.1$mar.after <- sub.Y.check$mar.before[which.max(sub.Y.check$start)]
      sub.Y.check.1$n.samples <- length(wave)
      
      return(sub.Y.check.1)
    })
    
    X_attr <- do.call(rbind, new_X_attr_list)
    
    attributes(X)$check.results <- X_attr
  }
  
  
  return(X)
}

##############################################################################################################
#' rbind method for class \code{selection_table}
#'
#' @param ... Objects of class \code{selection_table}, generated by \code{\link{selection_table}}.
#' @keywords internal
#'
#' @export
#'

rbind.selection_table <- function(..., deparse.level = 1) {
  mcall <- list(...)
  X <- mcall[[1]]
  Y <- mcall[[2]]
  
  if (!is_selection_table(X) | !is_selection_table(Y)) stop2("both objects must be of class 'selection_table'")
  
  if (any(paste(X$sound.files, X$selec) %in% paste(Y$sound.files, Y$selec))) stop2("Some sound files/selec are found in both selection tables")
  
  cl.nms <- intersect(names(X), names(Y))
  
  W <- rbind(as.data.frame(X[, cl.nms, drop = FALSE]), as.data.frame(Y[, cl.nms, drop = FALSE]),
             make.row.names = TRUE,
             stringsAsFactors = FALSE
  )
  
  cl.nms.cr <- intersect(names(attr(X, "check.results")), names(attr(Y, "check.results")))
  
  attr(W, "check.results") <- rbind(attr(X, "check.results")[, cl.nms.cr, drop = FALSE], attr(Y, "check.results")[, cl.nms.cr, drop = FALSE], make.row.names = TRUE, stringsAsFactors = FALSE)
  
  attr(W, "by.song") <- attr(X, "by.song")
  
  class(W) <- class(X)
  
  attributes(W)$call <- base::match.call()
  
  return(W)
}

## Example
# data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#
#
#
# writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
# writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
# writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
# writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#
# # create extended selection table
# # st <- selection_table(lbh_selec_table)
# st1 <- st[1:5, ]
#
# st2 <- st[6:10, ]
#
# # fix selection table
# st <- rbind(X = st1, Y = st2)


##############################################################################################################

#' rbind method for class \code{extended_selection_table}
#'
#' @param ... Objects of class \code{extended_selection_table}, generated by \code{\link{selection_table}}.
#' @keywords internal
#'
#' @export
#'

rbind.extended_selection_table <- function(..., deparse.level = 1) {
  mcall <- list(...)
  X <- mcall[[1]]
  Y <- mcall[[2]]
  
  if (!is_extended_selection_table(X) | !is_extended_selection_table(Y)) stop2("both objects must be of class 'extended_selection_table'")
  
  if (attr(X, "by.song")[[1]] != attr(Y, "by.song")[[1]]) stop2("both objects should have been created either 'by song' or by element' (see 'by.song' argument in selection_table())")
  
  if (any(paste(X$sound.files, X$selec) %in% paste(Y$sound.files, Y$selec))) stop2("Some sound files/selec are found in both extended selection tables")
  
  waves.X <- names(attr(X, "wave.objects"))
  waves.Y <- names(attr(Y, "wave.objects"))
  
  if (any(waves.X %in% waves.Y)) warning2("Some wave object names are found in both extended selection tables, they are assumed to refer to the same wave object and only one copy will be kept (use rename_est_waves() to change sound file/wave object names if needed)")
  
  cl.nms <- intersect(names(X), names(Y))
  
  W <- rbind(as.data.frame(X[, cl.nms, drop = FALSE]), as.data.frame(Y[, cl.nms, drop = FALSE]), make.row.names = TRUE)
  
  cl.nms.cr <- intersect(names(attr(X, "check.results")), names(attr(Y, "check.results")))
  
  attr(W, "check.results") <- rbind(attr(X, "check.results")[, cl.nms.cr, drop = FALSE], attr(Y, "check.results")[, cl.nms.cr, drop = FALSE], make.row.names = TRUE)
  
  attr(W, "wave.objects") <- c(attr(X, "wave.objects"), attr(Y, "wave.objects"))
  
  attr(W, "by.song") <- attr(X, "by.song")
  
  class(W) <- class(X)
  
  # remove duplicated  if any
  attr(W, "wave.objects") <- attr(W, "wave.objects")[!duplicated(names(attr(W, "wave.objects")))]
  
  # fix version to current version
  attributes(W)$warbleR.version <- packageVersion("warbleR")
  
  attributes(W)$call <- base::match.call()
  
  return(W)
}
