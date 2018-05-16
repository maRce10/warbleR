#' Create 'selection_table' and 'extended_selection_table' objects
#' 
#' \code{selection_table} converts data frames into an object of classes 'selection_table' or 'extended_selection_table'.
#' @usage selection_table(X, max.dur = 10, path = NULL, whole.recs = FALSE,
#' extended = FALSE, confirm.extended = TRUE, mar = 0.1, by.song = NULL, 
#' pb = TRUE, parallel = 1, ...)
#' @param X data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "selec": unique selection identifier (within a sound file), 3) "start": start time and 4) "end": 
#' end time of selections. Columns for 'top.freq', 'bottom.freq' and 'channel' are optional.  Alternatively, a 'selection_table' class object can be input to double check selections. 
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input object for other \code{\link{warbleR}} functions.
#' @param max.dur the maximum duration of expected for a selection  (ie. end - start). If surpassed then an error message 
#' will be generated. Useful for detecting errors in selection tables.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param whole.recs Logical. If \code{TRUE} the function will create a selection 
#' table for all sound files in the working directory (or "path") with `start = 0` 
#' and `end = wavdur()`. Default is if \code{FALSE}.  
#' @param extended Logical. If \code{TRUE}, the function will create an object of class 'extended_selection_table' 
#' which included the wave objects of the selections as an additional attribute ('wave.objects') to the data set. This is 
#' and self-contained format that does not require the original sound files for running most acoustic analysis in 
#' \code{\link{warbleR}}. This can largely faciliate the storing and sharing of (bio)acoustic data. Default 
#' is if \code{FALSE}. An extended selection table won't be created if there is any issue with the selection. See 
#' 'details'.
#' @param mar Numeric vector of length 1 specifying the margins (in seconds) 
#' adjacent to the start and end points of the selections when creating extended 
#' selection tables. Default is 0.1. Ignored if 'extended' is \code{FALSE}.
#' @param confirm.extended Logical. If \code{TRUE} then the size of the 'extended_selection_table' 
#' will be estimated and the user will be asked for confirmation (in the console)
#'before proceeding. Ignored if 'extended' is \code{FALSE}. This is used to prevent
#' generating objects too big to be dealth with by R. See 'details' for more information about extended selection table size.
#' @param by.song Character string with the column name containing song labels. If provided a wave object containing for 
#' all selection belonging to a single song would be saved in the extended selection table (hence only applicable for 
#' extended selection tables). Note that the function assumes that song labels are not repeated within a sound file. 
#' If \code{NULL} (default), wave objects are created for each selection (e.g. by selection). 
#' Ignored if \code{extended = FALSE}.
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied. 
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param ... Additional arguments to be passed to \code{\link{checksels}} for customizing
#' checking routine.
#' @return An object of class selection_table which includes the original data frame plus the following additional attributes:
#' \itemize{
#'    \item 1) A data frame with the output of \code{\link{checksels}} run on the input data frame. If a extended selection table is created it will also include the original values in the input data frame for each selections. This are used by downstream warbleR functions to improve efficiency and avoid
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
#' This is useful for avoiding errors in dowstream functions (e.g. \code{\link{specan}}, \code{\link{xcorr}}, \code{\link{catalog}}, \code{\link{dfDTW}}). Note also that corrupt files can be
#' fixed using \code{\link{fixwavs}} ('sox' must be installed to be able to run this function).
#' The 'selection_table' class can be input in subsequent functions. 
#' 
#' When \code{extended = TRUE} the function will generate an object of class 'extended_selection_table' which 
#' will also contains the wave objects for each of the selections in the data frame.
#' This transforms selection tables into self-contained objects as they no longer need the original
#' sound files to run acoustic analysis. This can largely faciliate the storing and sharing of (bio)acoustic data.
#' Extended selection table size will be a function of the number of selections \code{nrow(X)}, sampling rate, selection 
#' duration and margin duration. As a guide, a selection table
#' with 1000 selections similar to the ones in 'selec.table' (mean duration ~0.15
#'  seconds) at 22.5 kHz sampling rate and the default margin (\code{mar = 0.1}) 
#'  will generate a extended selection table of ~31 MB (~310 MB for a 10000 rows 
#'  selection table). You can check the size of the output extended selection table
#'  with the \code{\link[utils]{object.size}} function. Note that extended selection table created 'by.song' could be 
#'  considerable larger.
#' @seealso \code{\link{checkwavs}}
#' @export
#' @name selection_table
#' @name make.selection.table
#' @aliases make.selection.table
#' @examples
#' {
#' # First set temporary folder
#'  setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' # make selection table
#' st <- selection_table(X = selec.table)
#'
#' is_selection_table(st)
#' 
#' #' # make extended selection table
#' st <- selection_table(X = selec.table, extended = TRUE, confirm.extended = FALSE)
#' 
#' is_extended_selection_table(st)
#'
#' ### make extended selection by song  
#' # create a song variable
#' selec.table$song <- as.numeric(selec.table$sound.files)
#' 
#' st <- selection_table(X = selec.table, extended = TRUE, confirm.extended = FALSE, by.song = "song")
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on may-9-2018 (MAS)

selection_table <- function(X, max.dur = 10, path = NULL, whole.recs = FALSE,
    extended = FALSE, confirm.extended = TRUE, mar = 0.1, by.song = NULL, pb = TRUE, parallel = 1, ...)
{
  if (is.null(path)) path <- getwd()
  
  # reset working directory 
  on.exit(setwd(getwd()))
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(selection_table)
  
  # get warbleR options
  opt.argms <- .Options$warbleR
  
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
  
  # If parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  
  if (whole.recs){ 
    sound.files <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)
    
    if (length(sound.files) == 0) stop("No sound files were found") 
    
    X <- data.frame(sound.files, selec = 1, channel = 1, start = 0, end = wavdur(files = sound.files, path = path)$duration)
  }

  if(pb) write(file = "", x ="checking selections:")
    check.results <- checksels(X, path = path, wav.size = TRUE, ...)        
  
    if (any(check.results$check.res != "OK")) stop("Not all selections can be read (use check.sels() to locate problematic selections)")
    
  X <- check.results[ , names(check.results) %in% names(X)]
  
  ## Set the name for the class
  class(X) <- unique(append("selection_table", class(X)))
  
  check.results <- check.results[, names(check.results) %in% c("sound.files", "selec", by.song, "check.res", "duration", "min.n.sample", "sample.rate", "channels", "bits", "wav.size.MB", "sound.file.samples")]
  
  # add wave object to extended file
  if (extended)
{ 
    exp.size <- sum(round(check.results$bits * check.results$sample.rate * (check.results$duration + (mar * 2)) / 4) / 1024 ^ 2)
    
    if (confirm.extended)
    answer <- readline(prompt = paste0("Expected 'extended_selection_table' size is ~", round(exp.size, 2), "MB (~", round(exp.size/1024, 5), " GB) \n Do you want to proceed (y/n): \n")) else answer <- "yeah dude!"

    if (substr(answer, 1, 1) %in% c("y", "Y")) # if yes
{
      check.results$orig.start <- X$start
      check.results$orig.end <- X$end
        
      check.results$mar.after <- check.results$mar.before <- rep(mar, nrow(X))
      dur <- wavdur(files = as.character(X$sound.files))$duration
      
      #reset margin signals if lower than 0 or higher than duration
      for(i in 1:nrow(X))  
      {
        if(X$start[i] < mar) check.results$mar.before[i] <- X$start[i] 
        if(X$end[i] + mar > dur[i]) check.results$mar.after[i] <- dur[i] - X$end[i]
        }  
    
      if (!is.null(by.song))
        {
        Y <- song_param(X = as.data.frame(X), song_colm = by.song, pb = FALSE)[, c("sound.files", "song", "start", "end")]
        
        Y$mar.before <- sapply(unique(Y$song), function(x) check.results$mar.before[which.min(check.results$orig.start[check.results$song == x])])
        
        Y$mar.after <- sapply(unique(Y$song), function(x) check.results$mar.after[which.max(check.results$orig.end[check.results$song == x])])
      } else {
        Y <- X
        Y$mar.before <- check.results$mar.before
        Y$mar.after <- check.results$mar.after
        }
      
    #save wave objects as a list attributes
      # set clusters for windows OS
      # set pb options 
      pbapply::pboptions(type = ifelse(pb, "timer", "none"))
      
      if (Sys.info()[1] == "Windows" & parallel > 1)
        cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
      
       if(pb) write(file = "", x ="saving wave objects into extended selection table:")
      
      attributes(X)$wave.objects <- pbapply::pblapply(1:nrow(Y), cl = cl, function(x) read_wave(X = Y, index = x, from = Y$start[x] - Y$mar.before[x], to = Y$end[x] + Y$mar.after[x],  path = path))
    
    # reset for new dimensions  
    check.results$start <- X$start <- check.results$mar.before
    check.results$end <- X$end <- check.results$mar.before + check.results$duration

  names(check.results)[names(check.results) == "sound.files"] <- "orig.sound.files"
  names(check.results)[names(check.results) == "selec"] <- "orig.selec"
  
  if (!is.null(by.song)) 
  {
    names(attributes(X)$wave.objects) <- paste0(Y$sound.files, "-song_", Y$song) 
    X$sound.files <- check.results$sound.files <- paste0(X$sound.files, "-song_", as.data.frame(X)[ , names(X) == by.song,])
    
  for(i in unique(X$sound.files))
   check.results$selec[check.results$sound.files == i] <- X$selec[X$sound.files == i] <- 1:nrow(X[X$sound.files == i, drop = FALSE])
  
    check.results$n.samples <- NA
    
    durs <- X$end - X$start
    
    for(w in unique(X$sound.files))
    {
      check.results$start[check.results$sound.files == w] <- X$start[X$sound.files == w] <-  X$start[X$sound.files == w][which.min(check.results$orig.start[check.results$sound.files == w])] + (check.results$orig.start[check.results$sound.files == w] - min(check.results$orig.start[check.results$sound.files == w]))
     
    # add n.samples for header info
      check.results$n.samples[check.results$sound.files == w] <- length(attr(X, "wave.objects")[[which(names(attr(X, "wave.objects")) == w)]]@left)
    }
    check.results$end <- X$end <- X$start + durs
    
    } else 
    {  
    names(attributes(X)$wave.objects) <- check.results$sound.files <- X$sound.files <- paste(X$sound.files, X$selec, sep = "_")
  check.results$selec <- X$selec <- 1
  
  check.results$n.samples <- as.vector(sapply(attr(X, "wave.objects"), function(x) length(x@left)))     # add n.samples for header info
  }
  
  ## Set the name for the class
  class(X)[class(X) == "selection_table"] <- "extended_selection_table"
   }
  } else
   check.results$n.samples <- check.results$sound.file.samples
  
  # order check results columns
  check.results <- check.results[,na.omit(match(c("orig.sound.files", "orig.selec", "orig.start", "orig.end", "sound.files", "selec", "start", "end", "check.results", "duration", "sample.rate", "channels", "bits", "wav.size.MB", "mar.before", "mar.after", "n.samples"), names(check.results)))]
  
  attributes(X)$check.results <- check.results
  
  if(is_extended_selection_table(X) & !is.null(by.song)) attributes(X)$by.song  <- list(by.song = TRUE, song.column = by.song) else attributes(X)$by.song  <- list(by.song = FALSE, song.column = by.song)
  
  if(extended & confirm.extended & !is_extended_selection_table(X)) cat("'extended_selection_table' was not created")
  
  return(X)
}

# alias
make.selection.table <- selection_table

##############################################################################################################


#' Class 'selection_table': double-checked frequency/time coordinates of selections
#'
#' Class for selections of signals in sound files
#' @export
#' @exportClass  selection_table
#' @details An object of class \code{selection_table} created by \code{\link{selection_table}} is a list with the following elements:
#'  \itemize{
#'  \item\code{selections}: data frame containing the frequency/time coordinates of the selections, sound file names, and any  additional information
#'  \item \code{check.resutls}: results of the checks on data consistency using \link{checksels}
#' }
#' @seealso \code{\link{selection_table}}, \code{\link{selection_table}}

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
#' # First set temporary folder
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' 
#' is_selection_table(selec.table)
#' 
#' # setwd(tempdir())
#' 
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' st <- selection_table(selec.table)
#' 
#' is_selection_table(st)
#' 
#' class(st)
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on may-9-2018 (MAS)

is_selection_table <- function(x) inherits(x, "selection_table")


##############################################################################################################

#' Class 'extended_selection_table': selection table containing wave objects
#'
#' Class for selections of signals in sound files and corresponding wave objects
#' @export
#' @exportClass  extended_selection_table
#' @details An object of class \code{extended_selection_table} created by \code{\link{selection_table}} is a list with the following elements:
#'  \itemize{
#'  \item \code{selections}: data frame containing the frequency/time coordinates of the selections, sound file names, and any  additional information
#'  \item \code{check.resutls}: results of the checks on data consistency using \link{checksels}
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
#' # First set temporary folder
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' 
#' is_extended_selection_table(selec.table)
#' 
#' # setwd(tempdir())
#' 
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' st <- selection_table(selec.table, extended = TRUE, confirm.extended = FALSE)
#' 
#' is_extended_selection_table(st)
#' 
#' class(st)
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on may-9-2018 (MAS)

is_extended_selection_table <- function(x) inherits(x, "extended_selection_table")

##############################################################################################################

# function to subset extended selection table with [
`[.selection_table` <- function(X, i = NULL, j = NULL, drop = TRUE) {
  
  if (is.character(i)) i <- which(row.names(X) %in% i)
  if (is.character(j)) j <- which(names(X) %in% j)
  if (is.null(i)) i <- 1:nrow(X)
  if (is.null(j)) j <- 1:ncol(X)
  
  Y <- as.data.frame(X)[i, j, drop = drop]
  
  if (is.data.frame(Y)) 
{
  attributes(Y)$check.results <- attributes(X)$check.results[i, ,]
  
  attributes(Y)$by.song <- attributes(X)$by.song
  
  class(Y) <- class(X)
  }
  return(Y)
}

##############################################################################################################

# function to subset extended selection table with [
`[.extended_selection_table` <- function(X, i = NULL, j = NULL, drop = TRUE) {
  
  if (is.character(i)) i <- which(row.names(X) %in% i)
  if (is.character(j)) j <- which(names(X) %in% j)
  if(is.null(i)) i <- 1:nrow(X)
  if(is.null(j)) j <- 1:ncol(X)
  
  Y <- as.data.frame(X)[i, j, drop = drop]
  
  if (is.data.frame(Y)) 
  {
 attributes(Y)$wave.objects <- attributes(X)$wave.objects[names(attributes(X)$wave.objects) %in% Y$sound.files] 
  
 attributes(Y)$check.results <- attributes(X)$check.results[attributes(X)$check.results$sound.files %in% Y$sound.files, ] 

  attributes(Y)$by.song <- attributes(X)$by.song
  
  class(Y) <- class(X)
  }

  return(Y)
}


# function to print extended selection table
print.extended_selection_table <- function(X) {
  
  cat(paste("object of class 'extended_selection_table' \n contains a selection table data frame with"), nrow(X), "rows and", ncol(X), "columns: \n")
  
  print(head(as.data.frame(X)))
  
  if(nrow(X) > 6) cat(paste0("... and ", nrow(X) - 6, " more rows \n"))  
  
  cat(paste0(length(attr(X, "wave.objects"))," wave objects (as attributes): \n"))
  
  print(head(names(attr(X, "wave.objects"))))
  
  if(length(attr(X, "wave.objects")) > 6) cat(paste0("... and ", length(attr(X, "wave.objects")) - 6, " more \n"))  
  
  cat("and a data frame (check.results) generated by checkres() (as attribute) \n")
  
  if(attr(X, "by.song")[[1]]) cat("the selection table was created by song (see 'class_extended_selection_table')") else
    cat("the selection table was created by element (see 'class_extended_selection_table')")
}

##############################################################################################################

# function to print selection table 
print.selection_table <- function(X) {
  
  cat(paste("object of class 'selection_table' \n contains a selection table data frame with"), nrow(X), "rows and", ncol(X), "columns: \n")
  
  print(head(as.data.frame(X)))
  
  if(nrow(X) > 6) cat(paste0("... and ", nrow(X) - 6, " more rows \n"))  
  
  cat("and a data frame (check.results) generated by checkres() (as attribute)")
}

##############################################################################################################


#' Fix extended selection tables 
#' 
#' \code{fix_extended_selection_table} fixes extended selection tables that have lost their attributes
#' @usage fix_extended_selection_table(X, Y)
#' @param X an object of class 'selection_table' or data frame that  contains columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end).  
#' @param Y an object of class 'extended_selection_table'
#' @export
#' @name fix_extended_selection_table
#' @examples{
#' # First set temporary folder
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#'
#' # setwd(tempdir())
#'
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#'
#' # create extended selection table
#' ext_st <- selection_table(selec.table, extended = TRUE, confirm.extended = FALSE)
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
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on may-14-2018 (MAS)

fix_extended_selection_table <- function(X, Y){
  #X is new data frame and Y the original one
  #add wave objects
  if (!is_extended_selection_table(Y)) stop("Y must be a extended selection table")
  attributes(X)$wave.objects <- attributes(Y)$wave.objects[names(attributes(Y)$wave.objects) %in% X$sound.files] 
  
  #add check results
  # attributes(X)$check.results <- attributes(Y)$check.results[paste(attributes(Y)$check.results$sound.files, attributes(Y)$check.results$selec) %in% paste(attributes(X)$check.results$sound.files, attributes(X)$check.results$selec)]

  attributes(X)$check.results <- attributes(Y)$check.results[attributes(Y)$check.results$sound.files %in% X$sound.files, ]
  
    # add class
    # class(X) <- class(Y)
  
  #add additional attributes  
xtr.attr <- setdiff(names(attributes(Y)[!sapply(attributes(Y), is.null)]), c("names", "row.names", "check.results", "wave.objects", "dim", "dimnames"))

# xtr.attr <- xtr.attr[!is.null(xtr.attr)]

if(length(xtr.attr) > 0)
  # print(names(xtr.attr))
  for(i in xtr.attr) attr(X, i) <- attr(Y, i)
  
return(X)
}

##############################################################################################################

#' @export

rbind.selection_table <- function(..., deparse.level = 1) {
  
  mcall <- list(...)
  X <- mcall[[1]]
  Y <- mcall[[2]]
  
  if (!is_selection_table(X) | !is_selection_table(Y)) stop("both objects must be of class 'selection_table'")
  
  if (any(paste(X$sound.files, X$selec) %in% paste(Y$sound.files, Y$selec))) stop("Some sound files/selec are found in both selection tables")
  
  cl.nms <- intersect(names(X), names(Y))

  W <- rbind(as.data.frame(X[ , cl.nms, drop = FALSE]), as.data.frame(Y[ , cl.nms, drop = FALSE]), make.row.names = TRUE,
             stringsAsFactors = FALSE)
    
  cl.nms.cr <- intersect(names(attr(X, "check.results")), names(attr(Y, "check.results")))
  
  attr(W, "check.results") <- rbind(attr(X, "check.results")[ , cl.nms.cr, drop = FALSE], attr(Y, "check.results")[ , cl.nms.cr, drop = FALSE], make.row.names = TRUE, stringsAsFactors = FALSE)
  
  attr(W, "by.song") <- attr(X, "by.song")
  
 class(W) <- class(X)
 
 return(W)
 
  }

## Example
# data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#
# setwd(tempdir())
#
# writeWave(Phae.long1,"Phae.long1.wav")
# writeWave(Phae.long2,"Phae.long2.wav")
# writeWave(Phae.long3,"Phae.long3.wav")
# writeWave(Phae.long4,"Phae.long4.wav")
# 
# # create extended selection table
# # st <- selection_table(selec.table)
# st1 <- st[1:5, ]
# 
# st2 <- st[6:10, ]
# 
# # fix selection table
# st <- rbind(X = st1, Y = st2)


##############################################################################################################

#' @export

rbind.extended_selection_table <- function(..., deparse.level = 1) {
  
  mcall <- list(...)
  X <- mcall[[1]]
  Y <- mcall[[2]]
  
  X <- mcall[[1]]
  Y <- mcall[[2]]
  
  if (!is_extended_selection_table(X) | !is_extended_selection_table(Y)) stop("both objects must be of class 'selection_table'")

  if (!identical(attr(X, "by.song"), attr(Y, "by.song"))) stop("both objects should have been created either 'by song' or by element' (see 'by.song' argument in selection_table())")
  
  if (any(paste(X$sound.files, X$selec) %in% paste(Y$sound.files, Y$selec))) stop("Some sound files/selec are found in both extended selection tables")
  
  cl.nms <- intersect(names(X), names(Y))
  
  W <- rbind(as.data.frame(X[ , cl.nms, drop = FALSE]), as.data.frame(Y[ , cl.nms, drop = FALSE]), make.row.names = TRUE,
             stringsAsFactors = FALSE)
  
  cl.nms.cr <- intersect(names(attr(X, "check.results")), names(attr(Y, "check.results")))
  
  attr(W, "check.results") <- rbind(attr(X, "check.results")[ , cl.nms.cr, drop = FALSE], attr(Y, "check.results")[ , cl.nms.cr, drop = FALSE], make.row.names = TRUE, stringsAsFactors = FALSE)
  
  attr(W, "wave.objects") <- c(attr(W, "wave.objects"), attr(Y, "wave.objects"))
  
  attr(W, "by.song") <- attr(X, "by.song")
  
  class(W) <- class(X)
  
  return(W)
}

## Example
# data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#
# setwd(tempdir())
#
# writeWave(Phae.long1,"Phae.long1.wav")
# writeWave(Phae.long2,"Phae.long2.wav")
# writeWave(Phae.long3,"Phae.long3.wav")
# writeWave(Phae.long4,"Phae.long4.wav")
# 
# # create extended selection table
# # st <- selection_table(selec.table, extended = TRUE, confirm.extended = FALSE)
# st1 <- st[1:5, ]
# 
# st2 <- st[6:10, ]
# 
# # fix selection table
# st <- rbind(X = st1, Y = st2)

