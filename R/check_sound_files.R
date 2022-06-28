#' Check sound files
#' 
#' \code{check_sound_files} checks whether sound files can be read by subsequent functions.
#' @usage check_sound_files(X = NULL, path = NULL)
#' @param X Optional. 'selection_table' object or data frame with the following columns: 1) "sound.files": name of the sound 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. If provided the function also returns the
#' smallest number of samples from the listed selections, which limits the minimum window 
#' length (wl argument in other functions) that can be used in batch analyses. 
#' This could be useful for avoiding errors in downstream functions (e.g. \code{\link{spectro_analysis}}).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.  
#' @return If all sound files are ok, returns message "All files can be read".
#'   Otherwise returns the names of the corrupted sound files.
#' @details This function checks if sound files in the working directory can be read.
#' Users must set the working directory where they wish to check sound files beforehand. 
#' If X is provided it also returns the smallest number of samples from
#' the selections listed in X (if all files can be read). Note that corrupt files can be
#' fixed using \code{\link{fix_wavs}}) ('sox' must be installed to be able to run this function).
#' The function is intended for a "quick and dirty" check of the sound files in a selections data
#'  frame. For a more thorough analysis see \code{\link{check_sels}}.
#' @export
#' @seealso \code{\link{check_sels}} \code{\link{tailor_sels}}
#' @name check_sound_files
#' @examples{
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#' 
#' # without selection data frame
#' check_sound_files(path = tempdir())
#' 
#' # without selection data frame
#' check_sound_files(X = lbh_selec_table, path = tempdir())
#' }
#' @references {Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on jul-5-2016 (MAS)

check_sound_files <- function(X = NULL, path = NULL) { 
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(check_sound_files)
  
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
  
  #return warning if not all sound files were found
  files <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)
  if (length(files) == 0) stop("no sound files in working directory") 
  
  
  if (!is.null(X))
  {
    #if X is not a data frame
    if (!any(is.data.frame(X), is_selection_table(X))) stop("X is not of a class 'data.frame' or 'selection_table'")
    
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
    
    if (length(unique(X$sound.files[(X$sound.files %in% files)])) != length(unique(X$sound.files))) 
      cat(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% files)])), 
                    "sound file(s) not found"))
    
    #count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% files) 
    if (length(d) == 0){
      stop("The sound files are not in the working directory")
    }  else X <- X[d, , drop = FALSE]
    
    files <- files[files %in% X$sound.files]
  }
  
  samp.rate <- sapply(files, function(x) {
    # print(x)
    r <- try(suppressWarnings(warbleR::read_sound_file(X = x, path = path, header = TRUE)), silent = TRUE)
    if (is(r, "try-error")) return (NA) else
      return(r$sample.rate)  
    }) 
  
  if (length(files[is.na(samp.rate)])>0){
    cat("Some file(s) cannot be read")
    return(files[is.na(samp.rate)])
  } else {
    cat("All files can be read\n") 
    if (!is.null(X)) {
      df <- merge(X, data.frame(f = samp.rate, sound.files = names(samp.rate)), by = "sound.files")
      
      cat("smallest number of samples: ", floor(min((df$end - df$start)*df$f)), " (sound file:", as.character(df$sound.files[which.min((df$end - df$start)*df$f)]),"; selection label: ", df$selec[which.min((df$end - df$start)*df$f)], ")\n", sep = "")
    }
  }
  if (length(unique(samp.rate)) > 1)
    cat("Not all sound files have the same sampling rate (potentially problematic, particularly for cross_correlation())")
  }

##############################################################################################################
#' alternative name for \code{\link{check_sound_files}}
#'
#' @keywords internal
#' @details see \code{\link{check_sound_files}} for documentation. \code{\link{check_wavs}} will be deprecated in future versions.
#' @export

check_wavs <- check_sound_files


