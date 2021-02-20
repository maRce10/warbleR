#' Check .wav files
#' 
#' \code{wav_check} checks whether .wav files can be read by subsequent functions.
#' @usage wav_check(X = NULL, path = NULL)
#' @param X Optional. 'selection_table' object or data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "sel": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The output of \code{\link{auto_detec}} can 
#' also be used as the input data frame. If provided the function also returns the
#' smallest number of samples from the listed selections, which limits the minimum window 
#' length (wl argument in other functions) that can be used in batch analyses. 
#' This could be useful for avoiding errors in downstream functions (e.g. \code{\link{spectro_analysis}}).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.  
#' @return If all .wav files are ok, returns message "All files can be read".
#'   Otherwise returns the names of the corrupted .wav files.
#' @details This function checks if .wav files in the working directory can be read.
#' Users must set the working directory where they wish to check .wav files beforehand. 
#' If X is provided it also returns the smallest number of samples from
#' the selections listed in X (if all files can be read). Note that corrupt files can be
#' fixed using \code{\link{wav_fix}}) ('sox' must be installed to be able to run this function).
#' The function is intended for a "quick and dirty" check of the .wav files in a selections data
#'  frame. For a more thorough analysis see \code{\link{sel_check}}.
#' @export
#' @seealso \code{\link{sel_check}} \code{\link{sel_tailor}}
#' @name wav_check
#' @examples{
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#' 
#' # without selection data frame
#' wav_check(path = tempdir())
#' 
#' # without selection data frame
#' wav_check(X = lbh_selec_table, path = tempdir())
#' }
#' @references {Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on jul-5-2016 (MAS)

wav_check <- function(X = NULL, path = NULL) { 
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(wav_check)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
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
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) 
      stop("'path' provided does not exist") else
        path <- normalizePath(path) 
  
  #return warning if not all sound files were found
  files <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)
  if (length(files) == 0) stop("no .wav files in working directory") 
  
  
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
                    ".wav file(s) not found"))
    
    #count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% files) 
    if (length(d) == 0){
      stop("The .wav files are not in the working directory")
    }  else X <- X[d, , drop = FALSE]
    
    files <- files[files %in% X$sound.files]
  }
  
  a <- sapply(files, function(x) {
    r <- try(suppressWarnings(warbleR::read_wave(X = x, path = path, header = TRUE)), silent = TRUE)
    if (is(r, "try-error")) return (NA) else
      return(r$sample.rate)  
    }) 
  
  if (length(files[is.na(a)])>0){
    cat("Some file(s) cannot be read")
    return(files[is.na(a)])
  } else {
    cat("All files can be read") 
    if (!is.null(X)) {
      df <- merge(X, data.frame(f = a, sound.files = names(a)), by = "sound.files")
      
      cat("smallest number of samples: ", floor(min((df$end - df$start)*df$f)), " (sound file:", as.character(df$sound.files[which.min((df$end - df$start)*df$f)]),"; selection label: ", df$selec[which.min((df$end - df$start)*df$f)], ")", sep = "")
    }
  }
}

##############################################################################################################
#' alternative name for \code{\link{wav_check}}
#'
#' @keywords internal
#' @details see \code{\link{wav_check}} for documentation. \code{\link{check_wavs}} will be deprecated in future versions.
#' @export

check_wavs <- wav_check

##############################################################################################################
#' alternative name for \code{\link{wav_check}}
#'
#' @keywords internal
#' @details see \code{\link{wav_check}} for documentation. \code{\link{checkwavs}} will be deprecated in future versions.
#' @export

checkwavs <- wav_check
