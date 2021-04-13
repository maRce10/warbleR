#' Measure the duration of sound files
#' 
#' \code{duration_sound_files} measures the duration of sound files
#' @usage duration_sound_files(files = NULL, path = NULL)
#' @param files Character vector with the names of the sound files to be measured. The sound files should be in the working directory or in the directory provided in 'path'.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @return A data frame with the duration (in seconds) of the sound files.
#' @export
#' @name duration_sound_files
#' @details This function returns the duration (in seconds) of sound files.
#' @examples
#' {
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' 
#' duration_sound_files(path = tempdir())
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) 
#last modification on jul-5-2016 (MAS)

duration_sound_files <- function(files = NULL, path = NULL) { 
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(duration_sound_files)
  
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
  
  a <- sapply(files, function(x) {
    rec <- warbleR::read_sound_file(X = x, path = path, header = TRUE)
    return(rec$samples/rec$sample.rate)  
  })
   return(data.frame(sound.files = files, duration = a, row.names = NULL))

}


##############################################################################################################
#' alternative name for \code{\link{duration_sound_files}}
#'
#' @keywords internal
#' @details see \code{\link{duration_sound_files}} for documentation. \code{\link{wavdur}} will be deprecated in future versions.
#' @export

wavdur <- duration_sound_files


##############################################################################################################
#' alternative name for \code{\link{duration_sound_files}}
#'
#' @keywords internal
#' @details see \code{\link{duration_sound_files}} for documentation. \code{\link{wavdur}} will be deprecated in future versions.
#' @export

wav_dur <- duration_sound_files


##############################################################################################################
#' alternative name for \code{\link{duration_sound_files}}
#'
#' @keywords internal
#' @details see \code{\link{duration_sound_files}} for documentation. \code{\link{duration_wavs}} will be deprecated in future versions.
#' @export

duration_wavs <- duration_sound_files
