#' Convert .wav files to .flac
#' 
#' \code{wav_2_flac} converts several .wav files to .flac compressed lossless format
#' @usage wav_2_flac(files = NULL, path = NULL, overwrite = FALSE, 
#' pb = TRUE, parallel = 1)  
#' @param files character vector with the names of files to be converted. If \code{NULL} all files in the working directory (or 'path' if supplied) are converted.
#' @param path Character string containing the directory path where the .wav files are located.   
#' If \code{NULL} (default) then the current working directory is used. 
#' @param overwrite Logical. Control whether a .flac sound file that is already in the working directory should be 
#' overwritten.
#' @param pb Logical argument to control if progress bar is shown. Default is \code{TRUE}. It can also be
#' set globally using the 'pb' option (see \code{\link{warbleR_options}}).
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing). It can also be
#' set globally using the 'parallel' option (see \code{\link{warbleR_options}}).
#' @return .wav files saved in the working directory with same name as original mp3 files.
#' @export
#' @details The function will convert all mp3 files in  working directory or 'path' supplied to wav format. \href{https://cran.r-project.org/package=bioacoustics}{bioacoustics package} must be installed when changing sampling rates (i.e. if 'samp.rate' is supplied). Note that sound files are normalized using \code{\link[tuneR]{normalize}} so they can be written by \code{\link[tuneR]{writeWave}}.
#' @name wav_2_flac
#' @examples
#' \dontrun{
#' # create some .wav files
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#' 
#' # Convert all files to .flac format
#' wav_2_flac(path = tempdir())
#' 
#' #check this folder!!
#' open_wd(tempdir())
#' }
#' 
#' @details convert all .wav files in working directory to .flac compressed lossless format. It's just a silly wrapper over (\code{\link[seewave]{wav2flac}}) to simplify converting several files at once.
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) 
#last modification on abr-13-2021 (MAS)

wav_2_flac <- function(files = NULL, path = NULL, overwrite = FALSE, pb = TRUE, parallel = 1) {
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(wav_2_flac)
  
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
  
  # get files in path supplied
  files_in_path <- list.files(path = path, pattern = ".wav$", ignore.case = TRUE)
  
  if (is.null(files))
    files <- files_in_path
 else {
      if (!all(files %in% files_in_path))
        stop("some (or all) sound files were not found")
 } 
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  out <- pbapply::pbsapply(X = files, cl = cl, FUN = function(i) 
  
   try_na(seewave::wav2flac(file = file.path(path, i), overwrite = overwrite))
  )
  
}