#' Fix .wav files to allow importing them into R
#' 
#' \code{fixwavs} fixes sound files in .wav format so they can be imported into R.
#' @usage fixwavs(checksels = NULL, files = NULL, samp.rate = NULL, bit.rate = NULL,
#'  path = NULL, ...)
#' @param checksels Data frame with results from \code{\link{checksels}}. 
#' @param files Character vector with the names of the wav files to fix. Default is \code{NULL}.
#' @param samp.rate Numeric vector of length 1 with the sampling rate (in kHz) for output files. Default is \code{NULL}.
#' @param bit.rate Numeric vector of length 1 with the dynamic interval (i.e. bit rate) for output files.
#' Default is \code{NULL}. Currently not available.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param ... Additional arguments to be passed to \code{\link[seewave]{sox}}.
#' @return  A folder inside the working directory (or path provided) all 'converted_sound_files', containing 
#' sound files in a format that can be imported in R. 
#' @export
#' @name fixwavs
#' @details This function aims to simplify the process of converting sound files that cannot be imported into R to 
#' a format that can actually be imported. Problematic files can be determined using \code{\link{checksels}}. The  
#' \code{\link{checksels}} output can be directly input using the argument 'checksels'. Alternatively a vector of file 
#' names to be "fixed" can be provided (argument 'files'). Internally the function calls 'sox' through the 
#' \code{\link[seewave]{sox}} function. 'sox' must be installed to be able to run this function.
#'   
#' @examples
#' \dontrun{
#' # Set temporary working directory
#' # setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav") 
#' 
#' fixwavs(files = selec.table$sound.files)
#' 
#' #check this folder
#' getwd()
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#' #last modification on march-15-2017 (MAS)


fixwavs <- function(checksels = NULL, files = NULL, samp.rate = NULL, bit.rate = NULL, path = NULL, ...)
{

  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(fixwavs)
  
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
  
  #check path to working directory
  if(is.null(path)) path <- getwd() else {if(!file.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  if(is.null(checksels) & is.null(files)) stop("either 'checksels' or 'files' shoud be provided")
  
  if(!is.null(checksels))
  {fls <-unique(checksels$sound.files[checksels$check.res == "Sound file can't be read" | checksels$check.res == "file header corrupted"])
  if(length(fls) == 0) stop("All files were OK according tochecksels")

    #if X is not a data frame
  if(!class(checksels) == "data.frame") stop("checksels is not a data frame")
  
  if(!all(c("sound.files", "check.res") %in% colnames(checksels))) 
    stop(paste(paste(c("sound.files", "check.res")[!(c("sound.files", "check.res") %in% colnames(checksels))], collapse=", "), "column(s) not found in data frame (does not seem to be the output of checksels)"))
  } else fls <- unique(files)

  
  if(length(list.files(pattern = "\\.wav$", ignore.case = TRUE)) == 0) if(is.null(path)) stop("No .wav files in working directory") else stop("No .wav files in 'path' provided") 
  
  if(!is.null(samp.rate)) {
    if(!is.vector(samp.rate)) stop("'samp.rate' must be a numeric vector of length 1") else {
      if(!length(samp.rate) == 1) stop("'samp.rate' must be a numeric vector of length 1")}}  
  
  if(!is.null(bit.rate)) {
    if(!is.vector(bit.rate)) stop("'bit.rate' must be a numeric vector of length 1") else {
      if(!length(bit.rate) == 1) stop("'bit.rate' must be a numeric vector of length 1")}}  
  
    
if(!is.null(samp.rate) & is.null(bit.rate)) bit.rate <- 16

dir.create(file.path(getwd(), "converted_sound_files"))
  
  out <- pbapply::pblapply(fls, function(x)
    {
   
    #name  and path of original file
    # filin <- file.path(getwd(), x)
    
    #name  and path of converted file
    filout <- file.path("converted_sound_files", x)

    if(!is.null(samp.rate))
       out <- system(paste("sox", x, " -t wavpcm", filout, "rate", samp.rate * 1000), ignore.stdout = FALSE, intern = TRUE) else
        out <- system(paste("sox", x, "-t wavpcm", filout), ignore.stdout = FALSE, intern = TRUE) 
     })

  }


##############################################################################################################
#' alternative name for \code{\link{fixwavs}}
#'
#' @keywords internal
#' @details see \code{\link{fixwavs}} for documentation. \code{\link{fixwavs}} will be deprecated in future versions.
#' @export

fix_wavs <- fixwavs
