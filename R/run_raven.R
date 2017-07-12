#' Open sound files in Raven sound analysis software 
#' 
#' \code{run_raven} opens several sound files in Raven sound analysis software
#' @usage run_raven(raven.path = NULL, sound.files = NULL, path = NULL, at.the.time = 5,
#' import = FALSE, ...)  
#' @param raven.path A character string indicating the path of the directory in which to look for the raven executable file (where Raven was installed). 
#' @param sound.files character vector indicating the files that will be analyzed. If  \code{NULL} (default) then Raven will be run without opening any file.
#' @param path A character string indicating the path of the directory in which to look for
#' the sound files. If not provided (default) the function searches into the current working 
#' directory. Default is \code{NULL}.
#' @param at.the.time Numeric vector of length 1 controling how many files will be open in
#'  Raven at the same time. Note that opening too many files at once could make Raven run out
#'  of memory. You need to close Raven every time the batch of files is analyzed, so the next
#'  batch is opened.
#' @param import Logical. Controls if the selection tables generated should be return as a 
#' data frame into the R environment. This only works if the selections are saved in the 
#' "Selections" folder in the Raven directory. This argument calls the \code{\link{imp.raven}}
#' internally. Additional arguments can be passed to \code{\link{imp.raven}} to control the way the data is imported.
#' @param ... Additional arguments to be passed to \code{\link{imp.raven}} for customizing
#' how selections are imported (ignored if \code{import = FALSE}).
#' @return If \code{import = TRUE} a data frame with the selections produced during the analysis will be return as an data frame. See \code{\link{imp.raven}} for more details on how selections are imported.
#' @details The function runs Raven interactive analysis software (Cornell Lab of Ornithology), opening many files simultaneously. Raven will still run if no sound files are provided (i.e. \code{sound.files = NULL}). At the end of the analysis the data can be automatically imported back into R using the 'import' argument. 
#' @seealso \code{\link{imp.raven}} \code{\link{imp.syrinx}} 
#' @export
#' @name run_raven
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#'# save sound files 
#' data(list = c("Phae.long1", "Phae.long2"))
#' writeWave(Phae.long1, "Phae.long1.wav", extensible = FALSE)
#' writeWave(Phae.long2, "Phae.long2.wav", extensible = FALSE)
#' 
#' raven.path <- "PATH_TO_RAVEN_DIRECTORY_HERE" 
#' 
#' # run function 
#' run_raven(raven.path = raven.path, sound.files = c("Phae.long1.wav", "Phae.long2.wav"),
#'  at.the.time = 2, import = T, name.from.file = T, ext.case = "upper", all.data = T)  
#'  
#' #getting all the data
#' rav.dat<-run_raven(all.data = TRUE)
#' View(rav.dat)
#' 
#' # run function on all the wav files in the working directory 
#' run_raven(raven.path = raven.path, sound.files = list.files(pattern = "\.wav$", 
#' ignore.case = TRUE), at.the.time = 4, import = FALSE)
#'   
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-30-2017 (MAS)
run_raven <- function(raven.path = NULL, sound.files = NULL, path = NULL, at.the.time = 5,
                      import = FALSE, ...)
  {
  
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
  if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else 
    setwd(path)}  else path <- getwd() #set working directory
  
  if(is.null(raven.path))
    stop("Path to 'Raven' folder must be provided")  else
      if(class(try(setwd(raven.path), silent = TRUE)) == "try-error") stop("'raven.path' provided does not exist")
    
    
if(is.null(sound.files))
{
  if(Sys.info()[1] == "Windows")
    system(shQuote(file.path(raven.path, "Raven"), type = "cmd"), ignore.stderr = TRUE) else
      system(file.path(raven.path, "Raven"), ignore.stderr = TRUE)
} else {
  sound.files <- as.character(sound.files)
  if(!is.vector(sound.files))
    stop("'sound.files' must be a character vector")
  
  sf <- sound.files
  
  #return warning if not all sound files were found
  recs.wd <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)
  
  #count number of sound files in working directory and if 0 stop
  sound.files <- sound.files[sound.files %in% recs.wd]
  if(length(sound.files) == 0)
    stop("The .wav files are not in the working directory")
  
  # remove sound files not found
  if(length(sound.files) != length(sf)) 
   cat(paste(length(sf) - length(sound.files), ".wav file(s) not found"))
   
    # check if sound file names contains directory and fix
    if(basename(sound.files[1]) == sound.files[1])
    sound.files <- file.path(path, sound.files)

  # subset by groups of sound files according to at the time
  sq <- unique(c(seq(1, length(sound.files), by = at.the.time)))
  
  # run loop over files
  out <- pbapply::pblapply(sq, function(x)
    {
    fls <- sound.files[x:(x + at.the.time - 1)]
    fls <- fls[!is.na(fls)]
    
    fls <- paste(fls, collapse = " ")
    
    if(Sys.info()[1] == "Windows")
      comnd <- paste(shQuote(file.path(raven.path, "Raven"), type = "cmd"), fls) else
        comnd <- paste(file.path(raven.path, "Raven"), fls)
  
    # set working directory in Linux
    if(Sys.info()[1] == "Linux")     system(command = paste("cd", raven.path), ignore.stderr = TRUE)
  
    # run raven
    system(command = comnd, ignore.stderr = TRUE)
    }
    )
}
  
  if(import){
    sels <- imp.raven(path = file.path(raven.path, "Selections"), ...)
    
  if(any(names(sels) %in% "sound.files") & !is.null(sound.files)) sels <- sels[sels$sound.files %in% basename(sound.files), ]

  return(sels)
  }  

  # reset working directory   
   try(setwd(wd), silent = TRUE) 
   
}
