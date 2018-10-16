#' Resample wave objects in a extended selection table
#' 
#' \code{resample_est} resample wave objects in a extended selection table.
#' @usage resample_est(X, samp.rate = 44.1, bit.depth = 16,
#' sox = FALSE, avoid.clip = TRUE, pb = FALSE)
#' @param X object of class 'extended_selection_table' (see \code{\link{selection_table}}).
#' @param samp.rate Numeric vector of length 1 with the sampling rate (in kHz) for output files. Default is \code{NULL}.
#' @param bit.depth Numeric vector of length 1 with the dynamic interval (i.e. bit depth) for output files.
#' @param sox Logical to control whether \href{http://sox.sourceforge.net/sox.html}{sox} is used internally for resampling. Sox must be installed. Default is \code{FALSE}. sox is a better if having aliasing issues after resampling.
#' @param avoid.clip Logical to control whether the volumn is automatically 
#' adjusted to avoid clipping high amplitude signals when resampling. Ignored if
#'  '\code{sox = FALSE}. Default is \code{TRUE}.
#' @param pb Logical argument to control progress bar. Default is \code{FALSE}.
#' @return  An extended selection table with the modfied wave objects. 
#' @export
#' @name resample_est
#' @details This function aims to simplify the process of homogenizing sound 
#' files (sampling rate and bit depth). This is a necessary step before running 
#' any further analysis. 
#'   
#' @examples
#' \dontrun{
#' # Set temporary working directory
#' # setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec_table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav") 
#' 
#' # create extended selection table
#' X <- selection_table(X = selec.table, extended = TRUE, confirm.extended = FALSE, pb = FALSE)
#' 
#' # resample
#' Y <- resample_est(X)
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#' #last modification on oct-15-2018 (MAS)

resample_est <- function(X, samp.rate = 44.1, bit.depth = 16, sox = FALSE, avoid.clip = TRUE, pb = FALSE)
{
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))

  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(mp32wav)
  
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
  
  
  if (!sox)
    out <- pbapply::pblapply(attributes(X)$wave.objects, function(x)
  {
  
    if (x@samp.rate != samp.rate * 1000) {
      
      # filter first to avoid aliasing 
      x <- seewave::fir(wave = x , f = x@samp.rate, from = 0, to = samp.rate * 1000 / 2, bandpass = TRUE, output = "Wave")
      
      suppressWarnings(x <- tuneR::downsample(object = x, samp.rate = samp.rate * 1000))
      }
    
    # normalize 
    if (bit.depth != x@bit)
      x <- tuneR::normalize(object = x, unit = as.character(normalize))
    
    return(x)
    
    }) else {
     setwd(tempdir())
      
      out <- pbapply::pblapply(attributes(X)$wave.objects, function(x){
      
        tuneR::writeWave(extensible = FALSE, object = x, filename = "temp.R.file.wav")
   
        cll <- paste0("sox 'temp.R.file.wav' -t wavpcm ", "-b ", bit.depth, " 'temp.R.file2.wav' rate ", samp.rate * 1000, " dither -s") 
        
        if (avoid.clip) cll <- gsub("^sox", "sox -G", cll)
          
        if (Sys.info()[1] == "Windows") cll <- gsub("'", "", cll)
        
        out <- suppressWarnings(system(cll, ignore.stdout = FALSE, intern = TRUE)) 
        
        x <- tuneR::readWave(filename = "temp.R.file2.wav")
        
        return(x)
        })
      unlink(c("temp.R.file.wav", "temp.R.file2.wav"))  
  }   

  # fix attributes
  attributes(X)$check.res$samp.rate <- samp.rate
  attributes(X)$check.res$bits <- bit.depth
  attributes(X)$check.res$n.samples <- sapply(out, function(x) length(x@left)) 
  
  # replace with resampled waves
  attributes(X)$wave.objects <- out
  
  return(X)
}
