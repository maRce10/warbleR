#' Measure sound pressure level
#' 
#' \code{sound_pressure_level} measures sound pressure level in signals reference in a selection table.
#' @usage sound_pressure_level(X, reference = 20, parallel = 1, path = NULL, pb = TRUE, 
#' peak.amplitude = FALSE, wl = 100)
#' @param X object of class 'selection_table', 'extended_selection_table' or any data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end).
#' @param reference Numeric vector of length 1 indicating the pressure (in µPa) to be used as reference. Alternatively, a character vector with the name of a numeric column containing reference values for each row can be supplied. Default is 20 (µPa).
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing). It can also be
#' set globally using the 'parallel' option (see \code{\link{warbleR_options}}).
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used. It can also be
#' set globally using the 'wav.path' option (see \code{\link{warbleR_options}}).
#' @param pb Logical argument to control if progress bar is shown. Default is \code{TRUE}. It can also be
#' set globally using the 'pb' option (see \code{\link{warbleR_options}}).
#' @param peak.amplitude Logical argument controlling if the sound pressure level across the entire signal is return or only that of the highest amplitude (i.e. peak amplitude) of the signal. Default is \code{FALSE}. 
#' @param wl A numeric vector of length 1 specifying the spectrogram window length. Default is 512. 
#' @return The object supplied in 'X' with a new variable 
#' with the sound pressure level values ('SPL' or 'peak.amplitude' column, see argument 'peak.amplitude') in decibels. 
#' @export
#' @name sound_pressure_level
#' @encoding UTF-8
#' @details  Sound pressure level (SPL) is a logarithmic measure of the effective pressure of a sound relative to a reference, so it's a measure of sound intensity. Note that calibrated measures can be obtained only when the SPL of the environment where recordings were made is used as reference.
#'   \code{\link{sig2noise}}.
#' @examples
#' {
#' data(list = c("Phae.long1","lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) #save sound files 
#' 
#' spl <- sound_pressure_level(X = lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ], 
#' parallel = 1, pb = TRUE, path = tempdir())
#' }
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) and Grace Smith Vidaurre
#' @references {Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' \href{https://en.wikipedia.org/wiki/Sound_pressure}{Wikipedia: Sound pressure level}
#' }
#last modification on feb-08-2022 (MAS)

sound_pressure_level <- function(X, reference = 20, parallel = 1, path = NULL, pb = TRUE, peak.amplitude = FALSE, wl = 100){
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(sound_pressure_level)
  
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
    if (!dir.exists(path)) stop("'path' provided does not exist") else
      path <- normalizePath(path)
  
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
  
  # check sound files if not a extended selection table
  if (!is_extended_selection_table(X))
  {
  fs <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)
  if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
    cat(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                  "sound file(s) not found"))
  
  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% fs) 
  if (length(d) == 0){
    stop("The sound files are not in the working directory")
  }  else X <- X[d, , drop = FALSE]
  } else d <- 1:nrow(X)
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # get reference as column (add temporary column)
  if (is.numeric(reference)){
    X$...REFERENCE_TMP <- reference 
    reference <- "...REFERENCE_TMP"
    }
  
  # function to get SPL 
  spl_FUN <- function(X, i, path, reference) {
    
    signal <- read_wave(X, index = i, path = path)
    
    # only if more than 9 samples above twice wl (so it can have at least 2 segments)
    if (!peak.amplitude | peak.amplitude & (length(signal) + 9) <= wl *2)
    sigamp <- seewave::rms(seewave::env(signal, envt = "abs", plot = FALSE)) else {
      # sample cut points 
      cuts <- seq(1, length(signal), by = wl)
      
      # remove last one if few samples (SPL unreliable)
      if (cuts[length(cuts)] - cuts[length(cuts) - 1]  < 10)
            cuts <- cuts[-length(cuts)]
      
      sigamp <- sapply(2:length(cuts), function(e)
        seewave::rms(seewave::env(signal[cuts[e - 1]:cuts[e]], envt = "abs", plot = FALSE))
        )
    }
    
    signaldb <- 20 * log10(sigamp / X[i, reference])

    return(max(signaldb))
  }

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  SPL_l <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(X), cl = cl, FUN = function(i) 
  { 
    spl_FUN(X, i, path, reference)
  }) 
      
  # remove reference column
  X$...REFERENCE_TMP <- NULL
  
    # Add SNR data to X
    z <- data.frame(X, SPL = unlist(SPL_l))
    
    # rename column if peak.ampitude
    if (peak.amplitude)
        names(z)[ncol(z)] <- "peak.amplitude"
      
  # fix extended selection table
    if (is_extended_selection_table(X)) z <- fix_extended_selection_table(X = z, Y = X)  

    return(z)
}
