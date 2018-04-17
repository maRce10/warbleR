#' A wrapper for tuneR's readWave 
#' 
#' \code{read_wave} A wrapper for tuneR's \code{\link[tuneR]{readWave}} function that deals with extension case mismatches.
#' @usage read_wave(filename, from = 0, to = Inf, units = "seconds", header = FALSE,
#' toWaveMC = NULL, ignore.ext.case = TRUE) 
#' @param filename Filename of the file to be read.
#' @param from Where to start reading (in order to save memory by reading wave file piecewise), in units. 
#' @param to Where to stop reading (in order to save memory by reading wave file piecewise), in units. Default is \code{Inf}.
#' @param units	Units in which from and to is given, the default is "seconds".
#' @param header If \code{TRUE}, just header information of the Wave file are returned, otherwise (the default) the whole Wave object.
#' @param toWaveMC If \code{TRUE}, a WaveMC-class object is returned. If \code{NULL}
#' (default) or \code{FALSE} and a non-extensible Wave file or an extensible Wave
#'  file with no other than the 'FL' and 'FR' channels is found, a Wave-class object
#'  is returned, otherwise a WaveMC-class object.
#' @param ignore.ext.case Logical to control if the file extension case is ignored. Default is \code{TRUE}.
#' @return An object of class "Wave".
#' @export
#' @name read_wave
#' @details The function is a wrapper for \code{\link[tuneR]{readWave}} that ignores file extension mismatches, a common mistake when reading wave files.
#' @examples
#' {
#' # First set temporary folder
#' # setwd(tempdir())
#' 
#' # write wave files with lower case file extension
#' data(list = c("Phae.long1"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' 
#' # provide right extension case
#' read_wave("Phae.long1.wav")
#' 
#' # provide wrong extension case
#' read_wave("Phae.long1.WAV")
#' 
#' #' # provide no extension
#' read_wave("Phae.long1")
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on apr-18-2018 (MAS)

read_wave <- function (filename, from = 0, to = Inf, units = "seconds", header = FALSE, toWaveMC = NULL, ignore.ext.case = TRUE) 
{
  if (length(filename) != 1) 
    stop("Please specify exactly one 'filename'.")
  
  if (is.factor(filename)) filename <- as.character(filename) 
  
  # get file extension
  extnt <- substr(x = filename, start = nchar(filename) - 3, stop = nchar(filename))
  
  if(ignore.ext.case)
  {
    # check if is actually a file extension
    if (extnt %in% c(".wav", ".WAV"))  
      no.ext.name <- substr(x = filename, start = 0, stop = nchar(filename) - 4) else
        no.ext.name <- filename
      
      # paste file name and file extension
      if (file.exists(paste0(no.ext.name, ".wav"))) filename <- paste0(no.ext.name, ".wav")  else
        if (file.exists(paste0(no.ext.name, ".WAV"))) filename <- paste0(no.ext.name, ".wav")  
  }
  if (!file.exists(filename)) 
    stop("File '", filename, "' does not exist.")
  
  if (file.access(filename, 4)) 
    stop("No read permission for file ", filename)
  con <- file(filename, "rb")
  on.exit(close(con))
  int <- integer()
  RIFF <- readChar(con, 4)
  file.length <- readBin(con, int, n = 1, size = 4, endian = "little")
  WAVE <- readChar(con, 4)
  i <- 0
  while (!(RIFF == "RIFF" && WAVE == "WAVE")) {
    i <- i + 1
    seek(con, where = file.length - 4, origin = "current")
    RIFF <- readChar(con, 4)
    file.length <- readBin(con, int, n = 1, size = 4, endian = "little")
    WAVE <- readChar(con, 4)
    if (i > 5) 
      stop("This seems not to be a valid RIFF file of type WAVE.")
  }
  FMT <- readChar(con, 4)
  bext <- NULL
  if (header && (tolower(FMT) == "bext")) {
    bext.length <- readBin(con, int, n = 1, size = 4, endian = "little")
    bext <- sapply(seq(bext.length), function(x) readChar(con, 
                                                          1, useBytes = TRUE))
    bext[bext == ""] <- " "
    bext <- paste(bext, collapse = "")
    FMT <- readChar(con, 4)
  }
  i <- 0
  while (FMT != "fmt ") {
    i <- i + 1
    belength <- readBin(con, int, n = 1, size = 4, endian = "little")
    seek(con, where = belength, origin = "current")
    FMT <- readChar(con, 4)
    if (i > 5) 
      stop("There seems to be no 'fmt ' chunk in this Wave (?) file.")
  }
  fmt.length <- readBin(con, int, n = 1, size = 4, endian = "little")
  pcm <- readBin(con, int, n = 1, size = 2, endian = "little", 
                 signed = FALSE)
  if (!(pcm %in% c(0, 1, 3, 65534))) 
    stop("Only uncompressed PCM and IEEE_FLOAT Wave formats supported")
  channels <- readBin(con, int, n = 1, size = 2, endian = "little")
  sample.rate <- readBin(con, int, n = 1, size = 4, endian = "little")
  bytes.second <- readBin(con, int, n = 1, size = 4, endian = "little")
  block.align <- readBin(con, int, n = 1, size = 2, endian = "little")
  bits <- readBin(con, int, n = 1, size = 2, endian = "little")
  if (!(bits %in% c(8, 16, 24, 32, 64))) 
    stop("Only 8-, 16-, 24-, 32- or 64-bit Wave formats supported")
  if (fmt.length >= 18) {
    cbSize <- readBin(con, int, n = 1, size = 2, endian = "little")
    if (cbSize == 22 && fmt.length == 40) {
      validBits <- readBin(con, int, n = 1, size = 2, 
                           endian = "little")
      dwChannelMask <- readBin(con, int, n = 1, size = 4, 
                               endian = "little")
      channelNames <- MCnames[as.logical(intToBits(dwChannelMask)), 
                              "name"]
      SubFormat <- readBin(con, int, n = 1, size = 2, 
                           endian = "little", signed = FALSE)
      x <- readBin(con, "raw", n = 14)
    }
    else {
      if (cbSize > 0) 
        seek(con, where = fmt.length - 18, origin = "current")
    }
  }
  if (exists("SubFormat") && !(SubFormat %in% c(0, 1, 3))) 
    stop("Only uncompressed PCM and IEEE_FLOAT Wave formats supported")
  DATA <- readChar(con, 4)
  i <- 0
  while (length(DATA) && DATA != "data") {
    i <- i + 1
    belength <- readBin(con, int, n = 1, size = 4, endian = "little")
    seek(con, where = belength, origin = "current")
    DATA <- readChar(con, 4)
    if (i > 5) 
      stop("There seems to be no 'data' chunk in this Wave (?) file.")
  }
  if (!length(DATA)) 
    stop("No data chunk found")
  data.length <- readBin(con, int, n = 1, size = 4, endian = "little")
  bytes <- bits/8
  if (((sample.rate * block.align) != bytes.second) || ((channels * 
                                                         bytes) != block.align)) 
    warning("Wave file '", filename, "' seems to be corrupted.")
  if (header) {
    return(c(list(sample.rate = sample.rate, channels = channels, 
                  bits = bits, samples = data.length/(channels * bytes)), 
             if (!is.null(bext)) list(bext = bext)))
  }
  fctr <- switch(match.arg(units), samples = 1, seconds = sample.rate, 
                 minutes = sample.rate * 60, hours = sample.rate * 3600)
  if (fctr > 1) {
    from <- round(from * fctr + 1)
    to <- round(to * fctr)
  }
  N <- data.length/bytes
  N <- min(N, to * channels) - (from * channels + 1 - channels) + 
    1
  seek(con, where = (from - 1) * bytes * channels, origin = "current")
  if (pcm == 3 || (exists("SubFormat") && SubFormat == 3)) {
    sample.data <- readBin(con, "numeric", n = N, size = bytes, 
                           endian = "little")
  }
  else {
    if (bits == 24) {
      sample.data <- readBin(con, int, n = N * bytes, 
                             size = 1, signed = FALSE, endian = "little")
      sample.data <- as.vector(t(matrix(sample.data, nrow = 3)) %*% 
                                 256^(0:2))
      sample.data <- sample.data - 2^24 * (sample.data >= 
                                             2^23)
    }
    else {
      sample.data <- readBin(con, int, n = N, size = bytes, 
                             signed = (bytes != 1), endian = "little")
    }
  }
  toWaveMC <- if (pcm != 65534 || (exists("dwChannelMask") && 
                                   dwChannelMask %in% c(1, 3))) 
    isTRUE(toWaveMC)
  else TRUE
  if (toWaveMC) {
    object <- methods::new("WaveMC", samp.rate = sample.rate, bit = bits, 
                  pcm = !(pcm == 3 || (exists("SubFormat") && SubFormat == 
                                         3)))
    object@.Data <- matrix(sample.data, ncol = channels, 
                           byrow = TRUE)
    if (exists("channelNames")) {
      if ((lcN <- length(channelNames)) < channels) 
        channelNames <- c(channelNames, paste("C", (lcN + 
                                                      1):channels, sep = ""))
      colnames(object@.Data) <- channelNames
    }
  }
  else {
    object <- new("Wave", stereo = (channels == 2), samp.rate = sample.rate, 
                  bit = bits, pcm = !(pcm == 3 || (exists("SubFormat") && 
                                                     SubFormat == 3)))
    if (channels == 2) {
      sample.data <- matrix(sample.data, nrow = 2)
      object@left <- sample.data[1, ]
      object@right <- sample.data[2, ]
    }
    else {
      object@left <- sample.data
    }
  }
  return(object)
}
