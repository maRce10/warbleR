#' Interactive view of spectrograms to tailor selections 
#' 
#' \code{read_wave} produces an interactive spectrographic view (similar to \code{\link{manualoc}}) in 
#' which the start/end times and frequency range of acoustic signals listed in a data frame can be adjusted.
#' @usage read_wave(X = NULL, wl = 512, flim = c(0,22), wn = "hanning", mar = 0.5,
#'  osci = TRUE, pal = reverse.gray.colors.2, ovlp = 70, auto.next = FALSE, pause = 1,
#'   comments = TRUE, path = NULL, frange = FALSE, fast.spec = FALSE, ext.window = TRUE,
#'   width = 15, height = 5, index = NULL, collevels = NULL, 
#'   title = c("sound.files", "selec"), ts.df = NULL, col = "#E37222", 
#'   alpha = 0.7, auto.contour = FALSE, ...)
#' @param X 'selection.table' object or data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input data frame. Other data frames can be used as input, but must have at least the 4 columns mentioned above. Notice that, if an output file ("read_wave_output.csv") is found in the working directory it will be given priority over an input data frame.
#' @param wl A numeric vector of length 1 specifying the spectrogram window length. Default is 512.
#' @param flim A numeric vector of length 2 specifying the frequency limit (in kHz) of 
#'   the spectrogram, as in the function \code{\link[seewave]{spectro}}. 
#'   Default is c(0,22).
#' @param wn A character vector of length 1 specifying the window function (by default "hanning"). 
#' See function \code{\link[seewave]{ftwindow}} for more options.
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the 
#' start and end points of the selections to define spectrogram limits. Default is 0.5.
#' @param osci Logical argument. If \code{TRUE} adds a oscillogram whenever the spectrograms are produced 
#'   with higher resolution (see seltime). Default is \code{TRUE}.
#'   The external program must be closed before resuming analysis. Default is \code{NULL}.
#' @param pal A color palette function to be used to assign colors in the 
#'   plot, as in \code{\link[seewave]{spectro}}. Default is reverse.gray.colors.2. See Details.
#' @param ovlp Numeric vector of length 1 specifying the percent overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70.
#' @param auto.next Logical argument to control whether the functions moves automatically to the 
#' next selection. The time interval before moving to the next selection is controled by the 'pause' argument. Ignored if \code{ts.df = TRUE}. 
#' @param pause Numeric vector of length 1. Controls the duration of the waiting period before 
#' moving to the next selection (in seconds). Default is 1. 
#' @param comments Logical argument specifying if 'sel.comment' (when in data frame) should be included 
#' in the title of the spectrograms. Default is \code{TRUE}.
#' @param path Character string containing the directory path where the sound files are located.
#' @param frange Logical argument specifying whether limits on frequency range should be
#'  recorded. 
#' If \code{NULL} (default) then only the time limits are recorded.
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast' spectograms. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}} offer 
#' decreasing darkness levels. 
#' @param ext.window Logical. If \code{TRUE} then and external graphic window is used. Default 
#' dimensions can be set using the 'width' and 'height' arguments. Default is \code{TRUE}.
#' @param width Numeric of length 1 controling the width of the external graphic window. Ignored
#' if \code{ext.window = FALSE}. Default is 15.
#' @param height Numeric of length 1 controling the height of the external graphic window.
#' Ignored if \code{ext.window = FALSE}. Default is 5.
#' @param index Numeric vector indicating which selections (rows) of 'X' should be tailored. 
#'  Default is \code{NULL}. Ignored when the process is resumed. This can be useful when combined
#'  with \code{\link{filtersels}}) output (see 'index' argument in \code{\link{filtersels}}).
#' @param collevels Numeric. Set of levels used to partition the amplitude range (see 
#'  \code{\link[seewave]{spectro}}).
#' @param title Character vector with the names of the columns to be included in the title for each
#' selection.
#' @param ts.df Optional. Data frame with frequency contour time series of signals to be tailored. If provided then 
#' 'autonext' is set to \code{FALSE}. Default is \code{NULL}. The data frame must include the 'sound.files' and 'selec' 
#' columns for the same selections included in 'X'.
#' @param col Character vector defining the color of the points when 'ts.df' is provided. Default is "#E37222" (orange).
#' @param alpha Numeric of length one to adjust transparency of points when adjusting frequency contours.
#' @param auto.contour Logical. If \code{TRUE} contours are displayed automatically
#' (without having to click on 'contour'). Note that adjusting the selection box 
#' (frequency/time limits) won't be available. Default is \code{FALSE}. Ignored if
#' 'ts.df' is not provided. 
#' @param ... Additional arguments to be passed to the internal spectrogram creating function for customizing graphical output. The function is a modified version of \code{\link[seewave]{spectro}}, so it takes the same arguments. 
#' @return data frame similar to X with the and a .csv file saved in the working directory with start and end time of 
#'   selections.
#' @export
#' @name read_wave
#' @examples
#' \dontrun{
#' #Set temporary working directory
#' # setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' read_wave(X =  selec.table, flim = c(1,12), wl = 300, auto.next = TRUE)
#' 
#' # Read output .csv file
#' read_wave.df <- read.csv("read_wave_output.csv")
#' read_wave.df
#' 
#' # check this directory for .csv file after stopping function
#' getwd()
#' }
#' @details This function produces an interactive spectrographic
#'  view in which users can select new time/frequency 
#'  coordinates the selections. 4 "buttons" are provided at the upper right side of the spectrogram that
#'   allow to stop the analysis ("stop"), go to the next sound file ("next"), return to the 
#'   previous selection ("previous") or delete 
#'   the current selection ("delete"). An additional "button" ("contour") to tailored frequency contour is shown
#'   when 'ts.df' is provided. When a unit has been selected, the function plots 
#'   dotted lines in the start and end of the selection in the spectrogram (or a box if 
#'   \code{frange = TRUE}). Only the last selection is kept for each
#'    selection that is adjusted. The function produces a .csv file (read_wave_output.csv) 
#'    with the same information than the input data frame, except for the new time 
#'    coordinates, plus a new column (X$tailored) indicating if the selection 
#'   has been tailored. The file is saved in the working directory  and is updated every time the user
#'    moves into the next sound file (next sel "button") or stop the process 
#'  (Stop "button"). It also return the same data frame as and object in the R environment.
#'    If no selection is made (by clicking on the 'next' button) the 
#'  original time/frequency coordinates are kept. When resuming the process (after "stop" and re-running 
#'  the function in the same working directory), the function will continue working on the
#'  selections that have not been analyzed. The function also displays a progress bar right on
#'  top of the sepctrogram. The zoom can be adjusted by setting the \code{mar} argument.
#'  To fix contours a data.frame containing the 'sound.files' and 'selec' columns as in 'X' as well 
#'  as the frequency values at each contour step must be provided. The function plots points correponding to the 
#'  time/frequency coordinates of each element of the contour. Cliking on the spectrogram will substitute the 
#'  frequency value of the points. The contour point closest in time to the "click" will be replaced by the 
#'  frequency value of the "click". 
#'  
#' @seealso  \code{\link{manualoc}}
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on jul-5-2016 (MAS)

read_wave <- function (filename, from = 1, to = Inf, units = "seconds", header = FALSE, toWaveMC = NULL, ignore.ext.case = TRUE) 
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
    object <- new("WaveMC", samp.rate = sample.rate, bit = bits, 
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
