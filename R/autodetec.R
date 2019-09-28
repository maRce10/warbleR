#' Automatically detect vocalizations in sound files
#' 
#' \code{autodetec} automatically detects the start and end of vocalizations in sound files based
#' on amplitude, duration, and frequency range attributes.
#' @usage autodetec(X = NULL, threshold = 15, envt = "abs", ssmooth = NULL, msmooth = NULL, 
#'   power = 1, bp = NULL, osci = FALSE, wl = 512, xl = 1, picsize = 1, res = 100, 
#'   flim = c(0,22), ls = FALSE, sxrow = 10, rows = 10, mindur = NULL, maxdur = 
#'   NULL, redo = FALSE, img = TRUE, it = "jpeg", set = FALSE, flist = NULL, smadj = NULL,
#'   parallel = 1, path = NULL, pb = TRUE, pal = reverse.gray.colors.2, 
#'   fast.spec = FALSE, ...)
#' @param X 'selection_table' object or a data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). If provided the detection will be conducted only within 
#' the selections in 'X'.
#' @param threshold A numeric vector of length 1 specifying the amplitude threshold for detecting 
#'   signals (in \%).
#' @param envt Character vector of length 1 specifying the type of envelope to
#'   be used: "abs" for absolute amplitude envelope or "hil" for Hilbert 
#'   amplitude envelope. Default is "abs".
#' @param ssmooth A numeric vector of length 1 to smooth the amplitude envelope 
#'   with a sum smooth function. Default is NULL.
#' @param msmooth A numeric vector of length 2 to smooth the amplitude envelope 
#'   with a mean sliding window. The first component is the window length and 
#'   the second is the overlap between successive windows (in \%). Faster than ssmooth but time detection is 
#'   much less accurate. Will be deprecated in future versions. Default is NULL.
#' @param power A numeric vector of length 1 indicating a power factor applied 
#'   to the amplitude envelope. Increasing power will reduce low amplitude 
#'   modulations and increase high amplide modulations, in order to reduce 
#'   background noise. Default is 1 (no change).
#' @param bp Numeric vector of length 2 giving the lower and upper limits of a
#'   frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param osci Logical argument to add an oscillogram underneath spectrogram, as
#'   in \code{\link[seewave]{spectro}}. Default is \code{FALSE}. Not applied if ls is 
#'   \code{TRUE}.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param xl Numeric vector of length 1, a constant by which to scale 
#'   spectrogram width. Default is 1.
#' @param picsize Numeric argument of length 1. Controls the relative size of 
#'   the spectrogram. Default is 1.
#' @param res Numeric argument of length 1 controling resolution of images.
#'   Default is 100 (faster) although 300 - 400 is recommended for publication/ 
#'   presentation quality.
#' @param flim A numeric vector of length 2 for the frequency limit in kHz of 
#'   the spectrogram, as in \code{\link[seewave]{spectro}}. Default is c(0, 22).
#' @param ls Logical argument. If \code{TRUE}, long spectrograms as in \code{\link{lspec}} 
#'   are produced.
#' @param sxrow A numeric vector of length 1. Specifies seconds of spectrogram
#'   per row when creating long spectrograms. Default is 10. Applied when ls =
#'   \code{TRUE} and/or when X is not provided.
#' @param rows A numeric vector of length 1. Specifies number of rows per 
#'   image file when creating long spectrograms. Default is 10. Applied when ls =  
#'   \code{TRUE} and/or when X is not provided.
#' @param mindur Numeric vector of length 1 giving the shortest duration (in 
#'   seconds) of the signals to be detected. It removes signals below that 
#'   threshold.
#' @param maxdur Numeric vector of length 1 giving the longest duration (in 
#'   seconds) of the signals to be detected. It removes signals above that 
#'   threshold.
#' @param redo Logical argument. If \code{TRUE} all selections will be analyzed again 
#'   when code is rerun. If \code{FALSE} only the selections that do not have an 'autodetec' generated image 
#'   file in the working directory will be analyzed. Default is \code{FALSE}.
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
#' @param it A character vector of length 1  giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param set A logical argument indicating wheter the settings of the autodetection 
#'  process should be included in the image file name. If \code{TRUE}, threshold (th), envelope (envt), bandpass (bp),
#'  power (pw), smooth (smo, either mmsooth[1] or ssmooth), maxdur (mxdu), and mindur (midu) are included. 
#' @param flist character vector or factor indicating the subset of files that will be analyzed. Ignored
#' if X is provided.
#' @param smadj adjustment for amplitude smoothing. Character vector of length one indicating whether start end 
#' values should be adjusted. "start", "end" or "both" are the inputs admitted by this argument. Amplitude 
#' smoothing through ssmooth generates a predictable deviation from the actual start and end positions of the signals,
#' determined by the threshold and ssmooth values. This deviation is more obvious (and problematic) when the 
#' increase and decrease in amplitude at the start and end of the signal (respectively) is not gradual. Ignored if ssmooth is \code{NULL}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param pal Color palette function for spectrogram. Default is reverse.gray.colors.2. See 
#' \code{\link[seewave]{spectro}} for more palettes. Palettes as \code{\link[monitoR]{gray.2}} may work better when \code{fast.spec = TRUE}.
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast.spec' spectograms. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}} offer 
#' decreasing darkness levels. 
#' @param ... Additional arguments to be passed internally \code{\link{specreator}} for customizing
#' graphical output.
#' @return Image files with spectrograms showing the start and end of the detected signals. It 
#'   also returns a data frame containing the start and end of each signal by 
#'   sound file and selection number.
#' @export
#' @name autodetec
#' @details This function determines the start and end of signals in the sound file selections listed 
#'   in the input data frame ('X'). Alternatively, if no data frame is provided, the function detects signals across
#'   each entire sound file. It can also create long spectrograms highlighting the start and of the detected
#'   signals for all sound files in the working directory (if \code{img = TRUE}). Sound files should be located in the
#'    working directory or the path to the sound files should be provided using the 'path' argument. The input 
#'    data frame should have the following columns: c("sound.files","selec","start","end"). The ouptut of 
#'    \code{\link{manualoc}} can be used as the input data frame. This function uses a modified version of the 
#'    \code{\link[seewave]{timer}} function from seewave package to detect signals. 
#'   
#' @examples
#' \dontrun{
#' # Save to temporary working directory
#' 
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav")) 
#' 
#' ad <- autodetec(threshold = 5, env = "hil", ssmooth = 300, power=1,
#' bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE,
#' wl = 300, ls = FALSE, sxrow = 2, rows = 4, mindur = 0.1, maxdur = 1, set = TRUE, path = tempdir())
#' 
#' #run it with different settings
#' ad <- autodetec(threshold = 90, env = "abs", ssmooth = 300, power = 1, redo = TRUE,
#' bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE, 
#' wl = 300, ls = FALSE,  sxrow = 2, rows = 4, mindur=0.1, maxdur=1, set = TRUE, path = tempdir())
#' 
#' #check this folder!!
#' tempdir()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com}). Implements a
#' modified version of the timer function from seewave. 
#last modification on jul-5-2016 (MAS)

autodetec <- function(X= NULL, threshold=15, envt="abs", ssmooth = NULL, msmooth = NULL, power = 1, 
                      bp = NULL, osci = FALSE, wl = 512, xl = 1, picsize = 1, res = 100, flim = c(0,22), 
                      ls = FALSE, sxrow = 10, rows = 10, mindur = NULL, maxdur = NULL, redo = FALSE, 
                      img = TRUE, it = "jpeg", set = FALSE, flist = NULL, smadj = NULL, parallel = 1, 
                      path = NULL, pb = TRUE, pal = reverse.gray.colors.2,
                      fast.spec = FALSE, ...){
  
  
  # reset working directory 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type))
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(autodetec)
  
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
  
  #check path if not provided set to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) stop("'path' provided does not exist") 
  
  #if files not found
  if (length(list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)) == 0) if (is.null(path)) stop("No .wav files in working directory") else stop("No .wav files found") 
  
  #if bp is not vector or length!=2 stop
  if (!is.null(bp))
  {if (!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if (!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}}    
  
  #if flim is not vector or length!=2 stop
  if (is.null(flim)) stop("'flim' must be a numeric vector of length 2") else {
    if (!is.vector(flim)) stop("'flim' must be a numeric vector of length 2") else{
      if (!length(flim) == 2) stop("'flim' must be a numeric vector of length 2")}}   
  
  #if msmooth is not vector or length!=2 stop
  if (!is.null(msmooth)) {
    if (!is.vector(msmooth)) stop("'msmooth' must be a numeric vector of length 2") else {
      if (!length(msmooth) == 2) stop("'msmooth' must be a numeric vector of length 2")}}   
  
  #if ssmooth is not vector or length!=1 stop
  if (!is.null(ssmooth)) {
    if (!is.vector(ssmooth)) stop("'ssmooth' must be a numeric vector of length 1") else {
      if (!length(ssmooth) == 1) stop("'ssmooth' must be a numeric vector of length 1")}}   
  
  #if wl is not vector or length!=1 stop
  if (is.null(wl)) stop("'wl' must be a numeric vector of length 1") else {
    if (!is.vector(wl)) stop("'wl' must be a numeric vector of length 1") else{
      if (!length(wl) == 1) stop("'wl' must be a numeric vector of length 1")}}  
  
  #if sxrow is not vector or length!=1 stop
  if (is.null(sxrow)) stop("'sxrow' must be a numeric vector of length 1") else {
    if (!is.vector(sxrow)) stop("'sxrow' must be a numeric vector of length 1") else{
      if (!length(sxrow) == 1) stop("'sxrow' must be a numeric vector of length 1")}}  
  
  #if rows is not vector or length!=1 stop
  if (is.null(rows)) stop("'rows' must be a numeric vector of length 1") else {
    if (!is.vector(rows)) stop("'rows' must be a numeric vector of length 1") else{
      if (!length(rows) == 1) stop("'rows' must be a numeric vector of length 1")}}  
  
  #if picsize is not vector or length!=1 stop
  if (is.null(picsize)) stop("'picsize' must be a numeric vector of length 1") else {
    if (!is.vector(picsize)) stop("'picsize' must be a numeric vector of length 1") else{
      if (!length(picsize) == 1) stop("'picsize' must be a numeric vector of length 1")}}  
  
  #if xl is not vector or length!=1 stop
  if (is.null(xl)) stop("'xl' must be a numeric vector of length 1") else {
    if (!is.vector(xl)) stop("'xl' must be a numeric vector of length 1") else{
      if (!length(xl) == 1) stop("'xl' must be a numeric vector of length 1")}}  
  
  #if res is not vector or length!=1 stop
  if (is.null(res)) stop("'res' must be a numeric vector of length 1") else {
    if (!is.vector(res)) stop("'res' must be a numeric vector of length 1") else{
      if (!length(res) == 1) stop("'res' must be a numeric vector of length 1")}}  
  
  #if threshold is not vector or length!=1 stop
  if (is.null(threshold))  stop("'threshold' must be a numeric vector of length 1") else {
    if (!is.vector(threshold)) stop("'threshold' must be a numeric vector of length 1") else{
      if (!length(threshold) == 1) stop("'threshold' must be a numeric vector of length 1")}}  
  
  #if flist is not character vector
  if (!is.null(flist) & is.null(X) & any(!is.character(flist), !is.vector(flist))) stop("'flist' must be a character vector") 
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  #if it argument is not "jpeg" or "tiff" 
  if (!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))
  
  #if envt is not vector or length!=1 stop
  if (any(envt %in% c("abs", "hil"))){if (!length(envt) == 1) stop("'envt' must be a numeric vector of length 1")
  } else stop("'envt' must be either 'abs' or 'hil'" )
  
  if (any(!sapply(list(osci,ls, redo),is.logical))) 
    stop(paste(paste(c("osci","ls","redo")[!sapply(list(osci,ls, redo),is.logical)],collapse = " "),"not logical"))
  
  #stop if power is 0
  if (power == 0) 
    stop("'power' cannot equal to 0")
  
  if (!is.null(msmooth)) smo <- msmooth[1] else {if (!is.null(ssmooth)) smo <- ssmooth else smo <- 0}
  
  #if smadj argument is not "start" "end" or "both"
  if (!is.null(smadj)) if (!any(smadj == "start", smadj == "end", smadj == "both")) 
    stop(paste("smooth adjustment", smadj, "not allowed"))  
  
  if (!is.null(X)){
    
    #if X is not a data frame
    if (!any(is.data.frame(X), is_selection_table(X))) stop("X is not of a class 'data.frame' or 'selection_table'")
    
    #check if all columns are found
    if (any(!(c("sound.files", "selec", "start", "end") %in% colnames(X)))) 
      stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                     "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
    
    #if there are NAs in start or end stop
    if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end columns")  
    
    #if end or start are not numeric stop
    if (all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'start' and 'end' must be numeric")
    
    #if any start higher than end stop
    if (any(X$end - X$start <= 0)) stop(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))  
    
    #return warning if not all sound files were found
    fs <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)
    if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
      cat(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                ".wav file(s) not found"))
    
    #count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% fs) 
    if (length(d) == 0) stop("The .wav files are not in the working directory") else X <- X[d,]  
    xprov <- T #to replace X if not provided
  } else  { 
    if (!is.null(flist)) X <- warbleR::wavdur(files = flist, path = path) else
      X <- warbleR::wavdur(path = path)
    X$start <- 0
    X$selec <- 1
    names(X)[2] <- "end"  
    xprov <- F #to replace X if not provided
    if (nrow(X) == 0) stop("Files in 'flist' not in working directory")
  }
  
  #redo the ones that have no images in folder
  if (!redo) {
    imgfs <- list.files(path = path, pattern = "\\.jpeg$|\\.tiff$")
    done <- sapply(1:nrow(X), function(x){
      any(grep(paste(gsub(".wav","", X$sound.files[x]),X$selec[x], sep = "-"), imgfs,  invert = FALSE))
    })
    X <- X[!done, ]
    if (nrow(X) == 0) stop("All selections have been analyzed (redo = FALSE)")
  }    
  
  # if parallel was not called 
  if (pb) if (!ls & img) cat("Detecting signals in sound files and producing spectrogram:") else 
    cat("Detecting signals in sound files:")
  
  #create function to detec signals          
  adFUN <- function(i, X, flim, wl, bp, envt, msmooth, ssmooth, mindur, maxdur)
  {
    song <- warbleR::read_wave(X = X, path = path, index = i)
    
    if (length(song@left) > wl + 2)
    { 
      f <- song@samp.rate
      fl<- flim #in case flim is higher than can be due to sampling rate
      if (fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
      
      #filter frequnecies below 1000 Hz
      if (!is.null(bp))
        f.song<-seewave::ffilter(song, f=f, from = bp[1]*1000, to = bp[2]*1000, bandpass = TRUE, wl = wl, output="Wave") else
          f.song<-song
      
      #detect songs based on amplitude (modified from seewave::timer function)
      input <- seewave::inputw(wave = f.song, f = f)
      wave <- input$w
      f <- input$f
      rm(input)
      n <- length(wave)
      thres <- threshold/100
      wave1 <- seewave::env(wave = wave, f = f, msmooth = msmooth, ssmooth = ssmooth,  
                            envt = envt, norm = TRUE, plot = FALSE)
      
      n1 <- length(wave1)
      f1 <- f * (n1/n)
      if (power != 1) 
        wave1 <- wave1^power
      wave2 <- ifelse(wave1 <= thres, yes = 1, no = 2)
      n2 <- length(wave2)
      wave4 <- apply(as.matrix(1:(n2 - 1)), 1, function(x) wave2[x] + 
                       wave2[x + 1])
      n4 <- length(wave4)
      wave4[c(1, n4)] <- 3
      wave5 <- which(wave4 == 3)
      wave5[-1] <- wave5[-1] + 1
      f4 <- f * (n4/n)
      wave4 <- ts(wave4, start = 0, end = n4/f4, frequency = f4)
      positions <- time(wave4)[wave5]
      npos <- length(positions)
      durations <- apply(as.matrix(1:(npos - 1)), 1, function(x) positions[x + 
                                                                             1] - positions[x])
      if (wave2[1] == 1 & npos > 2) {
        signal <- durations[seq(2, npos - 1, by = 2)]
        start.signal <- positions[seq(2, npos - 1, by = 2)]
      }  else {
        signal <- durations[seq(1, npos - 1, by = 2)]
        start.signal <- positions[seq(1, npos - 1, by = 2)]
      }
      aut.det <- list(s = signal, s.start = start.signal)
      
      #put time of detection in data frame
      time.song <- data.frame(sound.files = X$sound.files[i], duration = aut.det$s, selec = NA, start = aut.det$s.start+X$start[i], end = (aut.det$s+aut.det$s.start+X$start[i]))
      
      #remove signals based on duration  
      if (!is.null(mindur)) time.song <-time.song[time.song$duration > mindur,]
      if (!is.null(maxdur)) time.song <-time.song[time.song$duration < maxdur,]
      
      if (nrow(time.song) > 0) 
      {if (xprov) time.song$selec <- paste(X$selec[i], 1:nrow(time.song), sep = "-") else
        time.song$selec <- 1:nrow(time.song)}
      
      #if nothing was detected
      if (nrow(time.song)==0)
        time.song <- data.frame(sound.files = X$sound.files[i], duration = NA,selec = NA,start = NA, end = NA) 
      
      time.song1 <- time.song
      
      time.song$start[is.na(time.song$start)] <- -2
      time.song$start[is.na(time.song$start)] <- -1
      
      
      if (!ls & img & nrow(time.song) > 0) {
        if (set) 
          fna <- paste(substring(X$sound.files[i], first = 1, last = nchar(as.character(X$sound.files[i]))-4),
                       "-", X$selec[i], "-autodetec","-th" ,threshold , "-env.", envt,"-bp", bp[1],".",bp[2], "-smo", smo, "-midu", mindur,
                       "-mxdu", maxdur, "-pw", power, sep = "") else
                         fna <- paste(substring(X$sound.files[i], first = 1, last = nchar(as.character(X$sound.files[i]))-4),
                                      "-", X$selec[i], "-autodetec", sep = "")  
                       
                       img_wrlbr_int(filename = paste(fna, paste0(".", it), sep = "-"), path = path, res = res, units = "cm", width = (10.16) * xl * picsize, height = (10.16) * picsize)
                       
                       spectro_wrblr_int(song, f = f, wl = wl, collevels=seq(-45,0,1),grid = FALSE, main = as.character(X$sound.files[i]), osc = osci,  colwave = "#07889B", fast.spec = fast.spec,
                                         scale = FALSE, palette = pal, flim = fl, ...)
                       rm(song)
                       if (nrow(time.song)>0)
                       {sapply(1:nrow(time.song), function(j)  abline(v = c(time.song$start[j]-X$start[i], time.song$end[j] - X$start[i]),col = adjustcolor("#E37222", alpha.f = 0.7), lwd = 2, lty = "dotted"))
                         
                         sapply(1:nrow(time.song), function(j)  text(time.song$start[j]+time.song$duration[j]/2-X$start[i],
                                                                     rep(c(((fl[2]-fl[1])*0.85)+fl[1],((fl[2]-fl[1])*0.9)+fl[1],((fl[2]-fl[1])*0.95)+fl[1]),
                                                                         nrow(time.song))[j],paste(X$selec[i], j, sep = "-"),cex=1))} 
                       
                       
                       
                       dev.off()
      }  
    }
    
    
    #remove duration column
    time.song1 <- time.song1[,grep("duration",colnames(time.song1), invert = TRUE)]
    
    return(time.song1)
    on.exit(rm(time.song1))
  }
  
  #Apply over each sound file
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makeCluster(parallel) else cl <- parallel
  
  ad <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(i) 
  { 
    adFUN(i, X, flim, wl, bp, envt, msmooth, ssmooth, mindur, maxdur)
  }) 
  
  results <- do.call(rbind, ad)
  
  #rename rows
  rownames(results) <- 1:nrow(results)
  
  #adjust time coordinates based on known deviance when using ssmooth
  if (!is.null(ssmooth) & !is.null(smadj))
  {if (smadj == "start" | smadj == "both") results$start <- results$start-((threshold*2.376025e-07)-1.215234e-05)*ssmooth 
  if (smadj == "end" | smadj == "both")  results$end <- results$end-((threshold*-2.369313e-07)+1.215129e-05)*ssmooth }  
  
  results1 <- results
  
  #remove NAs so the ones with no detections are printed
  results$start[is.na(results$start)] <- -2
  results$end[is.na(results$end)] <- -1
  
  # long spectrograms
  if (ls & img) {  
    if (any(parallel == 1, Sys.info()[1] == "Linux") & pb) cat("Producing long spectrogram:")
    
    #function for long spectrograms (based on lspec function)
    lspeFUN2 <- function(X, z, fl = flim, sl = sxrow, li = rows, fli = fli, pal, fast.spec) {
      #subset for a sound file    
      Y <- X[!is.na(X$start) & X$sound.files == z, ]
      
      #reset graphic parameters    
      collev = seq(-40, 0, 1)
      gr = FALSE
      cex = 1
      
      #loop to print spectros (modified from lspec function)
      rec <- warbleR::read_wave(z, path = path) #read wave file 
      f <- rec@samp.rate #set sampling rate
      frli<- fl #in case flim is higher than can be due to sampling rate
      if (frli[2] > ceiling(f/2000) - 1) frli[2] <- ceiling(f/2000) - 1 
      dur <- duration(rec)
      
      if (!length(grep("[^[:digit:]]", as.character(dur/sl))))  #if duration is multiple of sl
        rec <- seewave::cutw(wave = rec, f = f, from = 0, to = dur-0.001, output = "Wave") #cut a 0.001 segment of rec
      dur <- seewave::duration(rec) #set duration    
      
      #loop over pages 
      for (j in 1:ceiling(dur/(li*sl))){
        if (set) fna<-paste(substring(z, first = 1, last = nchar(as.character(z))-4),
                            "-autodetec.ls","-th" ,threshold , "-env.", envt, "-bp", bp[1],".",bp[2], "-smo", smo, "-midu", mindur,
                            "-mxdu", maxdur, "-pw", power, sep = "") else
                              fna <- paste(substring(z, first = 1, last = nchar(as.character(z))-4), "-autodetec.ls", sep = "")
                            
                            img_wrlbr_int(filename = paste(fna, "-p", j, ".tiff", sep = ""),  path = path, res = 160, units = "in", width = 8.5, height = 11)
                            
                            par(mfrow = c(li,  1), cex = 0.6, mar = c(0,  0,  0,  0), oma = c(2, 2, 0.5, 0.5), tcl = -0.25)
                            
                            #creates spectrogram rows
                            x <- 0
                            while(x <= li-1){
                              x <- x + 1
          if (all(((x)*sl+li*(sl)*(j-1))-sl < dur & (x)*sl+li*(sl)*(j-1) < dur)){  #for rows with complete spectro
            spectro_wrblr_int(rec, f = f, wl = wl, flim = frli, tlim = c(((x)*sl+li*(sl)*(j-1))-sl, (x)*sl+li*(sl)*(j-1)), collevels = collev, grid = gr, scale = FALSE, palette = pal, axisX = TRUE,
                    fast.spec = fast.spec, ...)
            if (x == 1)  text((sl-0.01*sl) + (li*sl)*(j - 1), frli[2] - (frli[2]-frli[1])/10, paste(substring(z, first = 1, 
       last = nchar(as.character(z))-4), "-p", j, sep = ""), pos = 2, font = 2, cex = cex)
          
      if (nrow(Y) > 0)
            {
                abline(v = c(Y$start, Y$end), col = "red", lty = 2)
  text(x = ((Y$start + Y$end)/2), y = frli[2] - 2*((frli[2] - frli[1])/12), labels = Y$selec, font = 4)
            }
            } else 
              { #for rows with incomplete spectro (final row)
  if (all(((x)*sl+li*(sl)*(j-1))-sl < dur & (x)*sl+li*(sl)*(j-1) > dur)){ 
    spectro_wrblr_int(seewave::pastew(seewave::noisew(f = f, d = (x)*sl+li*(sl)*(j-1)-dur+1, type = "unif",   
    listen = FALSE,  output = "Wave"), seewave::cutw(wave = rec, f = f, from = ((x)*sl+li*(sl)*(j-1))-sl,
     to = dur, output = "Wave"), f =f,  output = "Wave"), f = f, wl = wl, flim = frli, 
     tlim = c(0, sl), collevels = collev, grid = gr, scale = FALSE, palette = pal, axisX = FALSE, fast.spec = fast.spec, ...)
                                       
    if (x == 1)  text((sl-0.01*sl) + (li*sl)*(j - 1), frli[2] - (frli[2]-frli[1])/10, paste(substring(z, first = 1,                                                                                              last = nchar(as.character(z))-4), "-p", j, sep = ""), pos = 2, font = 2, cex = cex)
    
    #add axis to last spectro row
  axis(1, at = c(0:sl), labels = c((((x)*sl+li*(sl)*(j-1))-sl):((x)*sl+li*(sl)*(j-1))) , tick = TRUE)
  
  if (nrow(Y) > 0)
  {
    abline(v = c(Y$start, Y$end) - (((x)*sl+li*(sl)*(j-1))-sl), col = "red", lty = 2)
    text(x = ((Y$start + Y$end)/2) - (((x)*sl+li*(sl)*(j-1))-sl),  frli[2] - 2*((frli[2] - frli[1])/12), labels = Y$selec, font = 4)
  }
  
  #add line indicating end of sound file
  abline(v = dur-(((x)*sl+li*(sl)*(j-1))-sl), lwd = 2.5)
  usr<-par("usr")    
  polygon(x = rep(c(sl - ((x)*sl+li*(sl)*(j-1)-dur), usr[2]), each = 2), y = c(usr[3], usr[4], usr[4], usr[3]), col = "white")
  
  #add text indicating end of sound files
  text(dur-(((x)*sl+li*(sl)*(j-1))-sl), frli[2]-(frli[2]-frli[1])/2, "END OF SOUND FILE", pos = 4, font = 2, cex = 1.1)
  
  
  } else 
    {
    plot(1, 1, col = "white", col.axis =  "white", col.lab  =  "white", 
    xaxt = "n", yaxt = "n")
    
      usr<-par("usr")    
      polygon(x = rep(c(sl - ((x)*sl+li*(sl)*(j-1)-dur), usr[2]), each = 2), y = c(usr[3], usr[4], usr[4], usr[3]), col = "white")
      
    #add text indicating end of sound files
  text(dur-(((x)*sl+li*(sl)*(j-1))-sl), frli[2]-(frli[2]-frli[1])/2, "END OF SOUND FILE", pos = 4, font = 2, cex = 1.1)
    }
               }
        }
        dev.off() #reset graphic device
      }
    } 

    
    #Apply over each sound file
    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & parallel > 1)
      cl <- parallel::makeCluster(parallel) else cl <- parallel
    
    a1 <- pbapply::pblapply(X = unique(results$sound.files), cl = cl, FUN = function(z) 
    { 
      lspeFUN2(X = results, z = z, fl = flim, sl = sxrow, li = rows, pal = pal, fast.spec = fast.spec)
    }) 
    
  }

return(results1)
}


##############################################################################################################
#' alternative name for \code{\link{autodetec}}
#'
#' @keywords internal
#' @details see \code{\link{autodetec}} for documentation. \code{\link{autodetec}} will be deprecated in future versions.
#' @export

auto_detec <- autodetec
