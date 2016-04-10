#' Automatically detect vocalizations in sound files
#' 
#' \code{autodetec} automatically detects the start and end of vocalizations in sound files  based
#' on amplitude, duration, and frequency range attributes.
#' @usage autodetec(X = NULL, threshold = 15, envt = "abs", ssmooth = NULL, msmooth = NULL, 
#'   power = 1, bp = NULL, osci = FALSE, wl = 512, xl = 1, picsize = 1, res = 100, 
#'   flim = c(0,22), ls = FALSE, sxrow = 10, rows = 10, mindur = NULL, maxdur = 
#'   NULL, redo = FALSE, img = TRUE, it = "jpeg", set = FALSE, flist = NULL, smadj = NULL,
#'   parallel = 1)
#' @param X Data frame with results from \code{\link{manualoc}} function or any data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). 
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
#'   when code is rerun. If \code{FALSE} only the selections that do not have a image 
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
#'  It specifies the number of cores to be used. Default is 1 (e.i. no parallel computing).
#'   For windows users the 'parallelsugar' package should be installed.   
#' @return Image files with spectrograms showing the start and end of the detected signals. It 
#'   also returns a data frame containing the start and end of each signal by 
#'   sound file and selection number.
#' @export
#' @name autodetec
#' @details This function determines the start and end of signals in the segments of the sound files listed 
#'   in the input data frame. Alternatively, if no data frame is provided, the function detects signals across
#'   each entire sound file and creates long spectrograms for all sound files in the working directory.
#'   The input data frame should have the following columns: c("sound.files","selec","start","end"). 
#'   The ouptut of \code{\link{manualoc}} can be used as the input data frame. This function uses 
#'   a modified version of the \code{\link[seewave]{timer}} function from seewave 
#'   package to detect signals. 
#'   
#' @examples
#' \dontrun{
#' # First create empty folder
#' setwd(tempdir())
#' 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav") 
#' 
#' ad <- autodetec(threshold = 5, env = "hil", ssmooth = 300, power=1, 
#' bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE, 
#' wl = 300, ls = FALSE,  sxrow = 2, rows = 4, mindur=0.1, maxdur=1, set = TRUE)
#' 
#' #run it with different settings
#' ad <- autodetec(threshold = 10, env = "abs", ssmooth = 300, power = 1, 
#' bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE, 
#' wl = 300, ls = FALSE,  sxrow = 2, rows = 4, mindur=0.1, maxdur=1, set = TRUE)
#' 
#' #check this folder!!
#' getwd()
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}). Implements a
#' modified version of the timer function from seewave. 

autodetec<-function(X= NULL, threshold=15, envt="abs", ssmooth = NULL, msmooth = NULL, power = 1, 
                    bp = NULL, osci = FALSE, wl = 512, xl = 1, picsize = 1, res = 100, flim = c(0,22), 
                    ls = FALSE, sxrow = 10, rows = 10, mindur = NULL, maxdur = NULL, redo = FALSE, 
                    img = TRUE, it = "jpeg", set = FALSE, flist = NULL, smadj = NULL, parallel = 1){

  #if bp is not vector or length!=2 stop
  if(!is.null(bp))
  {if(!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if(!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}}    
  
  #if flim is not vector or length!=2 stop
  if(is.null(flim)) stop("'flim' must be a numeric vector of length 2") else {
    if(!is.vector(flim)) stop("'flim' must be a numeric vector of length 2") else{
      if(!length(flim) == 2) stop("'flim' must be a numeric vector of length 2")}}   
  
  #if msmooth is not vector or length!=2 stop
  if(!is.null(msmooth)) {
    if(!is.vector(msmooth)) stop("'msmooth' must be a numeric vector of length 2") else {
      if(!length(msmooth) == 2) stop("'msmooth' must be a numeric vector of length 2")}}   

  #if ssmooth is not vector or length!=1 stop
  if(!is.null(ssmooth)) {
    if(!is.vector(ssmooth)) stop("'ssmooth' must be a numeric vector of length 1") else {
      if(!length(ssmooth) == 1) stop("'ssmooth' must be a numeric vector of length 1")}}   
  
  #if wl is not vector or length!=1 stop
  if(is.null(wl)) stop("'wl' must be a numeric vector of length 1") else {
    if(!is.vector(wl)) stop("'wl' must be a numeric vector of length 1") else{
      if(!length(wl) == 1) stop("'wl' must be a numeric vector of length 1")}}  
  
  #if sxrow is not vector or length!=1 stop
  if(is.null(sxrow)) stop("'sxrow' must be a numeric vector of length 1") else {
    if(!is.vector(sxrow)) stop("'sxrow' must be a numeric vector of length 1") else{
      if(!length(sxrow) == 1) stop("'sxrow' must be a numeric vector of length 1")}}  
  
  #if rows is not vector or length!=1 stop
  if(is.null(rows)) stop("'rows' must be a numeric vector of length 1") else {
    if(!is.vector(rows)) stop("'rows' must be a numeric vector of length 1") else{
      if(!length(rows) == 1) stop("'rows' must be a numeric vector of length 1")}}  
  
  #if picsize is not vector or length!=1 stop
  if(is.null(picsize)) stop("'picsize' must be a numeric vector of length 1") else {
    if(!is.vector(picsize)) stop("'picsize' must be a numeric vector of length 1") else{
      if(!length(picsize) == 1) stop("'picsize' must be a numeric vector of length 1")}}  
  
  #if xl is not vector or length!=1 stop
  if(is.null(xl)) stop("'xl' must be a numeric vector of length 1") else {
    if(!is.vector(xl)) stop("'xl' must be a numeric vector of length 1") else{
      if(!length(xl) == 1) stop("'xl' must be a numeric vector of length 1")}}  
  
  #if res is not vector or length!=1 stop
  if(is.null(res)) stop("'res' must be a numeric vector of length 1") else {
    if(!is.vector(res)) stop("'res' must be a numeric vector of length 1") else{
      if(!length(res) == 1) stop("'res' must be a numeric vector of length 1")}}  
  
  #if threshold is not vector or length!=1 stop
  if(is.null(threshold))  stop("'threshold' must be a numeric vector of length 1") else {
    if(!is.vector(threshold)) stop("'threshold' must be a numeric vector of length 1") else{
      if(!length(threshold) == 1) stop("'threshold' must be a numeric vector of length 1")}}  
  #if parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  #if it argument is not "jpeg" or "tiff" 
  if(!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
  #if envt is not vector or length!=1 stop
  if(any(envt %in% c("abs", "hil"))){if(!length(envt) == 1) stop("'envt' must be a numeric vector of length 1")
  } else stop("'envt' must be either 'abs' or 'hil'" )
  
  if(any(!sapply(list(osci,ls, redo),is.logical))) 
    stop(paste(paste(c("osci","ls","redo")[!sapply(list(osci,ls, redo),is.logical)],collapse = " "),"not logical"))
  
  #stop if power is 0
  if (power == 0) 
    stop("'power' cannot equal to 0")
  
  if(!is.null(msmooth)) smo <- msmooth[1] else {if(!is.null(ssmooth)) smo <- ssmooth else smo <- 0}
  
  #if smadj argument is not "start" "end" or "both"
  if(!is.null(smadj)) if(!any(smadj == "start", smadj == "end", smadj == "both")) 
    stop(paste("smooth adjustment", smadj, "not allowed"))  
  
  #if parallel was called
      #if on windows you need parallelsugar package
   if(parallel > 1)
  { options(warn = -1)
    if(all(Sys.info()[1] == "Windows",requireNamespace("parallelsugar", quietly = TRUE) == TRUE)) 
      lapp <- function(X, FUN) parallelsugar::mclapply(X, FUN, mc.cores = parallel) else
       if(Sys.info()[1] == "Windows"){ 
      message("Windows users need to install the 'parallelsugar' package for parallel computing (you are not doing it now!)")
      lapp <- pbapply::pblapply} else lapp <- function(X, FUN) parallel::mclapply(X, FUN, mc.cores = parallel)} else lapp <- pbapply::pblapply
      
      options(warn = 0)
      
  if(!is.null(X)){
    
    #check if all columns are found
    if(any(!(c("sound.files", "selec", "start", "end") %in% colnames(X)))) 
      stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
        "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
      
    if(!class(X) == "data.frame") stop("X is not a data frame")
    
    #if there are NAs in start or end stop
    if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end columns")  
    
    #if end or start are not numeric stop
    if(all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'end' and 'selec' must be numeric")
    
    #if any start higher than end stop
    if(any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))  
    
    #return warning if not all sound files were found
    fs <- list.files(path = getwd(), pattern = ".wav$", ignore.case = TRUE)
    if(length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
      message(paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                    ".wav file(s) not found"))
    
    #count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% fs) 
    if(length(d) == 0) stop("The .wav files are not in the working directory") else X<-X[d,]  
    
    #redo the ones that have no images in folder
    if(!redo) {
      if(it == "tiff") tfs <- list.files(path = getwd(), pattern = ".tiff$", ignore.case = TRUE) else
        tfs <- list.files(path = getwd(), pattern = ".jpeg$", ignore.case = TRUE)
      
      if(set) X <- X[!(paste(substring(X$sound.files, first = 1, last = nchar(as.character(X$sound.files))-4),
              "-", X$selec, "-autodetec","-th" ,threshold ,"-bp", bp[1],".",bp[2], "-smo", smo, "-midu", mindur,
              "-mxdu", maxdur, "-pw", power, sep = "") %in% substring(tfs, 1, nchar(tfs)-15)),] else
      X <- X[!(paste(substring(X$sound.files, 1, nchar(as.character(X$sound.files))-4), X$selec, sep = "-") %in% substring(tfs, 1, nchar(tfs)-15)),]
      if(nrow(X) == 0) stop("All selections have been analyzed (redo = F)") 
    }    
    
    if(parallel == 1) {if(!ls & img) message("Detecting signals in sound files and producing spectrogram:") else 
      message("Detecting signals in sound files:")}
    
    ad<-lapp(1:nrow(X),function(i)
  {
    song<-tuneR::readWave(as.character(X$sound.files[i]),from=X$start[i],to=X$end[i],units="seconds")
    
    f <- song@samp.rate
    fl<- flim #in case flim its higher than can be due to samplin rate
    if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
    
    #filter frequnecies below 1000 Hz
    if(!is.null(bp))
    f.song<-seewave::ffilter(song, f=f, from = bp[1]*1000, to = bp[2]*1000, bandpass = T,wl= wl, output="Wave") else
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
    time.song<-data.frame(duration = aut.det$s, selec = NA, start = aut.det$s.start+X$start[i], end = (aut.det$s+aut.det$s.start+X$start[i]))
   
    #remove signals based on duration  
    if(!is.null(mindur)) time.song<-time.song[time.song$duration>mindur,]
    if(!is.null(maxdur)) time.song<-time.song[time.song$duration<maxdur,]
    
    if(nrow(time.song)>0) 
    time.song$selec <- paste(X$selec[i], 1:nrow(time.song), sep = "-")
    
    if(!ls & img) {
      if(set) 
        fna<-paste(substring(X$sound.files[i], first = 1, last = nchar(as.character(X$sound.files[i]))-4),
                   "-", X$selec[i], "-autodetec","-th" ,threshold , "-env.", envt,"-bp", bp[1],".",bp[2], "-smo", smo, "-midu", mindur,
                   "-mxdu", maxdur, "-pw", power, sep = "") else
        fna<-paste(substring(X$sound.files[i], first = 1, last = nchar(as.character(X$sound.files[i]))-4),
                "-", X$selec[i], "-autodetec", sep = "")                  
  
      if(it == "tiff") tiff(filename = paste(fna, ".tiff", sep = "-"), 
        width = (10.16) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res) else
          jpeg(filename = paste(fna, "-", X$selec[i], ".jpeg", sep = ""), 
               width = (10.16) * xl * picsize, height = (10.16) * picsize, units = "cm", res = res)
      
      seewave::spectro(song,f=f,wl = wl,collevels=seq(-45,0,1),grid=F,main = as.character(X$sound.files[i]),osc = osci,
              scale=F,palette=reverse.gray.colors.2,flim = fl)
      
      if(nrow(time.song)>0)
      {sapply(1:nrow(time.song), function(j)  abline(v=c(time.song$start[j]-X$start[i], time.song$end[j]-X$start[i]),col="red",lwd=2, lty= "dashed"))
    
      sapply(1:nrow(time.song), function(j)  text(time.song$start[j]+time.song$duration[j]/2-X$start[i],
                                                 rep(c(((fl[2]-fl[1])*0.85)+fl[1],((fl[2]-fl[1])*0.9)+fl[1],((fl[2]-fl[1])*0.95)+fl[1]),
                                                     nrow(time.song))[j],paste(X$selec[i], j, sep = "-"),cex=1))} 
    
    

 dev.off()
    }
  if(nrow(time.song)==0)
  time.song<-data.frame(duration=NA,selec=NA,start=NA, end=NA)
  return((time.song[,grep("duration",colnames(time.song),invert = T)]))
    
  }
  )

  results <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(results) <- c("sound.files", "selec", "start", "end")
  for(v in 1:length(ad))
    results<-rbind(results,data.frame(sound.files = X$sound.files[v],  ad[v]))

  results<-droplevels(results)

  } else 
    {
    
    #stop if no files .wav are found
    files <- list.files(pattern = "wav$", ignore.case = T) #list .wav files in working director
    if(length(files) == 0) stop("no .wav files in working directory")
    
    #subet based on file list provided (flist)
    if(!is.null(flist)) files <- files[files %in% flist]
    if(length(files) == 0) stop(".wav files are not in working directory")    

    #do the ones that have no images in folder
    if(!redo) {
      if(it == "tiff") {tfs <- list.files(path = getwd(), pattern = ".tiff$", ignore.case = TRUE)
                        tfs <- grep("autodetec", tfs, value = TRUE)
                        tfs <- gsub(".tiff$", "", tfs)
                          } else
    {tfs <- list.files(path = getwd(), pattern = ".jpeg$", ignore.case = TRUE)      
      tfs <- grep("autodetec", tfs, value = TRUE)
     tfs <- gsub(".jpeg$", "", tfs)
    }
if(length(tfs)>0) {if(set)
  { files <- files[grep(paste(tfs, collapse = "|"),
        sapply(gsub(".wav","", ignore.case = T, files), function(k) 
    {paste(k, "-autodetec.ls","-th" ,threshold , "-env.", envt, "-bp", bp[1],".",bp[2], "-smo",
           smo, "-midu", mindur, "-mxdu", maxdur, "-pw", power, "-p1", sep = "")}, 
    USE.NAMES = F),invert = TRUE)]} else
{  
#   if(length(grep(paste(paste(gsub(".wav","", ignore.case = T, files),"-autodetec.ls-p",sep = ""),
#                        collapse = "|"),tfs,value = T))>0) 
files <- grep(paste(sapply(tfs,function(x) strsplit(x,split = "-autodetec.ls-p")[[1]][1]), collapse = "|"),
     files, value = TRUE, invert = TRUE)}

if(length(files) == 0) stop("All files have been analyzed (redo = F)") 
    }
    }  
    
    if(parallel == 1)  message("Detecting signals in sound files:")
    
    ad<-lapp(1:length(files),function(i)
    {
      song<-tuneR::readWave(as.character(files[i]))
      f <- song@samp.rate
      fl<- flim #in case flim its higher than can be due to samplin rate
      if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
      
      #filter frequnecies below 1000 Hz
      if(!is.null(bp))
        {f.song<-seewave::ffilter(song, f=f, from = bp[1]*1000, to = bp[2]*1000, bandpass = T,wl=wl, output="Wave")} else
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
      if (wave2[1] == 1) {
        signal <- durations[seq(2, npos - 1, by = 2)]
        start.signal <- positions[seq(2, npos - 1, by = 2)]
      }  else {
        signal <- durations[seq(1, npos - 1, by = 2)]
        start.signal <- positions[seq(1, npos - 1, by = 2)]
      }
      aut.det <- list(s = signal, s.start = start.signal)
      

      #put time of detection in data frame
      time.song<-data.frame(duration = aut.det$s, start = aut.det$s.start, end = (aut.det$s+aut.det$s.start))
      
      #remove signals based on duration
      if(!is.null(mindur)) time.song<-time.song[time.song$duration>mindur,]
      if(!is.null(maxdur)) time.song<-time.song[time.song$duration<maxdur,]
      
      if(nrow(time.song)==0) time.song<-data.frame(duration=NA,start=NA, end=NA)
      
      return((time.song[,grep("duration",colnames(time.song),invert = T)]))
      
    })
    
    results <- data.frame(matrix(nrow = 0, ncol = 4))
    colnames(results) <- c("sound.files", "selec", "start", "end")
    for(v in 1:length(ad))
    {if(nrow(ad[[v]])<=1)
    {  
results<-rbind(results,data.frame(sound.files =  files[v], selec = 1,start=ad[[v]][1],end=ad[[v]][2]))
      } else{
      results<-rbind(results,data.frame(sound.files = files[v], selec = 1: nrow(ad[[v]]),ad[v]))
    }}
 
  results<-droplevels(results)
  
  #rename selections for the same sound file
  if(length(unique(results$sound.files))>1)
  {selec<-vector()
   for(e in 1:length(unique(results$sound.files))) 
   selec<-c(selec,c(1:table(results$sound.files)[e])) 
  results$selec<-selec} else results$selec<-1:nrow(results) 
    }
  
  #long spectrograms
if(any(ls,is.null(X)) & img) {
  if(parallel == 1) message("Producing long spectrogram:")
  
  collev = seq(-40, 0, 1)  
  manualoc = data.frame(results,sel.comment=NA)
  gr = FALSE
  pal = reverse.gray.colors.2
  cex = 1
  
    #read files
    files <- list.files(path = getwd(), pattern = ".wav$", ignore.case = T)  
  if(length(files) == 0) stop("no .wav files in working directory")  
  
  #do the ones that have no images in folder
#     if(!redo) {
#       if(it == "tiff") {tfs <- list.files(path = getwd(), pattern = ".tiff$", ignore.case = TRUE)
#           tfs <- grep("autodetec", tfs, value = TRUE) } else{
#         tfs <- list.files(path = getwd(), pattern = ".jpeg$", ignore.case = TRUE)
#         tfs <- grep("autodetec", tfs, value = TRUE)}      
#       if(length(tfs)>0)for(k in gsub(".wav","", ignore.case = T, files))
#         if(length(grep(k,tfs,value = T))>0) files <- grep(k, files, value = TRUE, invert = T)
#           }    
#       if(length(files) == 0) stop("All files have been analyzed (redo = F)") 
#   
#   
#stop if no files .wav are found
files <- list.files(pattern = "wav$", ignore.case = T) #list .wav files in working director
if(length(files) == 0) stop("no .wav files in working directory")

#subet based on file list provided (flist)
if(!is.null(flist)) files <- files[files %in% flist]

#do the ones that have no images in folder
if(!redo) {
  if(it == "tiff") {tfs <- list.files(path = getwd(), pattern = ".tiff$", ignore.case = TRUE)
                    tfs <- grep("autodetec", tfs, value = TRUE)
                    tfs <- gsub(".tiff$", "", tfs)
  } else
  {tfs <- list.files(path = getwd(), pattern = ".jpeg$", ignore.case = TRUE)      
   tfs <- grep("autodetec", tfs, value = TRUE)
   tfs <- gsub(".jpeg$", "", tfs)
  }
  if(length(tfs)>0) {if(set)
  { files <- files[grep(paste(tfs, collapse = "|"),
                        sapply(gsub(".wav","", ignore.case = T, files), function(k) 
                        {paste(k, "-autodetec.ls","-th" ,threshold , "-env.", envt, "-bp", bp[1],".",bp[2], "-smo",
                               smo, "-midu", mindur, "-mxdu", maxdur, "-pw", power, "-p1", sep = "")}, 
                        USE.NAMES = F),invert = TRUE)]} else
                        {  
                          #   if(length(grep(paste(paste(gsub(".wav","", ignore.case = T, files),"-autodetec.ls-p",sep = ""),
                          #                        collapse = "|"),tfs,value = T))>0) 
                          files <- grep(paste(sapply(tfs,function(x) strsplit(x,split = "-autodetec.ls-p")[[1]][1]), collapse = "|"),
                                        files, value = TRUE, invert = TRUE)}
  
  if(length(files) == 0) stop("All files have been analyzed (redo = F)") 
  }
} 

    #subet based on file list provided (flist)
#     if(!is.null(flist)) files <- files[files %in% flist]
    
    #remove the ones not in X
    if(!is.null(X)) {files <- files[files %in% X$sound.files]
    if(length(files) == 0) stop("sound files listed in data frame are not in working directory") } 

    #read manualoc files
    if(!is.null(manualoc)) manloc <- manualoc  else manloc <- NULL
    
    #apply over each sound file
  lapp(files, function(z, fl = flim, sl = sxrow, li = rows, ml = manloc, malo = manualoc) {
      
      #loop to print spectros (modified from lspec function)
      rec <- tuneR::readWave(z) #read wave file 
      f <- rec@samp.rate #set sampling rate
      frli<- fl #in case flim its higher than can be due to samplin rate
      if(frli[2] > ceiling(f/2000) - 1) frli[2] <- ceiling(f/2000) - 1 
      dur <- length(rec@left)/rec@samp.rate #set duration    
      
      if(!length(grep("[^[:digit:]]", as.character(dur/sl))))  #if duration is multiple of sl
        rec <- seewave::cutw(wave = rec, f = f, from = 0, to = dur-0.001, output = "Wave") #cut a 0.001 segment of rec     
      dur <- length(rec@left)/rec@samp.rate #set duration    
      
      if(!is.null(malo)) ml <- ml[ml$sound.files == z,] #subset manualoc data
      #loop over pages 
      for (j in 1:ceiling(dur/(li*sl))){
        if(set) fna<-paste(substring(z, first = 1, last = nchar(z)-4),
                            "-autodetec.ls","-th" ,threshold , "-env.", envt, "-bp", bp[1],".",bp[2], "-smo", smo, "-midu", mindur,
                           "-mxdu", maxdur, "-pw", power, sep = "") else
        fna<-paste(substring(z, first = 1, last = nchar(z)-4), "-autodetec.ls", sep = "")
          
        if(it == "tiff") tiff(filename = paste(fna, "-p", j, ".tiff", sep = ""),  
             res = 160, units = "in", width = 8.5, height = 11) else
               jpeg(filename = paste(fna, "-p", j, ".jpeg", sep = ""),  
               res = 160, units = "in", width = 8.5, height = 11)

        par(mfrow = c(li,  1), cex = 0.6, mar = c(0,  0,  0,  0), oma = c(2, 2, 0.5, 0.5), tcl = -0.25)
        
        #creates spectrogram rows
        x <- 0
        while(x <= li-1){
          x <- x + 1
          if(all(((x)*sl+li*(sl)*(j-1))-sl<dur & (x)*sl+li*(sl)*(j-1)<dur)){  #for rows with complete spectro
            seewave::spectro(rec, f = f, wl = 512, flim = frli, tlim = c(((x)*sl+li*(sl)*(j-1))-sl, (x)*sl+li*(sl)*(j-1)), 
                    ovlp = 10, collevels = collev, grid = gr, scale = FALSE, palette = pal, axisX = T)
            if(x == 1)  text((sl-0.01*sl) + (li*sl)*(j - 1), frli[2] - (frli[2]-frli[1])/10, paste(substring(z, first = 1, 
                                                                                                            last = nchar(z)-4), "-p", j, sep = ""), pos = 2, font = 2, cex = cex)
            if(!is.null(malo))  {if(any(!is.na(ml$sel.comment))) l <- paste(ml$selec,"-'",ml$sel.comment,
                                                                            "'",sep="") else {l <- ml$selec}
                                 mapply(function(se, s, e, sc, labels, fli = frli){
                                   abline(v = c(s, e), col = "red", lty = 2)
                                    text((s + e)/2,  fli[2] - 2*((fli[2] - fli[1])/12), labels = labels , font = 4)},
                                   se = ml$selec, s = ml$start, e = ml$end,sc = ml$sel.comment, 
                                   labels = l)} } else {
                                     if(all(((x)*sl+li*(sl)*(j-1))-sl < dur & (x)*sl+li*(sl)*(j-1)>dur)){ #for rows with incomplete spectro (final row)
                                       seewave::spectro(seewave::pastew(seewave::noisew(f = f,  d = (x)*sl+li*(sl)*(j-1)-dur+1,  type = "unif",   
                                                             listen = FALSE,  output = "Wave"), seewave::cutw(wave = rec, f = f, from = ((x)*sl+li*(sl)*(j-1))-sl,
                                                                                                     to = dur, output = "Wave"), f =f,  output = "Wave"), f = f, wl = 512, flim = frli, 
                                               tlim = c(0, sl), ovlp = 10, collevels = collev, grid = gr, scale = FALSE, palette = pal, axisX = F)
                                       
                                       #add manualoc lines and labels
                                       
                                       if(!is.null(malo)) { if(any(!is.na(ml$sel.comment))) l <- paste(ml$selec,"-'",ml$sel.comment,
                                                                                                       "'",sep="") else {l <- ml$selec}
                                                            lise <- ((x)*sl+li*(sl)*(j-1))-sl
                                                            mapply(function(se, s, e, sc, labels, fli = frli, ls = lise){
                                                               abline(v = c(s, e)-ls, col = "red", lty = 2)
                                                               text(((s + e)/2)-ls, fli[2] - 2*((fli[2] - fli[1])/12), 
                                                                   labels = labels, font = 4)}, 
                                                              se = ml$selec, s = ml$start, e = ml$end, sc = ml$sel.comment, labels = l)}
                                       
                                       #add axis to last spectro row
                                       axis(1, at = c(0:sl), labels = c((((x)*sl+li*(sl)*(j-1))-sl):((x)*sl+li*(sl)*(j-1))) , tick = TRUE)
                                       
                                       #add text indicating end of sound files
                                        text(dur-(((x)*sl+li*(sl)*(j-1))-sl), frli[2]-(frli[2]-frli[1])/2, "END OF SOUND FILE", pos = 4, font = 2, cex = 1.1)
                                       
                                       #add line indicating end of sound file
                                        abline(v = dur-(((x)*sl+li*(sl)*(j-1))-sl), lwd = 2.5)} else {plot(1, 1, col = "white", col.axis =  "white", col.lab  =  "white", 
                                                                                                          xaxt = "n", yaxt = "n")
                                       }}
        }
        dev.off() #reset graphic device
      }
    })  
}  
 
rownames(results) <- 1:nrow(results)
if(!is.null(ssmooth) & !is.null(smadj))
  {if(smadj == "start" | smadj == "both") results$start <- results$start-((threshold*2.376025e-07)-1.215234e-05)*ssmooth 
  if(smadj == "end" | smadj == "both")  results$end <- results$end-((threshold*-2.369313e-07)+1.215129e-05)*ssmooth }
return(results)
if(img) on.exit(dev.off())
}
