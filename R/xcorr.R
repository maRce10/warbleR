#' Spectrogram cross-correlation 
#' 
#' \code{xcorr} estimates the similarity of two spectrograms by means of cross-correlation
#' @usage xcorr(X, wl =512, frange= NULL, ovlp=90, dens=0.9, bp= NULL, wn='hanning', 
#' cor.method = "pearson", parallel = 1, path = NULL, pb = TRUE, na.rm = FALSE,
#'  dfrange = FALSE, cor.mat = TRUE)
#' @param  X 'selection_table', 'extended_selection_table' or data frame containing columns for sound files (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#' is 512.
#' @param frange A numeric vector of length 2 setting the upper and lower frequency limits (in kHz) 
#' in which to compare the signals or "frange" (default) to indicate that values in 'bottom.freq'
#' and 'top.freq' columns will be used as frequency limits. Alternatively, the \code{\link{dfts}} function can 
#' be used to determine this parameter if \code{dfrange = TRUE} and \code{frange = NULL}. This method is more 
#' adequate for pure tone signals. Default is \code{NULL}. Either 'frange' should be provided or 
#' set \code{dfrange = TRUE}.
#' @param dfrange Logical. If \code{TRUE} the \code{\link{dfts}} function can is used to determine the frequency range in which to compare signals.
#' Ignored if 'frange' is provided. 
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#' consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 90. High values of ovlp 
#' slow down the function but produce more accurate results.
#' @param dens Numeric vector of length 1 specifying the approximate density of points in which to sample amplitude. 
#' See \code{\link[monitoR]{makeTemplate}}. Deafult is 0.9.
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#' frequency bandpass filter (in kHz) in which to detect dominant frequency. 
#' Only applied when frange is \code{NULL}. Default is \code{NULL}.
#' @param wn A character vector of length 1 specifying the window name as in \code{\link[seewave]{ftwindow}}. 
#' @param cor.method A character vector of length 1 specifying the correlation method as in \code{\link[stats]{cor}}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is not accurate as the number of pairwise comparisons decreases on 
#' each iteration. The first iteration runs n-1 comparisons while the last one 
#' only 1 (\code{n = nrow(X)}).
#' @param na.rm Logical. If \code{TRUE} all NAs produced when pairwise cross-correlations failed are removed from the 
#' results. This means that all selections with at least 1 cross-correlation that failed are excluded.
#' @param cor.mat Logical. If \code{TRUE} only the correlation matrix is returned. Default is \code{TRUE}.
#' @return If corr.mat is \code{TRUE} the function returns a matrix with 
#' the maximum (peak) correlation for each pairwise comparison. Otherwise it will return a list that includes 1) a data frame with the correlation statistic for each "sliding" step, 2) a matrix with 
#' the maximum correlation for each pairwise comparison, and 3) the frequency range. 
#' @export
#' @name xcorr
#' @details This function calculates the pairwise similarity of multiple signals by means of spectrogram cross-correlation.
#' This method "slides" one spectrogram over the other calculating a correlation of the amplitude values at each step.
#' The function runs pairwise cross-correlations on several signals and returns a list including the correlation statistic
#' for each "sliding" step as well as the maximum (peak) correlation for each pairwise comparison. To accomplish this the margins
#' of the signals are expanded by half the duration of the signal both before and after the provided time coordinates. 
#' The correlation matrix could have NA's if some of the pairwise correlation did not work (common when sound files have been modified by band-pass filters).
#' This function is a modified version of the \code{\link[monitoR]{corMatch}} and \code{\link[monitoR]{makeTemplate}} 
#' from the awesome R package `monitoR`.   
#' @examples
#' {
#' #First set temporary working directory
#' # setwd(tempdir())
#' 
#' #load data
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4","selec.table"))
#' writeWave(Phae.long1, "Phae.long1.wav") #save sound files
#' writeWave(Phae.long2, "Phae.long2.wav")
#' writeWave(Phae.long3, "Phae.long3.wav")
#' writeWave(Phae.long4, "Phae.long4.wav")
#'
#'xcor <- xcorr(X = selec.table, wl = 300, frange = c(2, 9), ovlp = 90,
#'dens = 1, wn = 'hanning', cor.method = "pearson")
#' 
#' }
#' @seealso \code{\link{xcorr.graph}}
#' @author Marcelo Araya-Salas \email{araya-salas@@cornell.edu})
#' @references H. Khanna, S.L.L. Gaunt & D.A. McCallum (1997). Digital spectrographic 
#' cross-correlation: tests of sensitivity. Bioacoustics 7(3): 209-234
# last modification on may-7-2018 (MAS)

xcorr <- function(X = NULL, wl = 512, frange = NULL, ovlp = 90, dens = 0.9, bp = NULL, wn ='hanning', 
                  cor.method = "pearson", parallel = 1, path = NULL,
                  pb = TRUE, na.rm = FALSE, dfrange = FALSE, cor.mat = TRUE)
{
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(xcorr)
  
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
  if (is.null(path)) path <- getwd() else {if (!dir.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end") 
  
  #stop if only 1 selection
  if (nrow(X) == 1) stop("you need more than one selection to do cross-correlation")
  
  #if bp is not vector or length!=2 stop
  if (!is.null(bp)) {if (!is.vector(bp)) stop("'bp' must be a numeric vector of length 2") else{
    if (!length(bp) == 2) stop("'bp' must be a numeric vector of length 2")}}
  
  #if flim is not vector or length!=2 stop
  if (frange[1] != "frange")
  {
    if (is.null(frange) & !dfrange) stop("either 'frange' must be provided or 'dfrange' set to TRUE")
  if (!is.null(frange) & !is.vector(frange)) stop("'frange' must be a numeric vector of length 2 or set to 'frange'") else
      if (!is.null(frange) & !length(frange) == 2) stop("'frange' must be a numeric vector of length 2 or set to 'frange'")
    } else  {if (!any(names(X) == "bottom.freq") & !any(names(X) == "top.freq")) stop("'frange' = frange requires bottom.freq and top.freq columns in X")
      if (any(is.na(c(X$bottom.freq, X$top.freq)))) stop("NAs found in bottom.freq and/or top.freq") 
      if (any(c(X$bottom.freq, X$top.freq) < 0)) stop("Negative values found in bottom.freq and/or top.freq") 
      if (any(X$top.freq - X$bottom.freq < 0)) stop("top.freq should be higher than bottom.freq")
    }
  
  #if wl is not vector or length!=1 stop
  if (!is.numeric(wl)) stop("'wl' must be a numeric vector of length 1") else {
    if (!is.vector(wl)) stop("'wl' must be a numeric vector of length 1") else{
      if (!length(wl) == 1) stop("'wl' must be a numeric vector of length 1")}} 

  #if ovlp is not vector or length!=1 stop
  if (!is.numeric(ovlp)) stop("'ovlp' must be a numeric vector of length 1") else {
    if (!is.vector(ovlp)) stop("'ovlp' must be a numeric vector of length 1") else{
      if (!length(ovlp) == 1) stop("'ovlp' must be a numeric vector of length 1")}} 
  
  #if dens is not vector or length!=1 stop
  if (!is.numeric(dens)) stop("'dens' must be a numeric vector of length 1") else {
    if (!is.vector(dens)) stop("'dens' must be a numeric vector of length 1") else{
      if (!length(dens) == 1) stop("'dens' must be a numeric vector of length 1")}} 
  
  if (!is_extended_selection_table(X)){
    #return warning if not all sound files were found
    fs <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
    if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
      write(file = "", x = paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                                 ".wav file(s) not found"))
    
    #count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% fs) 
    if (length(d) == 0){
      stop("The .wav files are not in the working directory")
    }  else {
      X <- X[d, ]
    }
  }
  
  
# if frange was not provided the range is calculated with dominant frequency range  
if (dfrange & is.null(frange)) {df <- dfts(X, wl =300, img = FALSE, length.out = 50, parallel = parallel, clip.edges = TRUE)
  df <- df[, 3:ncol(df)]
frq.lim = c(min(df, na.rm = TRUE), max(df, na.rm = TRUE))
} else 
  if (frange == "frange") frq.lim <- c(min(X$bottom.freq), max(X$top.freq)) else frq.lim <- frange

  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
#create templates
  if (pb) write(file = "", x ="creating templates:")

  tempFUN <- function(X, x, wl, ovlp, wn, frq.lim)
  {
    clip <- read_wave(X = X, index = x)
    samp.rate <- clip@samp.rate
    
    # Fourier transform
    t.survey <- seewave::duration(clip)
    fspec <- seewave::spectro(wave = clip, wl = wl, ovlp = ovlp, wn = wn, plot = FALSE)
    
    # Filter amplitudes 
    t.bins <- fspec$time
    n.t.bins <- length(t.bins)
    which.frq.bins <- which(fspec$freq >= frq.lim[1] & fspec$freq <= frq.lim[2])
    frq.bins <- fspec$freq[which.frq.bins]
    n.frq.bins <- length(frq.bins)
    amp <- round(fspec$amp[which.frq.bins, ],2)
    
    # Create empty matrix for identifying selected cells
    on.mat <- matrix(0, nrow=n.frq.bins, ncol=n.t.bins)
    
    # Bin steps
    t.step <- t.bins[2] - t.bins[1]
    frq.step <- frq.bins[2] - frq.bins[1]
    
    # Set cells that meet criteria to 1 in bin.amp
    on.mat <- on.mat + sample(c(1, 0), length(on.mat), TRUE, c(dens, 1-dens))
    
    # Then find locations of 
    pts <- which(on.mat == 1, arr.ind = TRUE)
    pts <- pts[, 2:1]
    colnames(pts) <- c('t', 'frq')
    pts[, 'frq'] <- pts[, 'frq'] + min(which.frq.bins) - 1
    pt.on <- pts
    
    # Get amplitudes
    pts <- pt.on
    pts.trimmed <- pts
    pts.trimmed[, 'frq'] <- pts.trimmed[, 'frq'] - min(which.frq.bins) + 1
    pt.amp <- amp[pts.trimmed[, 2:1, drop=FALSE]]
    pts <- cbind(pts, pt.amp)
    colnames(pts) <- c('t', 'frq', 'amp')
    
    
    t.shift <- min(pts[, 1])
    first.t.bin <- t.shift*t.step - t.step
    pts[, 't'] <- pts[, 't'] - t.shift + 1
    
    n.t.bins <- diff(range(pts[, 't']))
    n.frq.bins <- diff(range(pts[, 'frq']))
    duration <- n.t.bins * t.step
    frq.lim <- range(pts[, 'frq']) * frq.step
    
    template <- list(X$sound.files[x], X$selec[x], samp.rate = as.integer(samp.rate), 
                     pts = pts, t.step = t.step, frq.step = frq.step, n.t.bins = as.integer(n.t.bins), 
                     first.t.bin = first.t.bin, n.frq.bins = as.integer(n.frq.bins), duration = duration)
    names(template)<-c("sound.files", "selec", "samp.rate", "pts", "t.step", "frq.step", "n.t.bins","first.t.bin",
                       "n.frq.bins", "duration")
    
    return(template)
  }
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function for templates
  ltemp <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(x) 
    tempFUN(X, x, wl, ovlp, wn, frq.lim)
    )   
  
names(ltemp) <- paste(X$sound.files,X$selec,sep = "-")

#correlation function
FUNXC <- function(i, cor.mat, survey ,wl, ovlp, wn, j, X)
{
  template <- ltemp[[i]]
  
  # Perform Fourier transform on survey
  survey.spec <- seewave::spectro(wave = survey, wl = wl, ovlp = ovlp, wn = wn, plot = FALSE)
  
  # NTS arbitrary adjustment to eliminate -Inf
  survey.spec$amp[is.infinite(survey.spec$amp)] <- min(survey.spec$amp[!is.infinite(survey.spec$amp)]) - 10
  frq.bins <- survey.spec$freq
  t.bins <- survey.spec$time
  t.survey <- seewave::duration(survey)
  t.step <- t.bins[2] - t.bins[1]
  frq.step <- frq.bins[2] - frq.bins[1]
  
  pts <- template$pts[, c(2:1, 3)]
  
  # Adjust pts if step sizes differ
  if (!isTRUE(all.equal(template$t.step, t.step, tolerance=t.step/1E4))) {
    pts[, 't'] <- round(pts[, 't'] * template$t.step/t.step)
  }
  if (!isTRUE(all.equal(template$frq.step, frq.step, tolerance=frq.step/1E6))) {
    pts[, 'frq'] <- round(pts[, 'frq'] * template$frq.step/frq.step)
  }
  
  # Determine the frequency limits from the template points
  frq.lim <- frq.bins[range(pts[, 'frq'])] 
  
  # Get number of time windows/bins in frequency domain data
  n.t.survey <- length(survey.spec$time)
  
  #  down amplitude matrix based on filter frequencies 
  which.frq.bins <- which(survey.spec$freq >= frq.lim[1] & survey.spec$freq <= frq.lim[2])
  amp.survey <- survey.spec$amp[which.frq.bins, ]
  
  # Shift frq indices in pts. The t indices already start at 1.
  pts[, 'frq'] <- pts[, 'frq'] - min(which.frq.bins) + 1
  n.t.template <- max(pts[, 't'])
  n.frq.template <- max(pts[, 'frq'])
  
  # Translate pts matrix of indices into a vector index so indexing is faster within the lapplyfun call
  pts.v <- (pts[, 't'] - 1)*n.frq.template + pts[, 'frq']
  amp.template <- pts[, 'amp']
  amp.survey.v <- c(amp.survey)  
  
  # Perform analysis for each time value (bin) of survey 
  # Starting time value (bin) of correlation window
  c.win.start <- as.list(1:(n.t.survey-n.t.template)*n.frq.template) # Starting position of subset of each survey amp matrix  
  score.survey <- sapply(X=c.win.start, FUN=function(x) 
    {
      # Unpack columns of survey amplitude matrix for correlation analysis
      try_na(cor(amp.template, amp.survey.v[x + pts.v], method=cor.method, use='complete.obs')) 
    }
    )
  
  # Collect score results and time (center of time bins) in data frame
  if (any(!is.na(score.survey)))
  score.L <- data.frame(sound.file1 = paste(X$sound.files[j], X$selec[j], sep= "-"),sound.file2 = paste(template$sound.files,template$selec, sep= "-"), time=survey.spec$time[1:(n.t.survey-n.t.template)+n.t.template/2][!is.na(score.survey)], 
                        score=score.survey[!is.na(score.survey)]) else 
                          
  score.L <- data.frame(sound.file1 = paste(X$sound.files[j], X$selec[j], sep= "-"), sound.file2 = paste(template$sound.files,template$selec, sep= "-"), time=survey.spec$time[1:(n.t.survey-n.t.template)+n.t.template/2][1], 
                                                score= NA)
  return(score.L)
}

#run cross-correlation
if (pb) write(file = "", x ="running cross-correlation:")

if (Sys.info()[1] == "Windows" & parallel > 1)
  cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel

ord.shuf <- sample(1:(nrow(X)-1))
a <- pbapply::pblapply(X = ord.shuf, cl = cl, FUN = function(j) 
 {
  a <- read_wave(X = X, index = j, header = TRUE)  
 
  margin <-(max(with(X, end[j:nrow(X)] - start[j:nrow(X)])))/2
  start <-X$start[j] - margin
  if (start < 0) {
    end <-X$end[j] + margin -start
    start <- 0} else
  end <-X$end[j] + margin
  if (end > a$samples/a$sample.rate) end <- a$samples/a$sample.rate - 0.001
  
  survey <- read_wave(X = X, index = j, from = start, to = end)
  
  score.L <- lapply((1+j):length(ltemp), function(i) try(FUNXC(i, cor.mat, survey, wl, ovlp, wn,  j, X), silent = T))
  
  if (any(!sapply(score.L, is.data.frame))) {
  if (j != (length(ltemp)-1))
    {
    combs <- t(combn(paste(X$sound.files, X$selec,sep = "-"), 2))
  combs <- combs[combs[,1] == paste(X$sound.files[j], X$selec[j],sep = "-"),]
  comDF <- data.frame(sound.file1 = combs[,1], sound.file2 = combs[,2], time = 0, score = NA, stringsAsFactors = FALSE)
  
  score.L <-lapply(1:length(score.L), function(x) {
    if (is.data.frame(score.L[[x]])) return(score.L[[x]]) else
      return(comDF[x,])
  })
  } else  score.L[[1]] <- data.frame(sound.file1 = paste(X$sound.files[j], X$selec[j],sep = "-"), sound.file2 = paste(X$sound.files[j + 1], X$selec[j + 1], sep = "-"), time = 0, score = NA, stringsAsFactors = FALSE)
  }
  score.df <- do.call("rbind", score.L)

  
    if (cor.mat)  
{      score.df <- data.frame(dyad = paste(score.df$sound.file1,score.df$sound.file2,sep = "/"), score.df)
  
  # calculate maximum correlation values
  score.df <- aggregate(as.data.frame(score.df$score), by = list(score.df$dyad), FUN = max)}
      
return(score.df)
  }
)

a <- a[order(ord.shuf)]

# put together correlation results in a single data frame
b <- do.call("rbind", a)
rm(a)

if (!cor.mat)
{b <- data.frame(dyad = paste(b$sound.file1,b$sound.file2,sep = "/"), b)

# calculate maximum correlation values
scores <- aggregate(as.data.frame(b$score), by = list(b$dyad), FUN = max)
} else scores <- b

names(scores)[2] <- "scores"

#create a similarity matrix with the max xcorr
mat <- matrix(nrow = nrow(X), ncol = nrow(X))
mat[]<-1
colnames(mat) <- rownames(mat) <- paste(X$sound.files, X$selec, sep = "-")

mat[lower.tri(mat, diag=FALSE)] <- scores$scores
mat <- t(mat)
mat[lower.tri(mat, diag=FALSE)] <- scores$scores

if (na.rm)
{
com.case <- intersect(rownames(mat)[stats::complete.cases(mat)], colnames(mat)[stats::complete.cases(t(mat))])
if (length(which(is.na(mat))) > 0) 
   warning(paste(length(which(is.na(mat))), "pairwise comparisons failed and were removed"))

   #remove them from mat
   mat <- mat[rownames(mat) %in% com.case, colnames(mat) %in% com.case]
if (nrow(mat) == 0) stop("Not selections remained after removing NAs (na.rm = TRUE)")

   #clean correlation data
   if (!cor.mat)
   b <- b[b$sound.file1 %in% com.case & b$sound.file2 %in% com.case, ]

}  
  
#list results
if (cor.mat) return(mat) else
{c <- list(b, mat, frq.lim)
names(c) <- c("correlation.data", "max.xcorr.matrix", "frq.lim") 
 
return(c)}

}


##############################################################################################################
#' alternative name for \code{\link{xcorr}}
#'
#' @keywords internal
#' @details see \code{\link{xcorr}} for documentation. \code{\link{xcorr}} will be deprecated in future versions.
#' @export

x_corr <- xcorr
