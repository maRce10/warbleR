#' Spectrogram cross-correlation 
#' 
#' \code{xcorr} estimates the similarity of two spectrograms by means of spectrographic cross-correlation
#' @usage xcorr(X, wl =512, bp = "pairwise.freq.range", ovlp = 90, dens = NULL, wn='hanning', 
#' cor.method = "pearson", parallel = 1, path = NULL, pb = TRUE, na.rm = FALSE,
#'  cor.mat = TRUE, compare.matrix = NULL)
#' @param  X 'selection_table', 'extended_selection_table' or data frame containing columns for sound files (sound.files), 
#' selection number (selec), and start and end time of signal (start and end).
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#' is 512.
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz) or "pairwise.freq.range" (default) to indicate that values in lowest bottom.freq
#'   and highest top.freq columns for the signals involved in a pairwise comparison will be used as bandpass limits.  
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#' consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 90. High values of ovlp 
#' slow down the function but produce more accurate results.
#' @param dens DEPRECATED.
#' @param wn A character vector of length 1 specifying the window name as in \code{\link[seewave]{ftwindow}}. 
#' @param cor.method A character vector of length 1 specifying the correlation method as in \code{\link[stats]{cor}}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param na.rm Logical. If \code{TRUE} all NAs produced when pairwise cross-correlations failed are removed from the 
#' results. This means that all selections with at least 1 cross-correlation that failed are excluded.
#' @param cor.mat Logical. If \code{TRUE} only the correlation matrix is returned. Default is \code{TRUE}.
#' @param compare.matrix A character matrix with 2 columns indicating the selections to be compared (column 1 vs column 2). The columns must contained the ID of the selection, which is given by combining the 'sound.files' and 'selec' columns of 'X',  separated by '-' (i.e. \code{paste(X$sound.files, X$selec, sep = "-")}). Default is \code{NULL}. If supplied only those comparisons will be calculated (as opposed to all pairwise comparisons as the default behavior) and the output will be a data frame composed of the supplied matrix and the correspondent cross-correlation values.
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
#' @examples
#' {
#' #load data
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4","lbh_selec_table"))
#' 
#' #save sound files
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) 
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#' # run cross correlation
#' xcor <- xcorr(X = lbh_selec_table, wl = 300, ovlp = 90, path = tempdir())
#' 
#' # using the 'compare.matrix' argument to specify pairwise comparisons
#' # create matrix with ID of signals to compare
#' cmp.mt <- cbind(
#' paste(lbh_selec_table$sound.files[1:10], lbh_selec_table$selec[1:10], sep = "-"), 
#' paste(lbh_selec_table$sound.files[2:11], lbh_selec_table$selec[2:11], sep = "-"))
#' 
#' # run cross-correlation on the selected pairwise comparisongs
#' xcor <- xcorr(X = lbh_selec_table, compare.matrix = cmp.mt, 
#' wl = 300, ovlp = 90, path = tempdir())
#' }
#' @seealso \code{\link{mfcc_stats}}, \code{\link{specan}}, \code{\link{df_DTW}}
#' @author Marcelo Araya-Salas \email{marceloa27@@gmail.com})
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' H. Khanna, S.L.L. Gaunt & D.A. McCallum (1997). Digital spectrographic cross-correlation: tests of sensitivity. Bioacoustics 7(3): 209-234
#' }
# last modification on oct-25-2019 (MAS)

xcorr <- function(X = NULL, wl = 512, bp = "pairwise.freq.range", ovlp = 90, dens = NULL, 
                  wn ='hanning', cor.method = "pearson", parallel = 1, 
                  path = NULL, pb = TRUE, na.rm = FALSE, cor.mat = TRUE, compare.matrix = NULL)
{
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(xcorr)
  
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
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) 
      stop("'path' provided does not exist")
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  # if is extended all should have the same sampling rate
  if (is_extended_selection_table(X) & length(unique(attr(X, "check.results")$sample.rate)) > 1) stop("all wave objects in the extended selection table must have the same sampling rate (they can be homogenized using resample_est())")
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end") 
  
  #stop if only 1 selection
  if (nrow(X) == 1) stop("you need more than one selection to do cross-correlation")
  
  # bp needed when no bottom and top freq
  if (bp[1] == "pairwise.freq.range" & is.null(X$bottom.freq))  stop("'bp' must be supplied when no frequency range columns are found in 'X' (bottom.freq & top.freq)")
  
  # stop if no bp
  if(is.null(bp[1])) stop("'bp' must be supplied")
  
  # dens deprecated
  if (!is.null(dens))  write(file = "", x = "'dens' has been deprecated and will be ignored")
  
  #if wl is not vector or length!=1 stop
  if (!is.numeric(wl)) stop("'wl' must be a numeric vector of length 1") else {
    if (!is.vector(wl)) stop("'wl' must be a numeric vector of length 1") else{
      if (!length(wl) == 1) stop("'wl' must be a numeric vector of length 1")}} 
  
  #if ovlp is not vector or length!=1 stop
  if (!is.numeric(ovlp)) stop("'ovlp' must be a numeric vector of length 1") else {
    if (!is.vector(ovlp)) stop("'ovlp' must be a numeric vector of length 1") else{
      if (!length(ovlp) == 1) stop("'ovlp' must be a numeric vector of length 1")}} 
  
  if (!is_extended_selection_table(X)){
    #return warning if not all sound files were found
    fs <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)
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
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # check sampling rate is the same for all selections if not a selection table
  if (is_extended_selection_table(X) & length(unique(attr(X, "check.results")$sample.rate)) > 1) stop("sampling rate must be the same for all selections")
  
  #create spectrograms
  if (pb) write(file = "", x ="creating spectrogram matrices (step 1 of 2):")
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # keep only selections in supplied compare.matrix to improve performance
  if (!is.null(compare.matrix))
  X <- X[paste(X$sound.files, X$selec, sep = "-") %in% unique(c(compare.matrix)), ]
    
  # get spectrogram for each selection
  spcs <- pbapply::pblapply(X = 1:nrow(X), cl = cl, FUN = function(j) {
    
    clp <- warbleR::read_wave(X = X, index = j, path = path)
    
    spc <- seewave::spectro(wave = clp, wl = wl, ovlp = ovlp, wn = wn, plot = FALSE, fftw = TRUE, norm = TRUE)
    
    spc[[3]][is.infinite(spc[[3]])] <- NA
  
    return(spc)
    })
  
  # check sampling rate is the same for all selections if not a selection table
  if (!is_extended_selection_table(X) & length(unique(sapply(spcs, function(x) length(x$freq)))) > 1) stop("sampling rate must be the same for all selections")
  
    # add selection name
  names(spcs) <- paste(X$sound.files, X$selec, sep = "-")

  # create function to calculate correlation between 2 spectrograms
  XC_FUN <- function(spc1, spc2, bp = bp){
    
    # filter frequency
    spc1$amp <- spc1$amp[spc1$freq >= bp[1] & spc1$freq <= bp[2], ]
    spc2$amp <- spc2$amp[which(spc2$freq >= bp[1] & spc2$freq <= bp[2]), ]
    
    # define short and long envelope for sliding one (short) over the other (long)
    if(ncol(spc1[[3]]) > ncol(spc2[[3]])) {
      lg.spc <- spc1[[3]]
      shrt.spc <- spc2[[3]]
    } else {
      lg.spc <- spc2[[3]]
      shrt.spc <- spc1[[3]]
    }
    
    # get length of shortest minus 1 (1 if same length so it runs a single correlation)
    shrt.lgth <- ncol(shrt.spc) - 1
    
    # steps for sliding one signal over the other  
    stps <- ncol(lg.spc) - ncol(shrt.spc)
    
    # set sequence of steps, if <= 1 then just 1 step
    if (stps <= 1) stps <- 1 else stps <- 1:stps 
    
    # calculate correlations at each step
    cors <- sapply(stps, function(x) {
      warbleR::try_na(cor(c(lg.spc[, x:(x + shrt.lgth)]), c(shrt.spc), method = cor.method, use='pairwise.complete.obs'))
    })
    
    return(cors)
  }
  
  # generate all possible combinations of selections, keep one with the orignal order of rows to create cor.table output
  if (is.null(compare.matrix))
  spc.cmbs.org <- spc.cmbs <- t(combn(names(spcs), 2)) else
    spc.cmbs.org <- spc.cmbs <- compare.matrix
  
  # shuffle spectrograms index so are not compared in sequence, which makes progress bar more precise when some selections are much longer than others
  ord.shuf <- sample(1:nrow(spc.cmbs))
  
  # sampling rate must be the same
  spc.cmbs <- spc.cmbs[ord.shuf, , drop = FALSE]
  
  #run cross-correlation
  if (pb) write(file = "", x ="running cross-correlation (step 2 of 2):")
  
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # get correlation
  xcrrs <- pbapply::pblapply(X = 1:nrow(spc.cmbs), cl = cl, FUN = function(j) {
    
    if (bp[1] %in% c("pairwise.freq.range", "frange"))
    BP <- c(min(X$bottom.freq[paste(X$sound.files, X$selec, sep = "-") %in% spc.cmbs[j, ]]), max(X$top.freq[paste(X$sound.files, X$selec, sep = "-") %in% spc.cmbs[j, ]])) else BP <- bp
    
    XC_FUN(spc1 = spcs[[spc.cmbs[j, 1]]], spcs[[spc.cmbs[j, 2]]], bp = BP)
  })
  
  # order as originally
  xcrrs <- xcrrs[order(ord.shuf)]
  
  # extract maximum correlation
  mx.xcrrs <- sapply(xcrrs, max, na.rm = TRUE)
  
  # only create correlation matrix if compare matrix was not supplied
  if (is.null(compare.matrix)){
  
  #create a similarity matrix with the max xcorr
  mat <- matrix(nrow = nrow(X), ncol = nrow(X))
  mat[] <- 1
  colnames(mat) <- rownames(mat) <- paste(X$sound.files, X$selec, sep = "-")
  
  # add max correlations
  mat[lower.tri(mat, diag=FALSE)] <- mx.xcrrs
  mat <- t(mat)
  mat[lower.tri(mat, diag=FALSE)] <- mx.xcrrs

  # remove NA's if any
  if (na.rm)
  {
    com.case <- intersect(rownames(mat)[stats::complete.cases(mat)], colnames(mat)[stats::complete.cases(t(mat))])
    if (length(which(is.na(mat))) > 0) 
      warning(paste(length(which(is.na(mat))), "pairwise comparisons failed and were removed"))
    
    #remove them from mat
    mat <- mat[rownames(mat) %in% com.case, colnames(mat) %in% com.case]
    if (nrow(mat) == 0) stop("Not selections remained after removing NAs (na.rm = TRUE)")
  } 
  
  # create correlation data matrix (keeps all correlation values, not only the maximum)
  if (!cor.mat) {
    cor.lst <- lapply(1:nrow(spc.cmbs.org), function(x) 
      data.frame(
        dyad = paste(spc.cmbs.org[x, ], collapse = "/"), 
        sound.file1 = spc.cmbs.org[x, 1], 
        sound.file2 = spc.cmbs.org[x, 2], 
        time = seq(0, length(xcrrs[[x]]) * spcs[[1]]$time[2], length.out = length(xcrrs[[x]])),
        score = xcrrs[[x]]))
  
    # put together in a single dataframe
    cor.table <- do.call(rbind, cor.lst)
    
    # remove missing values
  if (na.rm) 
    cor.table <- cor.table[cor.table$sound.file1 %in% com.case & cor.table$sound.file2 %in% com.case, ]
  } 
  
  #list results
  if (cor.mat) return(mat) else
  return(list(correlation.data = cor.table, max.xcorr.matrix = mat))
} else
  return(data.frame(compare.matrix, score = mx.xcrrs))
  
}


##############################################################################################################
#' alternative name for \code{\link{xcorr}}
#'
#' @keywords internal
#' @details see \code{\link{xcorr}} for documentation. \code{\link{xcorr}} will be deprecated in future versions.
#' @export

x_corr <- xcorr
