#' Time-frequency cross-correlation 
#' 
#' \code{cross_correlation} estimates the similarity of two sound waves by means of time-frequency cross-correlation
#' @usage cross_correlation(X, wl = 512, bp = "pairwise.freq.range", ovlp = 70, 
#' dens = NULL, wn = 'hanning', cor.method = "pearson", parallel = 1, 
#' path = NULL, pb = TRUE, na.rm = FALSE, cor.mat = NULL, output = "cor.mat", 
#' templates = NULL, surveys = NULL, compare.matrix = NULL, type = "fourier",
#'  nbands = 40, method = 1)
#' @param  X 'selection_table', 'extended_selection_table' or data frame containing columns for sound files (sound.files), 
#' selection number (selec), and start and end time of signal (start and end). 
#' All selections must have the same sampling rate. 
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#' is 512.
#' @param bp A numeric vector of length 2 for the lower and upper limits of a 
#'   frequency bandpass filter (in kHz) or "pairwise.freq.range" (default) to indicate that values in lowest bottom.freq
#'   and highest top.freq columns for the signals involved in a pairwise comparison will be used as bandpass limits.  
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#' consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70. High values of ovlp 
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
#' @param cor.mat DEPRECATED. Use 'compare.matrix' instead.
#' @param output Character vector of length 1 to determine if only the correlation matrix is returned ('cor.mat', default) or a list ('list') containing 1) the correlation matrix and 2) a data frame with correlation values at each sliding step for each comparison. The list, which is also of class 'xcorr.output', can be used to find detection peaks with \code{\link{find_peaks}} or to graphically explore detections using \code{\link{lspec}}.
#' @param templates Character vector with the selections in 'X' to be used as templates for cross-correlation detection. To refer to specific selections in 'X' the user must use the format "sound.file-selec" (e.g. "file1.wav-1"). If only the sound file name is included then the entire sound file is used as template.
#' @param surveys Character vector with the selections in 'X' to be used as surveys for cross-correlation detection. To refer to specific selections in 'X' the user must use the format "sound.file-selec" (e.g. "file1.wav-1"). If only the sound file name is included then the entire sound file is used as survey.
#' @param compare.matrix A character matrix with 2 columns indicating the selections to be compared (column 1 vs column 2). The columns must contained the ID of the selection, which is given by combining the 'sound.files' and 'selec' columns of 'X',  separated by '-' (i.e. \code{paste(X$sound.files, X$selec, sep = "-")}). Default is \code{NULL}. If the 'selec' ID is not supplied then the entire sound file is used instead. If 'compare.matrix' is supplied only those comparisons will be calculated (as opposed to all pairwise comparisons as the default behavior) and the output will be a data frame composed of the supplied matrix and the correspondent cross-correlation values. Note that 'method' is automatically set to 2 (create spectrograms on the fly) when 'compare.matrix' is supplied but can be set back to 1. When supplied 'output' is forced to be 'list'. 
#' @param type A character vector of length 1 specifying the type of cross-correlation; either "fourier" (i.e. spectrographic cross-correlation using Fourier transform; internally using \code{\link[seewave]{spectro}}; default) or "mfcc" (Mel cepstral coefficient spectrogram cross-correlation; internally using \code{\link[tuneR]{melfcc}}).
#' @param nbands Numeric vector of length 1 controlling the number of warped spectral bands to calculate when using \code{type = "mfcc"} (see \code{\link[tuneR]{melfcc}}). Default is 40. 
#' @param method Numeric vector of length 1 to control the method used to create spectrogram matrices. Two option are available:
#' \itemize{
#'    \item \code{1}:  matrices are created first (keeping them internally as a list) and cross-correlation is calculated on a second step. Note that this method may require lots of memory if selection and or sound files are large. 
#'    \item \code{2}: matrices are created "on the fly" (i.e. at the same time that cross-correlation is calculated). More memory efficient but may require extracting the same matrix several times, which will affect performance. Note that when using this method the function does not check if sound files have the same sampling rate which if not, may produce an error.
#'    }
#' @return If \code{output = "cor.mat"} the function returns a matrix with 
#' the maximum (peak) correlation for each pairwise comparison (if 'compare.matrix' is not supplied) or the peak correlation for each comparison in the supplied 'compare.matrix'. Otherwise it will return a list that includes 1) a matrix with 
#' the maximum correlation for each pairwise comparison ('max.xcorr.matrix') and 2) a data frame with the correlation statistic for each "sliding" step ('scores').
#' @export
#' @name cross_correlation
#' @details This function calculates the pairwise similarity of multiple signals by means of time-frequency cross-correlation. Fourier or Mel frequency ceptral coefficients spectrograms can be used as time-frequency representations of sound. 
#' This method "slides" the spectrogram of the shortest selection over the longest one calculating a correlation of the amplitude values at each step.
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
#' 
#' # run cross correlation on spectrograms (SPCC)
#' xcor <- cross_correlation(X = lbh_selec_table, wl = 300, ovlp = 90, path = tempdir())
#'
#' # run cross correlation on Mel cepstral coefficients (mfccs)
#' xcor <- cross_correlation(X = lbh_selec_table, wl = 300, ovlp = 90, path = tempdir(), type = "mfcc")
#'   
#' # using the 'compare.matrix' argument to specify pairwise comparisons
#' # create matrix with ID of signals to compare
#' cmp.mt <- cbind(
#' paste(lbh_selec_table$sound.files[1:10], lbh_selec_table$selec[1:10], sep = "-"), 
#' paste(lbh_selec_table$sound.files[2:11], lbh_selec_table$selec[2:11], sep = "-"))
#' 
#' # run cross-correlation on the selected pairwise comparisongs
#' xcor <- cross_correlation(X = lbh_selec_table, compare.matrix = cmp.mt, 
#' wl = 300, ovlp = 90, path = tempdir())
#' }
#' @seealso \code{\link{mfcc_stats}}, \code{\link{spectro_analysis}}, \code{\link{freq_DTW}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' H. Khanna, S.L.L. Gaunt & D.A. McCallum (1997). Digital spectrographic cross-correlation: tests of sensitivity. Bioacoustics 7(3): 209-234
#' 
#' Lyon, R. H., & Ordubadi, A. (1982). Use of cepstra in acoustical signal analysis. Journal of Mechanical Design, 104(2), 303-306.
#' }
# last modification on jan-03-2020 (MAS)

cross_correlation <- function(X = NULL, wl = 512, bp = "pairwise.freq.range", ovlp = 70, dens = NULL, 
                  wn ='hanning', cor.method = "pearson", parallel = 1, 
                  path = NULL, pb = TRUE, na.rm = FALSE, cor.mat = NULL, output = "cor.mat",
                  templates = NULL, surveys = NULL,
                  compare.matrix = NULL, type = "fourier", nbands = 40, method = 1)
{
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(cross_correlation)
  
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
    if (!dir.exists(path) & !is_extended_selection_table(X)) 
      stop("'path' provided does not exist") else
        path <- normalizePath(path)
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X), is_extended_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table' or 'extended_selection_table'")
  
  # if is extended all should have the same sampling rate
  if (is_extended_selection_table(X) & length(unique(attr(X, "check.results")$sample.rate)) > 1) stop("all wave objects in the extended selection table must have the same sampling rate (they can be homogenized using resample_est_waves())")
  
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end") 
  
  #stop if only 1 selection
  if (nrow(X) == 1 & is.null(compare.matrix)) stop("you need more than one selection to do cross-correlation")
  
  # bp needed when no bottom and top freq
  if (bp[1] == "pairwise.freq.range" & is.null(X$bottom.freq))  stop("'bp' must be supplied when no frequency range columns are found in 'X' (bottom.freq & top.freq)")
  
  # stop if no bp
  if(is.null(bp[1])) stop("'bp' must be supplied")
  
  # dens deprecated
  if (!is.null(dens))  write(file = "", x = "'dens' has been deprecated and will be ignored")
  
  # dens deprecated
  if (!is.null(cor.mat))  write(file = "", x = "'dens' has been deprecated and will be ignored")
  
  #check output
  if (!any(output %in% c("cor.mat", "list"))) stop("'output' must be either 'cor.mat' or 'list'")  
  
  #if wl is not vector or length!=1 stop
  if (!is.numeric(wl)) stop("'wl' must be a numeric vector of length 1") else {
    if (!is.vector(wl)) stop("'wl' must be a numeric vector of length 1") else{
      if (!length(wl) == 1) stop("'wl' must be a numeric vector of length 1")}} 
  
  #if ovlp is not vector or length!=1 stop
  if (!is.numeric(ovlp)) stop("'ovlp' must be a numeric vector of length 1") else {
    if (!is.vector(ovlp)) stop("'ovlp' must be a numeric vector of length 1") else{
      if (!length(ovlp) == 1) stop("'ovlp' must be a numeric vector of length 1")}} 
  
  # extended selection table only need sound files in the working directory when doing detection
  if (!is_extended_selection_table(X) | is_extended_selection_table(X) & !is.null(templates) | is_extended_selection_table(X) & !is.null(compare.matrix)){
    #return warning if not all sound files were found
    fs <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)
    if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files))) 
      write(file = "", x = paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% fs)])), 
                                 "sound file(s) not found"))
    
    #count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% fs) 
    if (length(d) == 0){
      stop("The sound files are not in the working directory")
    }  else {
      X <- X[d, ]
    }
  }
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # check sampling rate is the same for all selections if not a selection table
  if (is_extended_selection_table(X) & length(unique(attr(X, "check.results")$sample.rate)) > 1) stop("sampling rate must be the same for all selections")
  
  # add selection id column to X
  X$selection.id <- paste(X$sound.files, X$selec, sep = "-")
  
  # if templates were supplied
  if (!is.null(templates))
    if (!all(templates %in% X$selection.id))
      stop("not all templates are referenced in 'X'")
  
  # check surveys
  if (!all(surveys %in% list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)))
    stop("not all sound files in 'surveys' are found in the working directory or 'path'")
  
  # if templates were supplied but no compare.matrix use files in working directory
  if (!is.null(templates) & is.null(compare.matrix))
    if (is.null(surveys))
      compare.matrix <- data.frame(X1 = rep(templates, length(fs)), X2 = fs) else
        compare.matrix <- data.frame(X1 = rep(templates, length(surveys)), X2 = surveys)
  
  
  if (!is.null(compare.matrix))
    output <- "list"
      
  # keep only selections in supplied compare.matrix to improve performance
  if (!is.null(compare.matrix)){ 
    X <- X[X$selection.id %in% unique(unlist(c(compare.matrix))), , drop = FALSE]
 
  # set method to 2 if not provided in call
  if (!any(names(call.argms) == "method"))
    method <- 2
  }
   
  # define number of steps in analysis to print message
  if (pb){
    steps <- getOption("int_warbleR_steps")
    
      if (steps[2] > 0) 
      {
        current.step <- steps[1]
        total.steps <- steps[2] 
      } else {
              if(method == 1) total.steps <- 2  else  total.steps <- 1
              
              current.step <- 1
              }
  } 
    
  # generate all possible combinations of selections, keep one with the original order of rows to create cor.table output
  if (is.null(compare.matrix))
    spc.cmbs.org <- spc.cmbs <- t(combn(X$selection.id, 2)) else
    {
      # if all selections in compare.matrix are from X
      if (all(unlist(c(compare.matrix)) %in% X$selection.id))
        spc.cmbs.org <- spc.cmbs <- compare.matrix else {
          
          # if some are complete sound files
          entire.sf <- setdiff(unlist(c(compare.matrix)), X$selection.id)
          
          # get duration of files
          wvdr <- info_wavs(path = path)
          
          wvdr <- wvdr[wvdr$sound.files %in% entire.sf, ]
          
          # put it in a data frame
          names(wvdr)[2] <- "end"
          wvdr$start <- 0
          wvdr$selec <- "entire.file"    
          wvdr$selection.id <- paste(wvdr$sound.files, wvdr$selec, sep = "-")
          
          out <- lapply(1:nrow(wvdr), function(x) {
            
            # get selection that are compared against each complete sound file
            sls <- setdiff(c(compare.matrix[compare.matrix[, 1] %in% wvdr$sound.files[x] | compare.matrix[, 2] %in% wvdr$sound.files[x], ]), wvdr$sound.files[x])
            
            # get freq range as min bottom and max top of all selections to be compared against a given sound file
            # channel is also the lowest
            suppressWarnings(df <- data.frame(
              wvdr[x, ], 
              channel = min(X$channel[X$selection.id %in% sls]),
              bottom.freq = min(X$bottom.freq[X$selection.id %in% sls]),
              top.freq = max(X$top.freq[X$selection.id %in% sls])
            )) 
            
            return(df)
            
          })
          
          wvdr <- do.call(rbind, out)        
          
          if (length(unique(wvdr$sample.rate)) > 1)
            stop("not all sound files have the same sampling rate")
          
          if (is_extended_selection_table(X))  
            if (unique(wvdr$sample.rate) != unique(attr(X, "check.results")$sample.rate))
              stop("not all sound files have the same sampling rate than the wave objects in the extended selection table")
          
          #get intersect of column names
          int.nms <- intersect(names(X), names(wvdr))
            
          X <- rbind(as.data.frame(X)[, int.nms, drop = FALSE], wvdr[, int.nms])
          
          # change complete file names in compare matrix
          for (i in entire.sf)
            compare.matrix[compare.matrix == i] <- paste(i, "entire.file", sep = "-")
          
          # set new matrices to allow changes down stream
          spc.cmbs.org <- spc.cmbs <- compare.matrix
        }
      
    }

  # function to get spectrogram matrices
  spc_FUN <- function(j, pth, W, wlg, ovl, w, nbnds) {
    
    clp <- warbleR::read_sound_file(X = W, index = j, path = pth)
    
    if (type == "fourier" | type == "spectrogram")
      spc <- seewave::spectro(wave = clp, wl = wlg, ovlp = ovl, wn = w, plot = FALSE, fftw = TRUE, norm = TRUE)
    
    if (type == "mfcc"| type == "mel")
    {
      # calculate MFCCs
      spc <- melfcc(clp, nbands = nbnds, hoptime = (wlg / clp@samp.rate) * ((if(ovl > 0) ovl else 100) / 100), wintime =  wlg / clp@samp.rate, dcttype = "t3", fbtype = "htkmel", spec_out = TRUE)
      
      # change name of target element so it maches spectrogram output names
      names(spc)[2] <- "amp" 
      
      # and flip
      spc$amp <- t(spc$amp)
      
      # add element with freq values for each band
      spc$freq <- seq(0, clp@samp.rate / 2000, length.out = nbnds)
    }
    
    # replace inf by NA
    spc$amp[is.infinite(spc$amp)] <- NA
    
    return(spc)
  }
  
  if (method == 1){
  #create spectrograms
  if (pb) 
      write(file = "", x = paste0("creating spectrogram matrices (step ", current.step," of ", total.steps,"):"))
  
   
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # get spectrogram for each selection
  spcs <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(X), cl = cl, FUN = function(e) spc_FUN(j = e, pth = path, W = X, wlg = wl, ovl = ovlp, w = wn, nbnds = nbands))
  
  # check sampling rate is the same for all selections if not a selection table
  if (!is_extended_selection_table(X) & length(unique(sapply(spcs, function(x) length(x$freq)))) > 1) stop("sampling rate must be the same for all selections")
  
    # add selection name
  names(spcs) <- X$selection.id
 }
  
  # create function to calculate correlation between 2 spectrograms
  XC_FUN <- function(spc1, spc2, b = bp, cm = cor.method){
    
    # filter frequency ranges
    spc1$amp <- spc1$amp[spc1$freq >= b[1] & spc1$freq <= b[2], ]
    spc2$amp <- spc2$amp[which(spc2$freq >= b[1] & spc2$freq <= b[2]), ]

    # define short and long envelope for sliding one (short) over the other (long)
    if(ncol(spc1$amp) > ncol(spc2$amp)) {
      lg.spc <- spc1$amp
      shrt.spc <- spc2$amp
    } else {
      lg.spc <- spc2$amp
      shrt.spc <- spc1$amp
    }
    
    # get length of shortest minus 1 (1 if same length so it runs a single correlation)
    shrt.lgth <- ncol(shrt.spc) - 1
    
    # steps for sliding one signal over the other  
    stps <- ncol(lg.spc) - ncol(shrt.spc)
    
    # set sequence of steps, if <= 1 then just 1 step
    if (stps <= 1) stps <- 1 else stps <- 1:stps 
    
    # calculate correlations at each step
    cors <- sapply(stps, function(x, cor.method = cm) {
      warbleR::try_na(cor(c(lg.spc[, x:(x + shrt.lgth)]), c(shrt.spc), method = cm, use ='pairwise.complete.obs'))
    })
    
    return(cors)
  }
  
  # shuffle spectrograms index so are not compared in sequence, which makes progress bar more precise when some selections are much longer than others
  ord.shuf <- sample(1:nrow(spc.cmbs))
  
  spc.cmbs <- spc.cmbs[ord.shuf, , drop = FALSE]
  
  #run cross-correlation
  if (pb) 
    write(file = "", x = paste0("running cross-correlation (step ", current.step + 1," of ", total.steps,"):"))
        
  # set parallel cores
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # get correlation
  xcrrs <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(spc.cmbs), cl = cl, FUN = function(j, BP = bp, cor.meth = cor.method) {
    
    if (BP[1] %in% c("pairwise.freq.range", "frange"))
    BP <- c(min(X$bottom.freq[X$selection.id %in% spc.cmbs[j, ]]), max(X$top.freq[X$selection.id %in% spc.cmbs[j, ]]))
    
    # extract amplitude matrices
    if (method == 1) {
    spc1 <- spcs[[spc.cmbs[j, 1]]]
    spc2 <- spcs[[spc.cmbs[j, 2]]]
    } 
    
    if (method == 2) {
      spc1 <- spc_FUN(j = which(X$selection.id == spc.cmbs[j, 1]), pth = path, W = X, wlg = wl, ovl = ovlp, w = wn, nbnds = nbands)
  
      spc2 <- spc_FUN(j = which(X$selection.id == spc.cmbs[j, 2]), pth = path, W = X, wlg = wl, ovl = ovlp, w = wn, nbnds = nbands)
    }
    
    # get cross correlation
    XC_FUN(spc1 = spc1, spc2 = spc2, b = BP, cm = cor.meth)
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
  colnames(mat) <- rownames(mat) <- X$selection.id
  
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
} else {
  mat <- data.frame(compare.matrix, score = mx.xcrrs)
 if (na.rm) mat <- mat[!is.infinite(mat$score), ]
  }
  ## create correlation data matrix (keeps all correlation values, not only the maximum)
  # time is the one of the longest selection as the shortest is used as template 
  if (output == "list") {
    cor.lst <- lapply(1:nrow(spc.cmbs.org), function(x) 
      {
      # get duration of both selections
      durs <- c(
        (X$end - X$start)[X$selection.id == spc.cmbs.org[x, 1]],
                (X$end - X$start)[X$selection.id == spc.cmbs.org[x, 2]]
        )
      
      # put in a data frame
      df <- suppressWarnings(data.frame(dyad = paste(spc.cmbs.org[x, ], collapse = "/"),
                       sound.files = spc.cmbs.org[x, which.max(durs)], 
                       template = spc.cmbs.org[x, which.min(durs)], 
                       time = if (!is.null( xcrrs[[x]])) c(X$start[X$selection.id == spc.cmbs.org[x, 1]], X$start[X$selection.id == spc.cmbs.org[x, 2]])[which.max(durs)] + seq(min(durs)/2, max(durs) - min(durs)/2, length.out = length(xcrrs[[x]])) else NA, 
                       score = if (!is.null(xcrrs[[x]])) xcrrs[[x]] else NA
                      ))
          
      return(df)
      })
     
    # put together in a single dataframe
    cor.table <- do.call(rbind, cor.lst)
    
    # remove missing values
    if (na.rm) 
    {      
      if (exists("com.case"))
      cor.table <- cor.table[cor.table$sound.files %in% com.case & cor.table$template %in% com.case, ]

    errors <- cor.table[is.na(cor.table$score), ]    
    errors$score <- NULL
    
    cor.table <- cor.table[!is.infinite(cor.table$score), ]
    }
  } 
  
  #list results
  if (output == "cor.mat") return(mat) else{
    
    output_list <- list(
      max.xcorr.matrix = mat, 
      scores = cor.table, 
      org.selection.table = X, 
      hop.size.ms = warbleR::read_sound_file(X, 1, header = TRUE, path = path)$sample.rate / wl, 
      errors = if (na.rm) errors else NA,
      parameters = lapply(call.argms, eval),
      call = base::match.call(),
      spectrogram = type,
      warbleR.version = packageVersion("warbleR")
      )
    
    class(output_list) <- c("list", "xcorr.output")
    
    return(output_list)
      }
  
}


##############################################################################################################
#' alternative name for \code{\link{cross_correlation}}
#'
#' @keywords internal
#' @details see \code{\link{cross_correlation}} for documentation. \code{\link{xcorr}} will be deprecated in future versions.
#' @export

xcorr <- cross_correlation


##############################################################################################################

#' print method for class \code{xcorr.output}
#'
#' @param x Object of class \code{xcorr.output}, generated by \code{\link{cross_correlation}}.
#' @param ...	 further arguments passed to or from other methods. Ignored when printing selection tables.
#' @keywords internal
#'
#' @export
#' 

print.xcorr.output <- function(x, ...) {
  
  cat(crayon::cyan(paste("Object of class", crayon::bold("'xcorr.output' \n"))))
  
  cat(crayon::silver(paste(crayon::bold("\nContains: \n"), "The output of a detection routine from the following", crayon::italic("cross_correlation()"), "call: \n")))
  
  cll <- paste0(deparse(x$call))
  cat(crayon::silver(crayon::italic(gsub("    ", "", cll), "\n")))
  
  max.scores <- x$max.xcorr.matrix
  max.scores$X2 <- gsub("-entire.file", "", max.scores$X2)
  max.scores$score <- NULL
  names(max.scores) <- c("template", "survey")
  
  cat(crayon::silver(paste("\n The routine was run on", x$spectrogram, "spectrograms using the following templates and surveys (selections or files in which templates were looked for): \n")))

  #print data frame
  #define columns to show
  cols <- if (ncol(max.scores) > 6)  1:6 else 1:ncol(max.scores)
  
  kntr_tab <- knitr::kable(head(max.scores[, cols]), escape = FALSE, digits  = 4, justify = "centre", format = "pipe")
  
  for(i in 1:length(kntr_tab)) cat(crayon::silver(paste0(kntr_tab[i], "\n")))
  
  if (nrow(max.scores) > 6) cat(crayon::silver(paste0(if (ncol(max.scores) <= 6) "..." else "", " and ", nrow(max.scores) - 6, " more row(s) (run ", crayon::italic(paste0(gsub("[^[:alnum:]=\\.]","", x$call[2]), "$max.xcorr.matrix[, 1:2]")), " to see it all) \n")))  
  

  cat(crayon::silver(paste(crayon::bold("\nIncludes:"), "\n* A data frame ('max.xcorr.matrix') with the highest correlation value for each pair of templates and surveys \n")))
  
  cat(crayon::silver("\n* A data frame ('scores',", nrow(x$scores), "rows) with the cross correlation scores for each of the", nrow(max.scores), "template/survey combination(s) \n"))
  cat(crayon::silver(paste("\n* A selection table data frame ('org.selection.table') referencing the templates location in sound files \n")))
  
  cat(crayon::silver(paste("\n Use", crayon::bold(crayon::italic("full_spectrograms()")), "to plot cross_correlation scores along spectrograms \n")))
  
  cat(crayon::silver(paste("\n Use", crayon::bold(crayon::italic("find_peaks()")), "to extract detections from this object \n")))
  
  # print warbleR version
  if (!is.null(x$warbleR.version))
    cat(crayon::silver(paste0("\n Created by warbleR ", x$warbleR.version)), "\n") else
      cat(crayon::silver("\n Created by warbleR < 1.1.27 \n"))
  
}
