#' Find cross-correlation peaks  
#' 
#' \code{find_peaks} find peaks in cross-correlation scores from \code{\link{xcorr}} 
#' @usage find_peaks(xc.output, parallel = 1, cutoff = 0.4, path = NULL, pb = TRUE, 
#' max.peak = FALSE, output = "data.frame")
#' @param xc.output output of \code{\link{xcorr}} after setting \code{output = "list"}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param cutoff Numeric vector of length 1 with a value between 0 and 1 specifying the correlation cutoff for detecting peaks. Default is 0.4.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param max.peak Logical argument to control whether only the peak with the highest correlation value is returned (if TRUE; cutoff will be ignored). Default is \code{FALSE}.
#' @param output Character vector of length 1 to determine if only the detected peaks are returned ('cormat') or a list ('list') containing 1) the peaks  and 2) a data frame with correlation values at each sliding step for each comparison. The list, which is also of class 'peaks.output', can be used to graphically explore detections using \code{\link{lspec}}.
#' @return The function returns a data frame with time and correlation score for the  
#' detected peaks.
#' @export
#' @name find_peaks
#' @details This function finds cross-correlation peaks along signals (analogous to \code{\link[monitoR]{findPeaks}}).
#' @examples
#' {
#' # load data
#' data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table2", "comp_matrix"))
#' 
#' # save sound files
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav")) 
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' 
#' # run cross-correlation
#' xc.output <- xcorr(X = lbh_selec_table2, output = "list", 
#' compare.matrix = comp_matrix, path = tempdir())
#' 
#' # find peaks
#' pks <- find_peaks(xc.output = xc.output, path = tempdir())
#' }
#' @seealso \code{\link{autodetec}}, \code{\link[monitoR]{findPeaks}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' H. Khanna, S.L.L. Gaunt & D.A. McCallum (1997). Digital spectrographic cross-correlation: tests of sensitivity. Bioacoustics 7(3): 209-234
#' }
# last modification on jan-03-2020 (MAS)
find_peaks <- function(xc.output, parallel = 1, cutoff = 0.4, path = NULL, pb = TRUE, max.peak = FALSE, output = "data.frame") 
{
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(find_peaks)
  
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
      stop("'path' provided does not exist") else
        path <- normalizePath(path)
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS and no soz
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # loop over scores of each dyad
  pks <- pbapply::pblapply(unique(xc.output$scores$dyad), cl = cl, FUN = function(i) {
    
    # extract data for a dyad
    dat <- xc.output$scores[xc.output$scores$dyad == i, ]

    ## get peaks as the ones higher than previous and following scores  
    pks <- dat[c(FALSE, diff(dat$score) > 0) & c(rev(diff(rev(dat$score)) > 0), FALSE) & dat$score > cutoff, , drop = FALSE]
    
    # get the single highest peak
    if (max.peak)
      pks <- dat[which.max(dat$score), , drop = FALSE]
    
    return(pks)
  })
  
  # put results in a data frame
  peaks <- do.call(rbind, pks)

  # relabel rows
  if (nrow(peaks) > 0)
  {  rownames(peaks) <- 1:nrow(peaks)
  
  # remove dyad column
  peaks$dyad <- NULL
  
  #### name as in a warbleR selection table
  # remove selec info at the end
  peaks$sound.files <- substr(peaks$sound.files, start = 0, regexpr("\\-[^\\-]*$", peaks$sound.files) - 1)
  
  #### add start and end
  # add template column to selection table in xc.output
  Y <- xc.output$selection.table
  Y$template <- paste(Y$sound.files, Y$selec, sep = "-")
  
  # Y <- Y[Y$template %in% comp_mat[, 1], ]
  
  # add start as time - half duration of template
  peaks$start <- sapply(1:nrow(peaks), function(i){
   
    peaks$time[i] - 
    ((Y$end[Y$template == peaks$template[i]] - 
    Y$start[Y$template == peaks$template[i]])  / 2)
    
  })
  
  # add end as time + half duration of template
  peaks$end <- sapply(1:nrow(peaks), function(i){
    
    peaks$time[i] + 
      ((Y$end[Y$template == peaks$template[i]] - 
         Y$start[Y$template == peaks$template[i]]) / 2)
    
  })
  
  # add selec labels
  peaks$selec <- 1

  if (nrow(peaks) > 1)
  for(i in 2:nrow(peaks)) 
  if (peaks$sound.files[i] == peaks$sound.files[i - 1])
        peaks$selec[i] <- peaks$selec[i - 1] + 1

  # sort columns in a intuitive order
  peaks <- sort_colms(peaks)
  
  # output results
  if (output == "data.frame") return(peaks) else{
    
    output_list <- list(selection.table = peaks, scores = xc.output$scores,  cutoff = cutoff)
    
    class(output_list) <- c("list", "find_peaks.output")
    
    return(output_list)
  }
  
  } else {
  
  # no detections    
  write(file = "", x = "no peaks above cutoff were detected")  
  
  return(NULL)  
  }
}
