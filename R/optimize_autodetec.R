#' Optimize the detection of signals based on a-priori detections
#' @usage optimize_autodetec(X, Y, threshold = 10, power = 1, wl = 512, ssmooth = 0, 
#' hold.time = 0, mindur = NULL, maxdur = NULL, parallel = 1, by.sound.file = FALSE, 
#' bp = NULL, path = NULL)
#' @param X 'selection_table' object or a data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). \strong{It should contain the selections that will be used for detection optimization}.
#' @param Y An object of 'autodetec.output' in which to optimize detections. Must refer to the same sound files as in 'X'. Optional.
#' @param threshold A numeric vector of length 1 specifying the amplitude threshold for detecting
#'   signals (in \%). \strong{Several values can be supplied for optimization}.
#' @param power A numeric vector of length 1 indicating a power factor applied to the amplitude envelope. Increasing power will reduce low amplitude modulations and increase high amplitude modulations, in order to reduce background noise. Default is 1 (no change). \strong{Several values can be supplied for optimization}.
#' @param wl A numeric vector of length 1 specifying the window used internally by
#' \code{\link[seewave]{ffilter}} for bandpass filtering (so only applied when 'bp' is supplied). Default is 512.
#' \strong{Several values can be supplied for optimization}.
#' @param ssmooth A numeric vector of length 1 to smooth the amplitude envelope
#'   with a sum smooth function. Default is 0. Note that smoothing is applied before thinning (see 'thinning' argument). \strong{Several values can be supplied for optimization}.
#' @param hold.time Numeric vector of length 1. Specifies the time range at which selections will be merged (i.e. if 2 selections are separated by less than the specified hold.time they will be merged in to a single selection). Default is \code{0}. \strong{Several values can be supplied for optimization}.
#' @param mindur Numeric vector of length 1 giving the shortest duration (in
#'   seconds) of the signals to be detected. It removes signals below that
#'   threshold. \strong{Several values can be supplied for optimization}.
#' @param maxdur Numeric vector of length 1 giving the longest duration (in
#'   seconds) of the signals to be detected. It removes signals above that
#'   threshold. \strong{Several values can be supplied for optimization}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param by.sound.file Logical to control if diagnostics are calculated for each sound file independently (\code{TRUE}) or for all sound files combined (\code{FALSE}, default).
#' @param bp Numeric vector of length 2 giving the lower and upper limits of a
#'   frequency bandpass filter (in kHz). Default is \code{NULL}.
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used. Only needed if 'Y' is not supplied.
#' @return A data frame in which each row shows the result of a detection job with a particular combination of tuning parameters (including in the data frame). It also includes the following diagnostic metrics:
#' \itemize{
#'  \item \code{true.positives}: number of detections that correspond to signals referenced in 'X'. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'X'. 
#'  \item \code{false.positives}: number of detections that don't match any of the signals referenced in 'X'. In a perfect detection routine it should be 0.    
#'  \item \code{split.positives}: number of signals referenced in 'X' that were overlapped by more than 1 detection (i.e. detections that were split). In a perfect detection routine it should be 0.  
#'  \item \code{mean.duration.true.positives}: mean duration of true positives (in s).  
#'  \item \code{mean.duration.false.positives}: mean duration of false positives (in s). 
#'  \item \code{proportional.time.true.positives}: ratio of total duration of true positives to the total duration of signals referenced in 'X'. In a perfect detection routine it should be 1.
#'  \item \code{sensitivity}: Proportion of signals referenced in 'X' that were detected. In a perfect detection routine it should be 1.
#'  \item \code{specificity}: Proportion of detections that correspond to signals referenced in 'X' that were detected. In a perfect detection routine it should be 1.
#'  } 
#' @export
#' @name optimize_autodetec
#' @details This function takes a selections data frame or 'selection_table' ('X') and the output of a \code{\link{autodetec}}  routine ('Y') and estimates the detection performance for different detection parameter combinations. This is done by comparing the detection position in time of the reference selections in 'X'. The function returns several diagnostic metrics to allow user to determine which parameter values provide a detection that more closely matches the selections in 'X'. Those parameters can be later used for performing a more efficient \code{\link{autodetec}}.
#'
#' @examples{
#' # Save to temporary working directory
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#' 
#' # try optimization with 2 tresholds
#' optimize_autodetec(X = selec.table, threshold = c(10, 15), path = tempdir())
#' 
#' # this time by each sound file and 3 thresholds
#' optimize_autodetec(X = selec.table, threshold = c(5, 10, 15), by.sound.file = TRUE, 
#' path = tempdir())
#' 
#' # run autodetec with output list
#' ad <- autodetec(output = "list", thinning = 1 / 10, ssmooth = 300, path = tempdir())
#' optimize_autodetec(X = selec.table, Y = ad, threshold = c(5, 10, 15), path = tempdir())
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}).
#last modification on dec-21-2020 (MAS)
optimize_autodetec <- function(X, Y = NULL, threshold = 10, power = 1, wl = 512, ssmooth = 0, hold.time = 0, mindur = NULL, maxdur = NULL, parallel = 1, by.sound.file = FALSE, bp = NULL, path = NULL){
 
      if (!is.null(Y)) {
        if (!is(Y, "autodetec.output")) stop("'Y' must be and object of class 'autodetec.ouput'")
        }
  
      exp_grd <- expand.grid(threshold = threshold, wl = wl, power = power, ssmooth = ssmooth, hold.time = hold.time, mindur = if(is.null(mindur)) -Inf else mindur, maxdur = if(is.null(maxdur)) Inf else maxdur)
      
      # function to calculate diagnostics  
      grid_FUN <- function(exp_grd, X, Y){
        
        # subset to sound files in X
        if (!is.null(Y)){
        Y$selection.table <- Y$selection.table[Y$selection.table$sound.files %in% X$sound.files, ]
        Y$envelopes <- Y$envelopes[Y$envelopes$sound.files %in% X$sound.files, ]
        Y$org.selection.table <- Y$org.selection.table[Y$org.selection.table$sound.files %in% X$sound.files, ]
        # set flist as null
        flist <- NULL
        } else flist <- unique(as.character(X$sound.files))
        
        grid_results <- lapply(seq_len(nrow(exp_grd)), function(x){
          
          ad <- warbleR::autodetec(X = Y, wl = exp_grd$wl[x], threshold = exp_grd$threshold[x], ssmooth = exp_grd$ssmooth[x], mindur = exp_grd$mindur[x], maxdur = exp_grd$maxdur[x], parallel = parallel, pb = FALSE, power = exp_grd$power[x], hold.time = exp_grd$hold.time[x], bp = bp, path = path, flist = flist)
          
          ad$..row.id <- 1:nrow(ad)    
          
          ad <- ad[!is.na(ad$start), ]
          
          if (nrow(ad) > 0){
            #look at detections matching 1 training selection at the time
            performance_l <- lapply(unique(X$sound.files), function(z){
            
            # get subset for that sound file
            W <- X[X$sound.files == z, ]            
            
            detections_l <- lapply(seq_len(nrow(W)), function(y){
            
              ad[ad$sound.files %in% W$sound.files[y] & (ad$start >= W$start[y] & ad$start < W$end[y]) | 
                   ad$sound.files %in% W$sound.files[y] & (ad$end > W$start[y] & ad$end <= W$end[y]) | 
                   ad$sound.files %in% W$sound.files[y] & (ad$start <= W$start[y] & ad$end >= W$end[y]) | 
                   ad$sound.files %in% W$sound.files[y]  & (ad$start >= W$start[y] & ad$end  <= W$end[y]), ]  
            })
            
            detections <- do.call(rbind, detections_l)  
            
            result <- if (nrow(detections > 0))
              data.frame(
                count = nrow(detections),
                prop.time = sum(detections$end - detections$start) / sum(W$end - W$start),
                mean.duration.true.positives = mean(detections$end - detections$start),
                mean.duration.false.positives = mean((ad$end - ad$start)[(!ad$..row.id %in% detections$..row.id) & ad$sound.files == z]),
                split.positives = sum(sapply(detections_l, nrow) > 1),
                sensitivity = nrow(detections) / nrow(W)
              ) else
              data.frame(
                count = 0,
                prop.time = 0,
                mean.duration.true.positives = 0,
                mean.duration.false.positives = mean((ad$end - ad$start)[ad$sound.files == z]),
                split.positives = 0,
                sensitivity = 0
                )
            
            return(result)
            })
                    
         performance <- do.call(rbind, performance_l)
        
         out <-
           data.frame(
             threshold = exp_grd$threshold[x],
             wl = exp_grd$wl[x],
             power = exp_grd$power[x],
             ssmooth = exp_grd$ssmooth[x],
             hold.time = exp_grd$hold.time[x],
             mindur = exp_grd$mindur[x],
             maxdur = exp_grd$maxdur[x],
             true.positives = sum(performance$count),
             false.positives = nrow(ad) - sum(performance$count),
             split.positives = sum(performance$split.positives) ,
             mean.duration.true.positives = mean(performance$mean.duration.true.positives),
             mean.duration.false.positives = mean(performance$mean.duration.false.positives),
             proportional.time.true.positives = mean(performance$prop.time),
             sensitivity = mean(performance$sensitivity)
             )
           
        
        out$false.positives[out$false.positives < 0] <- 0      
        out$mean.duration.false.positives[is.na(out$mean.duration.false.positives) | out$false.positives == 0] <- NA
        out$mean.duration.true.positives[is.na(out$mean.duration.true.positives) | out$true.positives == 0] <- NA
        
        # make sensitvities higher than 1 (because of split positives) 1
        out$sensitivity[out$sensitivity > 1] <- 1
        
        out$specificity <- out$true.positives / (out$true.positives + out$false.positives)
          } else
            
            out <-
            data.frame(
              threshold = exp_grd$threshold[x],
              wl = exp_grd$wl[x],
              power = exp_grd$power[x],
              ssmooth = exp_grd$ssmooth[x],
              hold.time = exp_grd$hold.time[x],
              mindur = exp_grd$mindur[x],
              maxdur = exp_grd$maxdur[x],
              true.positives = NA,
              false.positives = NA,
              split.positives = NA,
              mean.duration.true.positives = NA,
              mean.duration.false.positives = NA,
              proportional.time.true.positives = NA,
              sensitivity = NA,
              specificity = NA
            )
          
         return(out)
          })
      
        results <- do.call(rbind, grid_results)
        
        return(results)
        }
        
      # run it separate over each sound file
    if (by.sound.file) {
      by.rec <- lapply(unique(X$sound.files), function(w) {
        W <- grid_FUN(exp_grd, X[X$sound.files == w, ], Y)
        W$sound.files <- w
        return(W)
        })
      
      output <- do.call(rbind, by.rec)
      } else
        output <- grid_FUN(exp_grd, X, Y)
      
      return(output)
}
