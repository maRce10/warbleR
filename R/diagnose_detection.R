#' Evaluate the performance of a signal detection procedure
#' 
#' \code{diagnose_detection} evaluates the performance of a signal detection procedure comparing the output selection table to a reference selection table 
#' @usage diagnose_detection(reference, detection, by.sound.file = FALSE, 
#' time.diagnostics = FALSE)
#' @param reference Data frame or 'selection.table' with the reference selections (start and end of the signals) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end".
#' @param detection Data frame or 'selection.table' with the detections (start and end of the signals) that will be compared against the 'reference' selections. Must contained at least the following columns: "sound.files", "selec", "start" and "end".
#' @param by.sound.file Logical argument to control whether performance diagnostics are summarized across sound files (when \code{by.sound.file = FALSE}, when more than 1 sound file is included in 'reference') or shown separated by sound file. Default is \code{FALSE}.
#' @param time.diagnostics Logical argument to control if diagnostics related to the duration of the signals ("mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives" and "proportional.duration.true.positives") are returned (if \code{TRUE}). Default is \code{FALSE}.
#' @return A data frame including the following detection performance diagnostics:
#' \itemize{
#'  \item \code{true.positives}: number of detections that correspond to signals referenced in 'reference'. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'. 
#'  \item \code{false.positives}: number of detections that don't match any of the signals referenced in 'reference'. In a perfect detection routine it should be 0.    
#'  \item \code{false.negatives}: number of signals in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0. 
#'  \item \code{split.positives}: number of signals referenced in 'reference' that were overlapped by more than 1 detection (i.e. detections that were split). In a perfect detection routine it should be 0.  
#'  \item \code{mean.duration.true.positives}: mean duration of true positives (in s). Only included when \code{time.diagnostics = TRUE}.  
#'  \item \code{mean.duration.false.positives}: mean duration of false positives (in s). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.negatives}: mean duration of false negatives (in s). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{proportional.duration.true.positives}: ratio of total duration of true positives to the total duration of signals referenced in 'reference'. In a perfect detection routine it should be 1. Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{sensitivity}: Proportion of signals referenced in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  \item \code{specificity}: Proportion of detections that correspond to signals referenced in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  } 
#' @export
#' @name diagnose_detection
#' @details The function evaluates the performance of a signal detection procedure by comparing its output selection table to a reference selection table in which all signals of interest have been selected. 
#' @examples
#' {
#' # load data
#' data(list = c("Phae.long1", "Phae.long2", "lbh_selec_reference"))
#' 
#' # save sound files
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav")) 
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' 
#' # run detection with auto_detec
#' ad1 <- auto_detec(path = tempdir(), flist = unique(lbh_selec_reference$sound.files),  
#' ssmooth = 1200, pb = FALSE, threshold =22, mindur = 0.10, maxdur = 0.18, 
#' bp = c(3, 9), power = 1)
#' 
#' # summarizing across sound files
#' diagnose_detection(reference = lbh_selec_reference, detection = ad1, 
#' by.sound.file = FALSE)
#' 
#' # by sound file
#' diagnose_detection(reference = lbh_selec_reference, detection = ad1, 
#' by.sound.file = TRUE)
#' 
#' # by sound file including time diagnostics
#' diagnose_detection(reference = lbh_selec_reference, detection = ad1, 
#' by.sound.file = TRUE, time.diagnostics = TRUE)
#' }
#' @seealso \code{\link{optimize_auto_detec}}, \code{\link{optimize_find_peaks}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
# last modification on jul-16-2021 (MAS)
diagnose_detection <- function(reference, detection, by.sound.file = FALSE, time.diagnostics = FALSE)
{
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(diagnose_detection)
  
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

  
  # remove rows with NAs in detection
  detection <- detection[!is.na(detection$start), ]
  
    # run when detection is not empty
    if (nrow(detection) > 0){
      # look at detections matching 1 training selection at the time
      performance_l <- lapply(unique(reference$sound.files), function(z){
        
        # get subset from template for that sound file
        W <- reference[reference$sound.files == z, ]
        
        # get subset from detection for that sound file
        Z <- detection[detection$sound.files == z, ]
        
        if (nrow(Z) > 0){
        # add row labels to both
        W$.row.id <- 1:nrow(W)
        Z$.row.id <- 1:nrow(Z)
        
        # these are all the true positives
        true.positives_l <- lapply(1:nrow(W), function(y){
          
          # defined as any detection that overlaps with the template selections
          Q <- Z[(Z$start >= W$start[y] & Z$start < W$end[y]) |
               (Z$end > W$start[y] & Z$end <= W$end[y]) |
               (Z$start <= W$start[y] & Z$end >= W$end[y]) |
               (Z$start >= W$start[y] & Z$end  <= W$end[y]), ]
        
          # add row label to find false.negatives
          Q$.template.row.id <- if (nrow(Q) > 0) W$.row.id[y] else
            vector()
          
          return(Q)
          })
        
        true.positives <- do.call(rbind, true.positives_l)
        
        # those not in true positives
        false.positives <- Z[!Z$.row.id %in% true.positives$.row.id, ]
        
        performance <- data.frame(
            sound.files = z,
            true.positives = length(unique(true.positives$.template.row.id)),
            false.positives = nrow(false.positives),
            false.negatives = nrow(W) - length(unique(true.positives$.template.row.id)),
            split.positives = sum(sapply(true.positives_l, nrow) > 1),
            mean.duration.true.positives = mean(true.positives$end - true.positives$start),
            mean.duration.false.positives = mean(false.positives$end - false.positives$start),
            mean.duration.false.negatives = mean(true.positives$end[!W$.row.id %in% true.positives$.template.row.id] - true.positives$start[!W$.row.id %in% true.positives$.template.row.id]),
            proportional.duration.true.positives = mean(true.positives$end - true.positives$start) / mean(W$end - W$start),
            sensitivity = length(unique(true.positives$.template.row.id)) / nrow(W),
            specificity =  length(unique(true.positives$.row.id)) / nrow(Z),
            stringsAsFactors = FALSE
          ) 
        
        # replace NaNs with NA
        for(i in 1:ncol(performance))
          if (is.nan(performance[, i])) performance[, i] <- NA
    
        # fix values when no false positives or true positives
        performance$false.positives[performance$false.positives < 0] <- 0
        performance$mean.duration.false.positives[is.na(performance$mean.duration.false.positives) | performance$false.positives == 0] <- NA
        performance$mean.duration.true.positives[is.na(performance$mean.duration.true.positives) | performance$true.positives == 0] <- NA
        
        # make sensitvities higher than 1 (because of split positives) 1
        performance$sensitivity[performance$sensitivity > 1] <- 1
        } else
          performance <- data.frame(
            sound.files = z,
            true.positives = 0,
            false.positives = 0,
            false.negatives = nrow(W),
            split.positives = NA,
            mean.duration.true.positives = NA,
            mean.duration.false.positives = NA,
            mean.duration.false.negatives = mean(W$end - W$start),
            proportional.duration.true.positives = NA,
            sensitivity = 0,
            specificity = 0,
            stringsAsFactors = FALSE
          ) 
        
        
        return(performance)
      })
      
      out_df <- do.call(rbind, performance_l)
      
    } else
      # output when there were no detections
  out_df <-
    data.frame(
      sound.files = unique(reference$sound.files),
      true.positives = 0,
      false.positives = 0,
      false.negatives = nrow(reference),
      split.positives = NA,
      mean.duration.true.positives = NA,
      mean.duration.false.positives = NA,
      mean.duration.false.negatives = NA,
      proportional.duration.true.positives = NA,
      sensitivity = 0,
      specificity = 0,
      stringsAsFactors = FALSE
  ) 
    
  # summarize across sound files
  if (!by.sound.file){
    
    # out_df <- data.frame(
    #   true.positives = sum(out_df$true.positives, na.rm = TRUE),
    #   false.positives = sum(out_df$false.positives, na.rm = TRUE),
    #   false.negatives = sum(out_df$false.negatives, na.rm = TRUE),
    #   split.positives = sum(out_df$split.positives, na.rm = TRUE),
    #   mean.duration.true.positives = mean(out_df$mean.duration.true.positives, na.rm = TRUE),
    #   mean.duration.false.positives = mean(out_df$mean.duration.false.positives, na.rm = TRUE),
    #   mean.duration.false.negatives = mean(out_df$mean.duration.false.negatives, na.rm = TRUE),
    #   proportional.duration.true.positives = weighted.mean(x = out_df$proportional.duration.true.positives, w = out_df$true.positives, na.rm = TRUE),
    #   sensitivity = sum(out_df$true.positives, na.rm = TRUE) / (sum(out_df$true.positives, na.rm = TRUE) + sum(out_df$false.negatives, na.rm = TRUE)),
    #   specificity = 1 - (sum(out_df$false.positives, na.rm = TRUE) / (sum(out_df$true.positives, na.rm = TRUE) + sum(out_df$false.positives, na.rm = TRUE))),
    #   stringsAsFactors = FALSE
    # ) 
    # 
    # # replace NaNs with NA
    # for(i in 1:ncol(out_df))
    #   if (is.nan(out_df[, i])) out_df[, i] <- NA
    
    out_df <- summarize_diagnostic(diagnostic = out_df, time.diagnostics = time.diagnostics)
  }
  
  # remove time diagnostics
  if (!time.diagnostics)
    out_df <- out_df[ , grep(".duration.", names(out_df), invert = TRUE)]
  
    return(out_df)
}
