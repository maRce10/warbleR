#' Summarize detection diagnostics
#' 
#' \code{summarize_diagnostic} summarizes detection diagnostics
#' @usage summarize_diagnostic(diagnostic, time.diagnostics = FALSE)
#' @param diagnostic A data frame with the reference selections (start and end of the signals) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end".
#' @return A data frame, typically the output of a detection optimization function (\code{\link{diagnose_detection}}, \code{\link{optimize_find_peaks}}, \code{\link{optimize_auto_detec}}) including the following detection performance diagnostics:
#' \itemize{
#'  \item \code{true.positives}: number of detections that correspond to signals referenced in 'reference'. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'. 
#'  \item \code{false.positives}: number of detections that don't match any of the signals referenced in 'reference'. In a perfect detection routine it should be 0.    
#'  \item \code{false.negatives}: number of signals in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0. 
#'  \item \code{split.positives}: number of signals referenced in 'reference' that were overlapped by more than 1 detection (i.e. detections that were split). In a perfect detection routine it should be 0.  
#'  \item \code{mean.duration.true.positives}: mean duration of true positives (in s). Optional.  
#'  \item \code{mean.duration.false.positives}: mean duration of false positives (in s). Optional.
#'  \item \code{mean.duration.false.negatives}: mean duration of false negatives (in s). Optional.
#'  \item \code{proportional.duration.true.positives}: ratio of total duration of true positives to the total duration of signals referenced in 'reference'. In a perfect detection routine it should be 1. Optional.
#'  \item \code{sensitivity}: Proportion of signals referenced in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  \item \code{specificity}: Proportion of detections that correspond to signals referenced in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  } 
#' @param time.diagnostics Logical argument to control if diagnostics related to the duration of the signals ("mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives" and "proportional.duration.true.positives") are returned (if \code{TRUE}). Default is \code{FALSE}.
#' @export
#' @name summarize_diagnostic
#' @details The function summarizes a detection diagnostic data frame in which diagnostic parameters are shown split by (typically) a categorical column, usually sound files. This function is used internally by \code{\link{diagnose_detection}}. 
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
#' summarize_diagnostic(reference = lbh_selec_reference, detection = ad1, 
#' by.sound.file = FALSE)
#' 
#' # by sound file
#' summarize_diagnostic(reference = lbh_selec_reference, detection = ad1, 
#' by.sound.file = TRUE)
#' 
#' # by sound file including time diagnostics
#' summarize_diagnostic(reference = lbh_selec_reference, detection = ad1, 
#' by.sound.file = TRUE, time.diagnostics = TRUE)
#' }
#' @seealso \code{\link{diagnose_detection}}, \code{\link{optimize_find_peaks}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
# last modification on aug-19-2021 (MAS)
summarize_diagnostic <- function(diagnostic, time.diagnostics = TRUE){
  
  # basic columns required in 'diagnostic'
  basic_colms <- c("true.positives", "false.positives", "false.negatives", "split.positives", "sensitivity", "specificity")
  
  #check diagnostic
  if (any(!(basic_colms %in% colnames(diagnostic))))
    stop(paste(paste(
      basic_colms[!(basic_colms %in% colnames(diagnostic))], collapse =
        ", "
    ), "column(s) not found in data frame"))
  
  # get extra column names (ideally should include tuning parameters)
  extra_colms <- setdiff(colnames(diagnostic), c(basic_colms, c("sound.files", "mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives", "proportional.duration.true.positives")))
  
  # create column combining all extra columns
  diagnostic$..combined.extra.colms <- if (length(extra_colms) > 0)
  apply(diagnostic[, extra_colms], 1, paste, collapse = "~>~") else "1"
  
  # get which extra columns were numeric
  if (length(extra_colms) > 0) numeric_colms <- sapply(diagnostic[, extra_colms], is.numeric)
  
  # switch to FALSE if no time columns
  if (is.null(diagnostic$mean.duration.true.positives)) time.diagnostics <- FALSE
  
  summ_diagnostic_l <- lapply(unique(diagnostic$..combined.extra.colms), function(x){
    # subset for each combination
    Y <- diagnostic[diagnostic$..combined.extra.colms == x, ]
   
    # summarize across sound files
    summ_diagnostic <- data.frame(
      true.positives = sum(Y$true.positives, na.rm = TRUE),
      false.positives = sum(Y$false.positives, na.rm = TRUE),
      false.negatives = sum(Y$false.negatives, na.rm = TRUE),
      split.positives = sum(Y$split.positives, na.rm = TRUE),
      ..combined.extra.colms = x,
      stringsAsFactors = FALSE
    ) 
    
    # add time diagnostics
    if (time.diagnostics){
      
      summ_diagnostic$mean.duration.true.positives <- mean(Y$mean.duration.true.positives, na.rm = TRUE)
      summ_diagnostic$mean.duration.false.positives <- mean(Y$mean.duration.false.positives, na.rm = TRUE)
      summ_diagnostic$mean.duration.false.negatives <- mean(Y$mean.duration.false.negatives, na.rm = TRUE)
      summ_diagnostic$proportional.duration.true.positives <- weighted.mean(x = Y$proportional.duration.true.positives, w = Y$true.positives, na.rm = TRUE)
      
    }  
    
    # add sensitivity and specificity at the end
    summ_diagnostic$sensitivity <- sum(Y$true.positives, na.rm = TRUE) / (sum(Y$true.positives, na.rm = TRUE) + sum(Y$false.negatives, na.rm = TRUE))
    summ_diagnostic$specificity <- 1 - (sum(Y$false.positives, na.rm = TRUE) / (sum(Y$true.positives, na.rm = TRUE) + sum(Y$false.positives, na.rm = TRUE)))
    
    # replace NaNs with NA
    for(i in 1:ncol(summ_diagnostic))
      if (is.nan(summ_diagnostic[, i])) summ_diagnostic[, i] <- NA
    
    return(summ_diagnostic) 
    
  })
  
  # put all in a single data frame
  summ_diagnostics_df <- do.call(rbind, summ_diagnostic_l)
  
  # add extra columns data
  if (length(unique(diagnostic$..combined.extra.colms)) > 1){
    extra_colms_df <- do.call(rbind, strsplit(summ_diagnostics_df$..combined.extra.colms, "~>~"))
    
    # add column names
    colnames(extra_colms_df) <- extra_colms
    
    # convert numeric columns
   if (any(numeric_colms)){
     extra_num_colms_df <- as.data.frame(apply(extra_colms_df[, numeric_colms], 2, as.numeric))
     
     # add non-numeric columns
     if (any(!numeric_colms)) {
        
       non_num_colms_df <- extra_colms_df[, !numeric_colms, drop = FALSE]
      colnames(non_num_colms_df) <- names(numeric_colms)[!numeric_colms]
        extra_colms_df <- cbind(non_num_colms_df, extra_num_colms_df)
       
       } else
         extra_colms_df <- extra_num_colms_df
     }
     
  # put all together 
    summ_diagnostics_df <- cbind(extra_colms_df, summ_diagnostics_df)
  }
  
  # remove column with all extra columns info
  summ_diagnostics_df$..combined.extra.colms <- NULL
  
  
 return(summ_diagnostics_df)
}