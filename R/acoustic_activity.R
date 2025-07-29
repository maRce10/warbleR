#' Estimate acoustic activity across sound files based on selections
#'
#' \code{acoustic_activity} estimates acoustic activity across sound files based on selections.
#' @param X 'selection_table' object or data frame with the following columns: 1) "sound.files": name of the .wav
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end":
#' end time of selections.
#' @param time.window Numeric. The time window in seconds to calculate the acoustic activity rate. Default is 60 seconds.
#' @param hop.size Numeric. The hop size in seconds to calculate the acoustic activity rate. It refers to the spacing between consecutive time windows. If \code{hop.size == time.window} then there is no overlap between time windows. Default is 1 second.
#' @param path Character string containing the directory path where the sound files are located.
#' By default the current working directory is used.
#' @param files Character vector with the names of the sound files to be used in the analysis. Default is \code{unique(X$sound.files)}. Use \code{list.files(tempdir(), pattern = ".wav$")} (or modify according to file extension) for using all sound files in the 'path' supplied (even those with no selections in 'X').
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return A data frame including the columns in the following columns:
#' \itemize{
#'    \item \code{sound.files}: files for which acoustic activity was measured   
#'    \item \code{start}: start of the time window where selections were counted (in seconds)
#'    \item \code{end}: end of the time window where selections were counted (in seconds)
#'    \item \code{counts}: number of selections in the time window (counted if the middle point of the selection is within the time window). Note that the last time window may not have the same length as the others if the sound file duration is not multiple of the time window.
#'    \item \code{rate}: number of selections per second.
#'    }
#' @details This function estimates the acoustic activity (a.k.a. vocal rate) across sound files based on selections. It counts the number of selections in a given time window (default is 60 seconds) and calculates the rate of acoustic activity per second. A sound is counted as present in a time window if its middle point (\code{(X$end + X$start) / 2}) is within that window. Acoustic activity rates (e.g., calls per minute) are a widely used metric in neuroscience research, providing quantitative insight into rodent ultrasonic vocalizations as indicators of affective states, social interactions, and motivational processes (e.g. Rojas-Carvajal et al. 2023, Wardak et al. 2024). 
#' 
#' \code{\link{song_analysis}}.
#' @seealso \code{\link{inflections}}
#' @export
#' @name acoustic_activity
#' @export
#' @examples{
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' # get vocal activity by second
#' va <- acoustic_activity(X = lbh_selec_table, path = tempdir(), time.window = 1, 
#'                    hop.size = 1)
#'                    
#' # get the row with the highest rate per sound file
#' do.call(rbind, lapply(split(va, va$sound.files), function(x) 
#' x[which.max(x$rate), ]))
#' 
#' #including a file with no annotations
#' writeWave(Phae.long1, file.path(tempdir(), "no_anns.wav"))
#' 
#' va <- acoustic_activity(X = lbh_selec_table, path = tempdir(), time.window = 1, 
#' hop.size = 1, files = list.files(tempdir(), pattern = ".wav$"))
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#' @references 
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' Rojas-Carvajal, M., Leandro, R., & Brenes, J. C. (2023). Distinct acute stressors exert an antagonistic effect on complex grooming during novelty habituation in rats. Behavioural Processes, 212, 104931.
#' 
#' Rojas-Carvajal, M., Sequeira-Cordero, A., & Brenes, J. C. (2020). Neurobehavioral effects of restricted and unpredictable environmental enrichment in rats. Frontiers in pharmacology, 11, 674.
#' 
#' Wardak, A. D., Olszyński, K. H., Polowy, R., Matysiak, J., & Filipkowski, R. K. (2024). Rats that learn to vocalize for food reward emit longer and louder appetitive calls and fewer short aversive calls. Plos one, 19(2), e0297174.

acoustic_activity <- function(X,
                           time.window = 60,
                           hop.size = 1,
                           path = ".",
                           files = unique(X$sound.files),
                           parallel = 1,
                           pb = TRUE) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(acoustic_activity)

  # get warbleR options
  opt.argms <- if (!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0

  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]

  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]

  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]

  # set options left
  if (length(opt.argms) > 0) {
    for (q in seq_len(length(opt.argms))) {
      assign(names(opt.argms)[q], opt.argms[[q]])
    }
  }

  # check path to working directory
  if (is.null(path)) {
    path <- getwd()
  } else if (!dir.exists(path)) {
    stop2("'path' provided does not exist")
  } else {
    path <- normalizePath(path)
  }

  # if X is not a data frame
  if (all(!any(is.data.frame(X), is_selection_table(X)))) stop2("X is not of a class 'data.frame' or 'selection_table'")

  if (is_extended_selection_table(X)) stop2("This function does not work on objects of class 'extended_selection_table'")

  if (!all(c(
    "sound.files", "selec",
    "start", "end"
  ) %in% colnames(X))) {
    stop2(paste(paste(c("sound.files", "selec", "start", "end")[!(c(
      "sound.files", "selec",
      "start", "end"
    ) %in% colnames(X))], collapse = ", "), "column(s) not found in data frame"))
  }

  # if end or start are not numeric stop
  if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop2("'start' and 'end' must be numeric")

  # if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop2("NAs found in start and/or end")

  # check for duplicates and if fix.selec = TRUE
  if (any(duplicated(paste(X$sound.files, X$selec)))) {
      stop2("Duplicated selection labels ('selec' column) for one or more sound files (can be fixed by setting fix.selec = TRUE)")
  }

  # check additional columns
  if (!"channel" %in% colnames(X)) {
    X$channel <- 1
  } else {
    if (!is.numeric(X$channel)) stop2("'channel' must be numeric")
    if (any(is.na(X$channel))) {
      message2("NAs in 'channel', assumed to be channel 1 \n")
      X$channel[is.na(X$channel)] <- 1
    }
  }

  # check if files are in working directory
  if (all(!file.exists(file.path(path, unique(X$sound.files))))) {
    stop2("no sound files found")
  }

  # update to new frequency range column names
  if (any(grepl("low.freq|high.freq", names(X)))) {
    names(X)[names(X) == "low.freq"] <- "bottom.freq"
    names(X)[names(X) == "high.freq"] <- "top.freq"
    message2("'low.freq' and 'high.freq' renamed as 'bottom.freq' and 'top.freq' \n")
  }

  # check if freq lim are numeric
  if (any(names(X) == "bottom.freq")) {
    if (!is(X$bottom.freq, "numeric")) stop2("'bottom.freq' is not numeric")
  }

  if (any(names(X) == "top.freq")) {
    if (!is(X$top.freq, "numeric")) stop2("'top.freq' is not numeric")
  }

  # check if NAs in freq limits
  if (any(names(X) %in% c("bottom.freq", "top.freq"))) {
    if (any(is.na(c(X$bottom.freq, X$top.freq)))) stop2("NAs found in 'top.freq' and/or 'bottom.freq' \n")
  }
  
  # get file durations  
  file_durations <- warbleR::duration_sound_files(files = files, path = path) 
    
    # counted if midpoint is within the segment
    rate_df_list <- lapply(files, function(x){
      
      X <- X[X$sound.files == x, ]
      X$mid.point <- (X$end + X$start) / 2
      
      # get duration for each sound file
      duration <- file_durations$duration[file_durations$sound.files == x]
      
      # get position in time where to sample
      sampling_times <- seq(0, duration, by = hop.size)
      
      # get the number of calls in a minute with a sliding window of 1 second across each sound file
      counts <- sapply(sampling_times, function(t) {
        # get the calls in the time window
        sum(X$mid.point >= t & X$mid.point < (t + time.window))
      })
      
      # add duration as last value if not the same
      if (sampling_times[length(sampling_times)] != duration) {
        sampling_times <- c(sampling_times, duration)
      } else {
        # remove last one if last value is the same as duration
        counts <- counts[-length(counts)]
      }
      
      # create a data frame with the time and the number of calls
      rate_df <- data.frame(
        sound.files = x,
        start = sampling_times[-length(sampling_times)],
        end =  ifelse(sampling_times[-length(sampling_times)] + time.window > duration, duration
                      , sampling_times[-length(sampling_times)] + time.window),
        counts = counts
      )
      
      return(rate_df)    
    })
    results_df <- do.call(rbind, rate_df_list)
    
    results_df$duration <- results_df$end - results_df$start
    results_df$rate <- results_df$counts / results_df$duration
    
    return(results_df)
}
