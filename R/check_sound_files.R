#' Check sound files
#'
#' \code{check_sound_files} checks whether sound files can be read by subsequent functions.
#' @param X Optional. 'selection_table' object or data frame with the following columns: 1) "sound.files": name of the sound
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end":
#' end time of selections. If provided the function also returns the
#' smallest number of samples from the listed selections, which limits the minimum window
#' length (wl argument in other functions) that can be used in batch analyses.
#' This could be useful for avoiding errors in downstream functions (e.g. \code{\link{spectro_analysis}}).
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param check.header Logical. Checks whether number of samples in the file header matches that in the actual file (i.e. if the header is corrupted). This could significantly affect the performance of the function (much slower) particularly
#' with long sound files.
#' @param verbose Logical to control whether the summary messages are printed to the console. Defaut is \code{TRUE}.
#' @return If all sound files are ok, returns message "All files can be read".
#'   Otherwise returns the names of the corrupted sound files.
#' @details This function checks if sound files in the working directory can be read.
#' Users must set the working directory where they wish to check sound files beforehand.
#' If X is provided it also returns the smallest number of samples from
#' the selections listed in X (if all files can be read). Note that corrupt files can be
#' fixed using \code{\link{fix_wavs}}) ('sox' must be installed to be able to run this function).
#' The function is intended for a "quick and dirty" check of the sound files in a selections data
#'  frame. For a more thorough analysis see \code{\link{check_sels}}.
#' @export
#' @seealso \code{\link{check_sels}} \code{\link{tailor_sels}}
#' @name check_sound_files
#' @examples{
#' # save wav file examples
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' # without selection data frame
#' check_sound_files(path = tempdir())
#'
#' # with selection data frame
#' check_sound_files(X = lbh_selec_table, path = tempdir())
#' }
#' @references Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on jul-5-2016 (MAS)

check_sound_files <- function(X = NULL,
                              parallel = 1,
                              path = NULL,
                              check.header = FALSE,
                              verbose = TRUE)
{
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(check_sound_files)

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

  # return warning if not all sound files were found
  files <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)
  if (length(files) == 0) stop2("no sound files in working directory")


  if (!is.null(X)) {
    # if X is not a data frame
    if (!any(is.data.frame(X), is_selection_table(X))) stop2("X is not of a class 'data.frame' or 'selection_table'")

    if (!all(c(
      "sound.files", "selec",
      "start", "end"
    ) %in% colnames(X))) {
      stop2(paste(paste(c("sound.files", "selec", "start", "end")[!(c(
        "sound.files", "selec",
        "start", "end"
      ) %in% colnames(X))], collapse = ", "), "column(s) not found in data frame"))
    }

    # if there are NAs in start or end stop
    if (any(is.na(c(X$end, X$start)))) stop2("NAs found in start and/or end")

    # if end or start are not numeric stop
    if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop2("'start' and 'end' must be numeric")

    # if any start higher than end stop
    if (any(X$end - X$start <= 0)) stop2(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))

    if (length(unique(X$sound.files[(X$sound.files %in% files)])) != length(unique(X$sound.files))) {
      message2(paste(
        length(unique(X$sound.files)) - length(unique(X$sound.files[(X$sound.files %in% files)])),
        "sound file(s) not found"
      ))
    }

    # count number of sound files in working directory and if 0 stop
    d <- which(X$sound.files %in% files)
    if (length(d) == 0) {
      stop2("The sound files are not in the working directory")
    } else {
      X <- X[d, , drop = FALSE]
    }

    files <- files[files %in% X$sound.files]
  }

  out_df <- data.frame(sound.files = files, row.names = NULL)
  
  out_df$samp.rate <- sapply(out_df$sound.files, function(x) {
    # print(x)
    r <- try(suppressWarnings(warbleR::read_sound_file(X = x, path = path, header = TRUE)), silent = TRUE)
    if (is(r, "try-error")) {
      return(NA)
    } else {
      return(r$sample.rate)
    }
  })

  out_df$result <- ifelse(is.na(out_df$samp.rate), "cannot be read", "can be read")

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }
  
  if (check.header){
  out_df$samples.in.file <- unlist(.pblapply(seq_len(nrow(out_df)), cl = cl, message = "checking samples in files", current = 0, total = 2, verbose = verbose, function(x) {
    # print(x)
    if (out_df$result[x] == "can be read") {
    r <- suppressWarnings(warbleR::read_sound_file(X = out_df$sound.files[x], path = path, header = FALSE))
    out <- length(r@left)
    } else {
      out <- NA
    }
    return(out)
  }))
  
  out_df$samples.in.header <- unlist(.pblapply(seq_len(nrow(out_df)), cl = cl, message = "checking samples in headers", current = 2, total = 2, verbose = verbose, function(x) {
    # print(x)
    if (out_df$result[x] == "can be read") {
      r <- suppressWarnings(warbleR::read_sound_file(X = out_df$sound.files[x], path = path, header = TRUE))
      out <- r$samples
    } else {
      out <- NA
    }
    return(out)
  }))
  out_df$result <- ifelse(out_df$samples.in.file != out_df$samples.in.header, "can be read/header corrupted", out_df$result)
    }
  
  

  if (any(out_df$result == "cannot be read")) {
    message2("Some file(s) cannot be read")
    # return(files[is.na(samp.rate)])
  } else {
    if (verbose) 
      message2("All files can be read\n")
    if (!is.null(X) & verbose) {
      df <- merge(X, out_df, by = "sound.files")

      message2(paste("smallest number of samples: ", floor(min((df$end - df$start) * df$samp.rate)), " (sound file:", as.character(df$sound.files[which.min((df$end - df$start) * df$samp.rate)]), "; selection label: ", df$selec[which.min((df$end - df$start) * df$samp.rate)], ")\n", sep = ""))
    }
  }
  if (length(unique(out_df$samp.rate)) > 1 & verbose) {
    message2("Not all sound files have the same sampling rate (potentially problematic, particularly for cross_correlation())")
  }
  
  invisible(out_df)
}
