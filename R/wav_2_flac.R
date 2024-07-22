#' Convert .wav files to .flac
#'
#' \code{wav_2_flac} converts several .wav files to .flac compressed lossless format
#' @param files character vector with the names of files to be converted. If \code{NULL} all files in the working directory (or 'path' if supplied) are converted.
#' @param path Character string containing the directory path where the .wav files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param overwrite Logical. Control whether a .flac sound file that is already in the working directory should be
#' overwritten.
#' @param pb Logical argument to control if progress bar is shown. Default is \code{TRUE}. It can also be
#' set globally using the 'pb' option (see \code{\link{warbleR_options}}).
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing). It can also be
#' set globally using the 'parallel' option (see \code{\link{warbleR_options}}).
#' @param reverse Logical argument to control if .wav files are converted into .flac files (default, \code{reverse = FALSE}) or .flac files are converted into .wav files \code{reverse = TRUE}.
#' @param compression Numeric string on length 1 indicating the level of compression for .flac files. Must a number between 0 (lowest) to 8 (highest compression). Default is 5.
#' @param flac.path Path to the flac program, mostly needed for windows OS.
#' @return .flac files saved in the working directory with same name as original wav files.
#' @export
#' @details The function will convert all .wav files in working directory or 'path' supplied to .flac format (or the opposite if \code{reverse = TRUE}). For reading 'flac' files on windows the path to the .exe is required. This can be set using the 'flac.path' argument (or globally using the same argument in \code{\link{warbleR_options}}). Note that reading 'flac' files requires creating a temporary copy in 'wav' format, which can be particularly slow for long files.
#' @name wav_2_flac
#' @examples
#' \dontrun{
#' # create some .wav files
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' # Convert all files to .flac format
#' wav_2_flac(path = tempdir())
#'
#' # check this folder!!
#' open_wd(tempdir())
#' }
#'
#' @details convert all .wav files in working directory to .flac compressed lossless format. It's just a silly wrapper over (\code{\link[seewave]{wav2flac}}) to simplify converting several files at once. The function works recursively, converting files within all subfolders.
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on abr-13-2021 (MAS)

wav_2_flac <-
  function(files = NULL,
           path = NULL,
           overwrite = FALSE,
           pb = TRUE,
           parallel = 1,
           reverse = FALSE,
           compression = 5,
           flac.path = "") {
    #### set arguments from options
    # get function arguments
    argms <- methods::formalArgs(wav_2_flac)

    # get warbleR options
    opt.argms <-
      if (!is.null(getOption("warbleR"))) {
        getOption("warbleR")
      } else {
        SILLYNAME <- 0
      }

    # remove options not as default in call and not in function arguments
    opt.argms <-
      opt.argms[!sapply(opt.argms, is.null) &
        names(opt.argms) %in% argms]

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

    # set path to flac
    if (is.null(getOption("warbleR")$flac.path)) {
      # on linux
      if (.Platform$OS.type == "unix") {
        if (missing(flac.path)) {
          run_flac <- "flac"
        } else {
          run_flac <- paste(flac.path, "flac", sep = "/")
        }
        if (system(paste(run_flac, "-v"), ignore.stderr = TRUE) !=
          0) {
          stop2("FLAC program was not found")
        }
      }

      # on windows
      if (.Platform$OS.type == "windows") {
        if (missing(flac.path)) {
          "flac" <- "flac.exe"
        }
        if (missing(flac.path)) {
          run_flac <- paste("C:/Program Files/FLAC/", "flac",
            sep = ""
          )
          if (!file.exists(run_flac)) {
            run_flac <- paste("C:/Program Files (x86)/FLAC/",
              "flac",
              sep = ""
            )
          }
        } else {
          run_flac <- paste(flac.path, "flac", sep = "/")
        }
        if (!file.exists(run_flac)) {
          stop2("FLAC program was not found")
        }
      }

      warbleR_options(flac.path = if (missing("flac.path")) {
        ""
      } else {
        flac.path
      })
    } else {
      run_flac <-
        if (getOption("warbleR")$flac.path == "") {
          "flac"
        } else {
          file.path(getOption("warbleR")$flac.path, "flac")
        }
    }

    # get files in path supplied
    files_in_path <-
      list.files(
        path = path,
        pattern = if (reverse) {
          ".flac$"
        } else {
          ".wav$"
        },
        ignore.case = TRUE
      )

    if (is.null(files)) {
      files <- files_in_path
    } else {
      if (!all(files %in% files_in_path)) {
        stop2("some (or all) sound files were not found")
      }
    }

    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & parallel > 1) {
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
    } else {
      cl <- parallel
    }


    # run loop apply function
    out_l <-
      .pblapply(
        pbar = pb,
        X = files,
        cl = cl,
        message = "converting sound files",
        total = 1,
        FUN = function(i) {
          # .try_na(
          flacwav(
            file = file.path(path, i),
            overwrite = overwrite,
            reverse = reverse,
            compression = compression,
            run_flac = run_flac
            # )
          )
        }
      )
  }

flacwav <-
  function(file,
           reverse = FALSE,
           overwrite = FALSE,
           exename = NULL,
           path2exe = NULL,
           compression = 5,
           run_flac) {
    # set compression
    compression <- paste0("--compression-level-", compression)

    # on linox and macOS
    if (.Platform$OS.type == "unix") {
      if (reverse) {
        wav_file <- gsub("flac$", "wav", file, ignore.case = TRUE)

        e <-
          system(
            paste(
              run_flac,
              "-d",
              file,
              "-o",
              wav_file,
              "--totally-silent",
              if (overwrite) {
                "--force"
              } else {
                ""
              }
            ),
            ignore.stderr = TRUE
          )
      } else {
        e <-
          system(
            paste(
              run_flac,
              file,
              "--totally-silent",
              if (overwrite) {
                "--force"
              } else {
                ""
              },
              compression
            ),
            ignore.stderr = TRUE,
            intern = FALSE
          )
      }
    }

    if (.Platform$OS.type == "windows") {
      if (missing(exename)) {
        exename <- "flac.exe"
      }
      if (missing(path2exe)) {
        run_flac <- paste("C:/Program Files/FLAC/", exename,
          sep = ""
        )
        if (!file.exists(run_flac)) {
          run_flac <- paste("C:/Program Files (x86)/FLAC/",
            exename,
            sep = ""
          )
        }
      } else {
        run_flac <- paste(path2exe, exename, sep = "/")
      }
      if (!file.exists(run_flac)) {
        stop2("FLAC program was not found.")
      }
      if (reverse) {
        e <- system(
          paste(
            shQuote(run_flac),
            "-d",
            shQuote(file,
              type = "cmd"
            ),
            "-o",
            shQuote(wav_file,
              type = "cmd"
            ),
            "--totally-silent",
            if (overwrite) {
              "--force"
            } else {
              ""
            },
            sep = " "
          ),
          ignore.stderr = TRUE
        )
      } else {
        e <-
          system(
            paste(
              shQuote(run_flac),
              shQuote(file, type = "cmd"),
              "--totally-silent",
              if (overwrite) {
                "--force"
              } else {
                ""
              },
              compression,
              sep = " "
            ),
            ignore.stderr = TRUE
          )
      }
    }
  }
