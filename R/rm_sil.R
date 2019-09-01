#' Remove silence in wave files
#'
#' \code{rm_sil} Removes silences in wave files
#' @usage rm_sil(path = NULL, min.sil.dur = 2, img = TRUE, it = "jpeg", flim = c(0, 12), 
#' flist = NULL, parallel = 1, pb = TRUE)
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param min.sil.dur Numeric. Controls the minimum duration of silence segments that would be removed.
#' @param img Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.
#' @param it A character vector of length 1  giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param flim A numeric vector of length 2 indicating the highest and lowest 
#'   frequency limits (kHz) of the spectrogram as in 
#'   \code{\link[seewave]{spectro}}. Default is c(0,12). Ignored if `img = FALSE`.
#' @param flist character vector or factor indicating the subset of files that will be analyzed. If not provided
#' then all wave files in the working directory (or path) will be processed.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}. 
#' @return Sound files for which silence segments have been removed are saved in the new 
#' folder "silence-removed_files". If `img = TRUE` then spectrogram images highlighting the silence segments 
#' that were removed are also saved. 
#' @export
#' @name rm_sil
#' @details The function removes silence segments (i.e. segments with very low amplitude values) from wave files. 
#' @seealso \code{\link{fixwavs}}, \code{\link{autodetec}} 
#' @examples{
#' # save sound file examples
#' data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
#' sil <- silence(samp.rate = 22500, duration = 3, xunit = "time")
#' 
#' wv1 <- pastew(pastew(Phae.long1, sil, f = 22500, output = "Wave"), 
#' Phae.long2, f = 22500, output = "Wave")
#' 
#' #check silence in between amplitude peaks
#' env(wv1)
#'
#'  #save wave file
#'  writeWave(object = wv1, filename = file.path(tempdir(), "wv1.wav"),
#'   extensible = FALSE)
#' 
#' #remove silence
#' rm_sil(flist = "wv1.wav", pb = FALSE, path = tempdir())
#' 
#' #check this floder
#' tempdir()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on mar-13-2018 (MAS)

rm_sil <- function(path = NULL, min.sil.dur = 2, img = TRUE, it = "jpeg", flim = c(0, 12), 
                   flist = NULL, parallel = 1, pb = TRUE)
{

  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(rm_sil)
  
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
    if (!dir.exists(path)) stop("'path' provided does not exist")  
  
  #read files
  files <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)  
  
  #stop if files are not in working directory
  if (length(files) == 0) stop("no .wav files in working directory")
  
  #subet based on file list provided (flist)
  if (!is.null(flist)) files <- files[files %in% flist]
  if (length(files) == 0)  stop("selected .wav files are not in working directory")

  #if it argument is not "jpeg" or "tiff" 
  if (!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
  #if parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  files <- files[!is.na(files)]
  
  #stop if files are not in working directory
  if (length(files) == 0) stop("all .wav files have been processed")
  
  dir.create(file.path(path, "silence-removed_files"), showWarnings = FALSE)
  
  rm.sil.FUN <- function(fl, f = 5000, msd = min.sil.dur, flm = flim, mg = img) {
    
    # read wave
    wv <- warbleR::read_wave(X = fl, path = path)  
    
    #in case flim is higher than can be due to sampling rate
    if (flm[2] > ceiling(wv@samp.rate/2000) - 1) flm[2] <- ceiling(wv@samp.rate/2000) - 1 
    
    #downsample to speed up process
    if (wv@samp.rate > f + 1000) wv1 <- downsample(object = wv, samp.rate =  f) else wv1 <- wv
    writeWave(wv1, file.path(tempdir(),  fl))
  
    ad <- autodetec(threshold = 0.06, ssmooth = 1500, parallel = 1, pb = FALSE, img = FALSE, flist = fl, path = tempdir())
    
    # remove the silence less than min.sil.dur 
    if (nrow(ad) > 1) for(i in 1:(nrow(ad) - 1)) {
      if (i == nrow(ad)) break
      if (ad$start[i + 1] - ad$end[i] < msd) { 
        ad$start[i] <- ad$start[i + 1]
        ad <- ad[-(i + 1), ]
        }
    }
    
    
    if (mg)
    {
      img_wrlbr_int(filename = paste0(fl, ".rm.silence.", it), path = file.path(path, "silence-removed_files"),  res = 160, units = "in", width = 8.5, height = 4) 
    
      par(mar = c(4, 4, 1, 1))
      spectro_wrblr_int(wv, ovlp = 0, grid = FALSE, scale = FALSE, palette = monitoR::gray.3, axisX = TRUE, fast.spec = TRUE, flim = flm)
    
      # label silence in spectro
      if (nrow(ad) > 1) lapply(1:(nrow(ad) - 1), function(z)
        {
        # add arrow
        arrows(x0 = ad$end[z], y0 = flm[1] + flm[2]/2, x1 = ad$start[z + 1], y1 = flm[1] + flm[2]/2, code = 3,
               col = adjustcolor("red2", alpha.f = 0.5), lty = "solid", lwd = 4, angle = 30)

        # add silence label
        text(x = mean(c(ad$end[z], ad$start[z + 1])), y = flm[1] + flm[2]/1.8, labels = "Silence", col = adjustcolor("red2", alpha.f = 0.5), pos = 3)

      }) else  {text(x = (length(wv@left)/wv@samp.rate)/2, y = flm[1] + flm[2]/1.2, labels = "No silence(s) detected", col = adjustcolor("red2", alpha.f = 0.8), pos = 3)
        }

            dev.off()    
    }
    
    #cut silence from file
    if (nrow(ad) > 1) {for(z in (nrow(ad) - 1):1)   wv <- deletew(wave = wv, from = ad$end[z], to = ad$start[z + 1], plot = FALSE, output = "Wave")
    writeWave(object = wv, filename = file.path(path, "silence-removed_files", fl), extensible = FALSE)
    } else  file.copy(from = wv, to = file.path(path, "silence-removed_files", fl))
    }
  
  if (pb) cat("searching for silence segments in wave files:")
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  # run loop apply function
  out <- pbapply::pblapply(X = files, cl = cl, FUN = function(i) 
  { 
    rm.sil.FUN(fl = i, f = 5000, msd = min.sil.dur, flm = flim, mg = img) 
  }) 

}
  
