#' Simulate animal vocalizations
#' 
#' \code{sim_songs} simulate animal vocalizations in a wave object under brownian motion frequency drift.
#' @usage sim_songs(n = 1, durs = 0.2, harms = 3, amps = c(1, 0.5, 0.2), gaps = 0.1, freqs = 5, 
#' samp.rate = 44.1, sig2 = 0.5, steps = 10, bgn = 0.5, seed = NULL, diff_fun = "GBM", 
#' fin = 0.1, fout = 0.2, shape = "linear", selec_table = FALSE, file_name = NULL,
#' path = NULL) 
#' @param n Number of song subunits (e.g. elements). Default is 1.
#' @param durs Numeric vector with the duration of subunits in seconds. It should either be a single value (which would 
#' be used for all subunits) or a vector of length \code{n}. 
#' @param harms NUmeric vector of length 1 specifying the number of harmonics to simulate. 1 indicates that only the fundamental
#' frequency harmonic will be simulated.
#' @param amps Numeric vector with the relative amplitude of each of the harmonics (including the fundamental frequency).
#' @param gaps Nueric vector with the duration of gaps (silence between subunits) in seconds. It should either be a single value
#'  (which would  be used for all subunits) or a vector  of length \code{n + 1}. 
#' @param freqs Numeric vector with the initial frequency of the subunits (and ending frequency if \code{diff_fun == "BB"}) in kHz. 
#' It should either be a single value (which would  be used for all subunits) or a vector of length \code{n}. 
#' @param samp.rate Numeric vector of length 1. Sets the sampling frequency of the wave object (in kHz). Default is 44.1.
#' @param sig2 Numeric vector of length 1 defining the sigma value of the brownian motion model. Higher values will produce faster 
#' frequency modulations. Ignored if \code{diff_fun == "BB"}. Default is 0.1. Check the \code{\link[Sim.DiffProc]{BB}} 
#' for more details. 
#' @param steps Numeric vector of length 1. Controls the mean number of segments in which each song subunit is split during 
#' the brownian motion process. If not all subunits have the same duration, longer units will be split in more steps (although 
#' the average duration subunit will have the predefined number of steps). Default is 10.
#' @param bgn Numeric vector of length 1 indicating the background noise level. 0 means no additional noise will 1 means 
#' noise at the same amplitude than the song subunits. Default is 0.5.
#' @param seed Numeric vector of length 1. This allows users to get the same results in different runs (using  \code{\link[base]{set.seed}} internally). Default is \code{NULL}.
#' @param diff_fun Character vector of length 1 controlling the function used to simulate the brownian motion process of 
#' frequency drift across time. Only "BB", "GBM" and "pure.tone" are accepted at this time.Check the \code{\link[Sim.DiffProc]{BB}} 
#' for more details.
#' @param fin Numeric vector of length 1 setting the proportion of the sub-unit to fade-in amplitude (value between 0 and 1). 
#' Default is 0.1. Note that 'fin' + 'fout' cannot be higher than 1.   
#' @param fout Numeric vector of length 1 setting the proportion of the sub-unit to fade-out amplitude (value between 0 and 1).
#' Default is 0.2. Note that 'fin' + 'fout' cannot be higher than 1.  
#' @param shape Character string of length 1 controlling the shape of in and out amplitude fading of the song sub-units
#' ('fin' and 'fout'). "linear" (default), "exp" (exponential), and "cos" (cosine) are currently allowed.
#' @param selec_table Logical. If \code{TRUE} a data frame containing the start/end time, and bottom/top frequency of the sub-units is also returned and the wave object
#' is saved as a ".wav" file in the working directory. Default is \code{FALSE}.
#' @param file_name Character string for naming the ".wav" file. Ignored if 
#' 'selec_table' is \code{FALSE}. If not provided the date-time stamp will be used.
#' @param path Character string containing the directory path where the sound files are located. Ignored if 'selec_table' is \code{FALSE}. 
#' If \code{NULL} (default) then the current working directory is used.
#' @return A wave object containing the simulated songs. If 'selec_table' is \code{TRUE} the function saves the wave object as a '.wav' sound file in the working directory (or 'path') and returns a list including 1) a selection table with the start/end time, and bottom/top frequency of the sub-units and 2) the wave object. 
#' @seealso \code{\link{querxc}} for for downloading bird vocalizations from an online repository.
#' @export
#' @name sim_songs
#' @details This functions uses a geometric (\code{diff_fun == "GBM"}) or Brownian bridge (\code{diff_fun == "BB"}) motion stochastic process to simulate modulation in animal vocalizations (i.e. frequency traces across time). 
#' The function can also simulate pure tones (\code{diff_fun == "pure.tone"}, 'sig2' is ignored). 
#' Several song subunits (e.g. elements) can be simulated as well as the corresponding harmonics.
#' @examples
#' {
#'  # simulate a song with 3 elements and no harmonics
#'  sm_sng <- sim_songs(n = 3, harms = 1)
#'  
#'  # plot spectro
#'  seewave::spectro(sm_sng)
#'  
#'  # simulate a song with 5 elements and 2 extra harmonics
#' sm_sng2 <- sim_songs(n = 5, harms = 3)
#' 
#'  # plot spectro
#'  seewave::spectro(sm_sng2)
#'
#' # six pure tones with frequency ranging form 4 to 6 and returning selection table
#' sm_sng <- sim_songs(n = 6, harms = 1, seed = 1, diff_fun = "pure.tone", 
#'                   freqs = seq(4, 6, length.out = 6), selec_table = TRUE)
#'
#' plot spectro
#' seewave::spectro(sm_sng$wave, flim = c(1, 10))
#' 
#' # selection table
#' sm_sng$selec_table
#'}
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on feb-22-2018 (MAS)

sim_songs <- function(n = 1, durs = 0.2, harms = 3, amps = c(1, 0.5, 0.2), gaps = 0.1, freqs = 5, samp.rate = 44.1, 
                      sig2 = 0.5, steps = 10, bgn = 0.5, seed = NULL, diff_fun = "GBM", fin = 0.1,
                      fout = 0.2, shape = "linear", selec_table = FALSE, 
                      file_name = NULL, path = NULL) {
  
  # reset working directory 
  if (selec_table)
    {
  on.exit(options(warn = .Options$warn))
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(sim_songs)
  
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
  if (is.null(path))  path <- getwd() else 
    if (!dir.exists(path)) 
      stop("'path' provided does not exist") 
  
  }
  
  if (length(durs) != n & length(durs) != 1) stop("length of 'durs' should be 1 or equal to 'n'")
  if (length(amps) != harms  & harms > 1) stop("length of 'amps' should be equal to 'harms'")
  if (length(gaps) != n + 1 & length(gaps) != 1) stop("length of 'gaps' should be 1 or equal to 'n' + 1")
  if (length(durs) == 1 & n != 1) durs <- rep(durs, n)
  if (length(gaps) == 1) gaps <- rep(gaps, n + 1)
  if (length(freqs) == 1 & n != 1) freqs <- rep(freqs, n)
  if (harms < 1) stop("'harms' should at least 1")
  if (harms > 10) harms <- 10
  if (harms > 1) amps <- amps / max(amps)
  
  # set diffusion function 
  if (diff_fun == "GBM") df_fn <- Sim.DiffProc::GBM 
  if (diff_fun == "BB") df_fn <- Sim.DiffProc::BB
    
  if (!is.null(seed))
    seeds <- 1:(3 * n) + seed  
  
  # harmonics frequencies relative to fundamental
  hrm_freqs <- c(1/2, 1/3, 2/3, 1/4, 3/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10)
  hrm_freqs <- sort(1 / hrm_freqs)
  
  # simulate frequency contour and amplitude envelope of song elements and gaps
  frq_amp <- lapply(seq_len(n), function(x) {
    
    # number of freq values
    N <- round(x = steps * durs[x]/mean(durs), digits = 0)
    
    # simulate frequency modulation
    if (diff_fun != "pure.tone")
    sng_frq <- as.vector((df_fn(N = ifelse(N < 2, 2, N), sigma = sig2) * sample(c(-1, 1), 1)) + freqs[x]) else
    sng_frq <- rep(freqs[x], ifelse(N < 2, 2, N))
    
    
    # patch to avoid negative numbers
    sng_frq <- abs(sng_frq)
    sng_frq[sng_frq == 0] <- 0.01
    
    app_n <- round(durs[x] * (samp.rate * 1000), 0)
    
    sng_frq <- stats::spline(x = sng_frq, n = ifelse(app_n < 2, 2, app_n))$y
    
    frng <- range(sng_frq)
    
    sng_amp <- rep(amps[1], length(sng_frq))
    
    if (fin != 0 & fout != 0)
    sng_amp <- fade_env_wrblr_int(nvlp = sng_amp, fin = fin, fout = fout, shape = shape)
    
    # add starting gap
    if (x == 1) {
      
      if (!is.null(seed)) set.seed(seeds[x + n])
      gp_frq1 <- sample(1:((samp.rate * 1000) / 2),  round(gaps[1] * (samp.rate * 1000), 0))
      
      sng_frq <- c(gp_frq1, sng_frq)
      
      gp_amp1 <- rep(x = 0.000001, round(gaps[1] * (samp.rate * 1000), 0))
      
      sng_amp <- c(gp_amp1, sng_amp)
    }
    
    if (!is.null(seed)) set.seed(seeds[x + (n * 2)])
    gp_frq <- sample(1:((samp.rate * 1000) / 2),  round(gaps[x + 1] * (samp.rate * 1000), 0))
    
    gp_amp <- rep(x = 0.000001, round(gaps[x + 1] * (samp.rate * 1000), 0))
    
    frq <- c(sng_frq, gp_frq)
    
    amp <- c(sng_amp, gp_amp)
    
    return(data.frame(frq, amp, bottom.freq = round(frng[1], 3), top.freq = round(frng[2], 3), subunit = x))
  })
  
  frq_amp <- do.call(rbind, frq_amp)
  
  # add noise
  ns <- noisew(f = (samp.rate * 1000), d = nrow(frq_amp)/(samp.rate * 1000))
  
  # fix noise samples to match songs
  while(length(ns) < nrow(frq_amp)) ns[length(ns) + 1] <- sample(ns, 1)
  while(length(ns) > nrow(frq_amp)) ns <- ns[1:(length(ns) - 1)]
  
  # standardize noise amplitude (range = c(0, bgn))
  ns <- ns + abs(min(ns))
  ns <- ns / max(ns)
  ns <- ns * bgn
  frq_amp$amp <- (frq_amp$amp + ns) * 1000
  
  #create WAV
  wv <- synth2(env = frq_amp$amp, ifreq = frq_amp$frq * 1000, f = (samp.rate * 1000), plot = FALSE)
  
  if (harms > 1)
    for(i in seq_len(harms - 1))
      wv <- wv + synth2(env= frq_amp$amp /amps[1] * amps[i + 1], ifreq = frq_amp$frq * 1000 * hrm_freqs[i], f = (samp.rate * 1000), plot = FALSE)
  
  wv <- tuneR::Wave(left = wv, samp.rate = (samp.rate * 1000), bit = 16)
  
  # create selection table and save sound file
  if (selec_table)
  {
    if (is.null(file_name)) file_name <- gsub(" ", "_", paste0(format(Sys.time()), ".wav")) else
      file_name <- paste0(file_name, ".wav")
    
    # fix name if file already exists
    nchr <- nchar(file_name) - 4
    x <- 1
    
    while(file.exists(file.path(path, file_name))){
      file_name <- paste0(substr(file_name, 0, nchr),"_", x, ".wav")
    x <- x + 1
        }
    
    options(warn = -1)
    writeWave(object = wv, filename = file.path(path, file_name), extensible = FALSE)
    
    start <- cumsum(c(gaps[1], durs[- length(durs)] + gaps[-c(1, length(gaps))]))

    st <- data.frame(sound.files = file_name, selec = 1:n, start, end = c(start + durs), stringsAsFactors = FALSE, bottom.freq = tapply(frq_amp$bottom.freq, frq_amp$subunit, mean), top.freq = tapply(frq_amp$top.freq, frq_amp$subunit, mean))
    
  }
  
  if (selec_table) return(list(selec_table = st, wave = wv)) else
  return(wv)
}
