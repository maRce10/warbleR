#' Simulate animal vocalizations
#' 
#' \code{sim_songs} simulate animal vocalizations in a wave object under brownian motion frequency drift.
#' @usage sim_songs(n = 1, durs = 0.2, harms = 3, amps = c(1, 0.5, 0.2), gaps = 0.1, freqs = 5, 
#' samp.rate = 44.1, sig2 = 0.5, steps = 50, bgn = 0.5, seed = NULL, diff_fun = "GBM") 
#' @param n Number of song subunits (e.g. elements). Default is 1.
#' @param durs Numeric vector with the duration of subunits in seconds. It should either be a single value (which would 
#' be used for all subunits) or a vector of length \code{n}. 
#' @param harms NUmeric vector of length 1 specifyin the number of harmonics to simulate. 1 indicates that only the fundamental
#' frequency harmonic will be simulated.
#' @param amps Numeric vector with the relative amplitude of each of the harmonics (including the fundamental frequency).
#' @param gaps Nueric vector with the duration of gaps (silence between subunits) in seconds. It should either be a single value
#'  (which would  be used for all subunits) or a vector  of length \code{n + 1}. 
#' @param freqs Nueric vector with the initial frequency of the subunits (and ending frequency if \code{diff_fun == "BB"}) in kHz. 
#' It should either be a single value (which would  be used for all subunits) or a vector of length \code{n}. 
#' @param samp.rate Numerirc vector of length 1. Sets the sampling frequency of the wave object (in kHz). Default is 44.1.
#' @param sig2 Numeric vector of length 1 defining the sigma value of the brownian motion model. Higher values will produce faster 
#' frequency modulations. Ignored if \code{diff_fun == "BB"}. Default is 0.1. Check the \code{\link[Sim.DiffProc]{BB}} 
#' for more details. 
#' @param steps Numeric vector of length 1. Controls the mean number of segments in which each song subunit is split during 
#' the brownian motion process. If not all subunits have the same duration, longer units will be split in more steps (although 
#' the average duration subunit will have the predefined number of steps). Default is 50.
#' @param bgn Numeric vector of length 1 indicating the background noise level. 0 means no additional noise will 1 means 
#' noise at the same amplitude than the song subunits. Default is 0.5.
#' @param seed Numeric vector of length 1. This allows users to get the same results in different runs (using  \code{\link[base]{set.seed}} internally). Default is \code{NULL}.
#' @param diff_fun Character vector of length 1 controlling the function used to simulate the brownian motion process of 
#' frequency drift across time. Only "BB" and "GBM" are accepted at this time.Check the \code{\link[Sim.DiffProc]{BB}} 
#' for more details. 
#' @seealso \code{\link{querxc}} for for downloading bird vocalizations from an online repository.
#' @export
#' @name sim_songs
#' @details This functions uses a brownian motion stochastic process to simulate animal vocalizations (i.e. frequency traces across time). 
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
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on feb-22-2018 (MAS)

sim_songs <- function(n = 1, durs = 0.2, harms = 3, amps = c(1, 0.5, 0.2), gaps = 0.1, freqs = 5, samp.rate = 44.1, 
                      sig2 = 0.5, steps = 50, bgn = 0.5, seed = NULL, diff_fun = "GBM") {
  
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
  if (diff_fun == "GBM") df_fn <- Sim.DiffProc::GBM else df_fn <- Sim.DiffProc::BB
  
  if (!is.null(seed))
    seeds <- 1:(3 * n) + seed  
  
  hrm_freqs <- c(1/2, 1/3, 2/3, 1/4, 3/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10)
  hrm_freqs <- sort(1 / hrm_freqs)
  
  # simulate frequency contour and amplitude envelope of song elements and gaps
  frq_amp <- lapply(seq_len(n), function(x) {
    
    N <- round(x = steps * durs[x]/mean(durs), digits = 0)
    
    if (!is.null(seed)) set.seed(seeds[x])
    sng_frq <- as.vector((df_fn(N = ifelse(N < 2, 2, N), sigma = sig2) * sample(c(-1, 1), 1)) + freqs[x]) 
    
    app_n <- round(durs[x] * (samp.rate * 1000), 0)
    
    sng_frq <- stats::spline(x = sng_frq, n = ifelse(app_n < 2, 2, app_n))$y
    
    sng_amp <- rep(amps[1], length(sng_frq))
    
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
    
    return(data.frame(frq, amp))
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
  
  return(wv)
}