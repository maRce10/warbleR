#' warbleR: A package to streamline acoustic analysis
#'   
#' The warbleR package offers three overarching categories of
#'   functions:
#'   
#'   \itemize{
#'   
#'   \item Xeno Canto downloads \item mp3 file management \item Streamlined 
#'   acoustic analysis in R }
#'   
#' @section Accessing Xeno Canto:
#'   
#'   \strong{querxc()}: Download recordings and metadata from Xeno Canto
#'   
#'   \strong{xcmaps()}: Create maps to visualize the geographic spread of Xeno 
#'   Canto recordings
#'   
#' @section Managing mp3 files:
#'   
#'   \strong{mp32wav()}: Convert several .mp3 files in working directory to .wav
#'   format
#'   
#'   \strong{checkwavs()}: Check whether .wav files can be read by subsequent 
#'   functions
#'   
#' @section Streamlining acoustic analysis in R:
#'   
#'   \strong{manualoc()}: Interactive spectrographic view to measure start and 
#'   end of acoustic signals
#'   
#'   \strong{lspec()}: Produce spectrograms of whole recordings split into 
#'   multiple rows
#'   
#'   \strong{specreator()}: Create spectrograms of manualoc selections
#'   
#'   \strong{snrspecs()}: Create spectrograms to visualize margins over which 
#'   noise will be measured by sig2noise
#'   
#'   \strong{sig2noise()}: Measure signal to noise ratio across multiple files
#'   
#'   \strong{trackfreqs()}: Create spectrograms to visualize frequency 
#'   measurements
#'   
#'   \strong{specan()}: Measures acoustic parameters on selected acoustic 
#'   signals
#'   
#' @importFrom rjson fromJSON
#' @importFrom pbapply pbapply pbsapply pblapply
#' @importFrom RCurl getURL
#' @importFrom maps map
#' @importFrom tuneR readWave readMP3 writeWave play
#' @importFrom seewave ffilter inputw env spectro cutw pastew noisew spec 
#'   specprop fpeaks fund dfreq
#'   
#' @author Marcelo Araya-Salas, Grace Smith Vidaurre, Hua Zhong 
#'   
#'   Maintainer: Marcelo Araya-Salas (marceloa27@@gmail.com)
#'   
#' @docType package
#' @name warbleR
#' @details License: GPL (>= 2)  
NULL
#> NULL 
#' 
