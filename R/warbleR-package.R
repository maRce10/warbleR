#' warbleR: A package to streamline bioacoustic analysis
#'warbleR is a package designed to streamline acoustic analysis in R. This 
#'package allows users to collect open-access acoustic data or input their 
#'own data into a workflow that facilitates automated spectrographic visualization 
#'and acoustic measurements. The functions facilitate downloading avian calls from Xeno-Canto,
#' creating maps of Xeno-Canto recordings, converting .mp3 files to .wav files, checking .wav 
#' files, automatically detecting calls by amplitude, manually selecting calls within recordings, 
#' printing spectrograms of recordings, printing spectrograms of individual calls, measuring signal 
#' to noise ratio and performing acoustic measurements in a batch proccess. 
#'      
#' The warbleR package offers three overarching categories of
#'   functions:
#'   
#'   \itemize{
#'   
#'   \item Xeno Canto downloads \item Sound file management \item Streamlined 
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
#' @importFrom maps map worldMapEnv
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
