#' warbleR: A package to streamline bioacoustic analysis
#' 
#' warbleR is a package designed to streamline analysis of (bio)acoustic signals in R. This 
#' package allows users to collect open-access avian vocalizations data or input their 
#' own data into a workflow that facilitates spectrographic visualization 
#' and measurement of acoustic parameters in a batch process. The functions facilitate searching and downloading 
#' avian vocalizations from Xeno-Canto \url{http://www.xeno-canto.org/}, creating maps of Xeno-Canto recordings, 
#' converting .mp3 files to .wav files, checking .wav files, automatically detecting acoustic signals, selecting 
#' them manually, printing spectrograms of whole recordings or individual signals, measuring signal 
#' to noise ratio, cross-correlation and performing acoustic measurements.
#'      
#' The warbleR package offers three overarching categories of
#'   functions:
#'   
#'   \itemize{
#'   
#'   \item Obtaining avian vocalization data \item Sound file management \item Streamlined 
#'   (bio)acoustic analysis in R }
#'   
#' @section Obtaining avian vocalization data:
#'   
#'   \code{\link{querxc}}: Download recordings and metadata from Xeno-Canto
#'   
#'   \code{\link{xcmaps}}: Create maps to visualize the geographic spread of Xeno-Canto recordings
#'   
#'   \code{\link{imp.syrinx}}: Importing Syrinx selections
#'   
#'   \code{\link{imp.raven}}: Importing Raven selections
#'   
#' @section Managing sound files:
#'   
#'   \code{\link{mp32wav}}: Convert several .mp3 files in working directory to .wav
#'   format
#'   
#'   \code{\link{checkwavs}}: Check whether .wav files can be read by subsequent 
#'   functions
#'   
#' @section Streamlining analysis of acoustic signal structure in R:
#'   
#'   \code{\link{autodetec}}: Automatically detect start and 
#'   end of acoustic signals
#'   
#'   \code{\link{manualoc}}: Interactive spectrographic view to measure start and 
#'   end of acoustic signals
#' 
#'   \code{\link{autodetec}}: Automatic detection of acoustic signals based on ampltiude 
#'
#'   \code{\link{lspec}}: Produce spectrograms of whole recordings split into 
#'   multiple rows
#'   
#'   \code{\link{specreator}}: Create spectrograms of manualoc selections
#'   
#'   \code{\link{snrspecs}}: Create spectrograms to visualize margins over which 
#'   noise will be measured by sig2noise
#'   
#'   \code{\link{sig2noise}}: Measure signal-to-noise ratio across multiple files
#'   
#'   \code{\link{trackfreqs}}: Create spectrograms to visualize frequency 
#'   measurements
#'   
#'   \code{\link{specan}}: Measure acoustic parameters on selected acoustic 
#'   signals
#'   
#'   \code{\link{xcorr}}: Pairwise cross-correlation of multiple signals  
#'   
#'   \code{\link{xcorr.graph}}: Pairwise cross-correlation of multiple signals
#'   
#'   \code{\link{dfts}}: Extract the dominant frequency values as a time series
#'   
#'   \code{\link{ffts}}: Extract the fundamental frequency values as a time series
#'   
#'   \code{\link{dfDTW}}: Calculates acoustic dissimilarity using dynamic time warping
#'    on dominant frequency contours
#'   
#'   \code{\link{ffDTW}}: Calculates acoustic dissimilarity using dynamic time warping
#'   on fundamental frequency contours

#'      \code{\link{compare.methods}}: Produces graphs to visually assess performance of acoustic 
#'   distance measurements 
#'   
#'   \code{\link{coor.graph}}: Creat graphs of coordinated singing 
#'   
#'   \code{\link{coor.test}}: Assess statistical significance of singing coordination 
#'   
#'   \code{\link{seltailor}}: nteractive view of spectrograms to tailor start and end of selections
#'   
#' @import maps
#' @import rjson
#' @import RCurl
#' @import pbapply
#' @import tuneR
#' @import seewave
#' @import fftw
#' @import graphics
#' @import grDevices
#' @import utils
#' @import parallel
#' @importFrom dtw dtwDist
#' @importFrom stats cor dist aggregate approx ave princomp time ts    
#' 
#' @author Marcelo Araya-Salas, Grace Smith Vidaurre, Hua Zhong 
#'   
#'   Maintainer: Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#'   
#' @docType package
#' @name warbleR
#' @details License: GPL (>= 2)  
NULL
#> NULL 
#'
