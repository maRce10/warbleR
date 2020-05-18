#' warbleR: A package to streamline bioacoustic analysis
#'
#' warbleR is intended to facilitate the analysis of the structure of animal acoustic signals in R. Users can collect open-access avian recordings or enter their own data into a workflow that facilitates spectrographic visualization and measurement of acoustic parameters. warbleR makes use of the fundamental sound analysis tools of the seewave package, and offers new tools for acoustic structure analysis. These tools are available for batch analysis of acoustic signals.
#'
#' The main features of the package are:
#'   \itemize{
#'   \item The use of loops to apply tasks through acoustic signals referenced in a selection table
#'   \item The production of images in the working folder with spectrograms that allow to organize data and verify acoustic analyzes
#'   }
#'   
#' The package offers functions to:
#'   \itemize{
#'   \item Explore and download Xeno Canto recordings
#'   \item Explore, organize and manipulate multiple sound files
#'   \item Detect signals automatically (in frequency and time)
#'   \item Create spectrograms of complete recordings or individual signals
#'   \item Run different measures of acoustic signal structure
#'   \item Evaluate the performance of measurement methods
#'   \item Catalog signals
#'   \item Characterize different structural levels in acoustic signals
#'   \item Statistical analysis of duet coordination
#'   \item Consolidate databases and annotation tables
#'   }
#'   
#' Most of the functions allow the parallelization of tasks, which distributes the tasks among several processors to improve computational efficiency. Tools to evaluate the performance of the analysis at each step are also available. In addition, warbleR satisfies the need for rigorous open source bioacoustic analysis, which facilitates opportunities for use in research and innovation of additional custom analyzes.
#'      
#' The warbleR package offers three overarching categories of
#'   functions:
#'   
#' @section Obtaining animal vocalization data:
#'   
#'   \code{\link{querxc}}: Download recordings and/or metadata from 'Xeno-Canto'
#'   
#'   \code{\link{find_annotations}}: Obtain annotations from 'audioblast.org' data base
#'   
#'   \code{\link{sim_songs}}: Simulate animal vocalizations
#'   
#'   
#' @section Managing sound files:
#'   
#'   \code{\link{selection_table}}: Create 'selection_table' class objects
#'   
#'   \code{\link{mp32wav}}: Convert several .mp3 files in working directory to .wav
#'   format
#'   
#'   \code{\link{checksels}}: Check whether selections can be read by subsequent functions
#'   
#'   \code{\link{checkwavs}}: Check whether .wav files can be read by subsequent 
#'   functions and the minimum windows length ("wl" argument) that can be used
#'   
#'   \code{\link{fixwavs}}: Fix .wav files so they can be read by other functions
#'   
#'   \code{\link{resample_est}}: Resample wave objects in extended selection tables
#'   
#'   \code{\link{wavdur}}: Determine the duration of sound files
#'   
#'   \code{\link{cut_sels}}: Cut selections from a selection table into individual sound files
#'  
#'   \code{\link{rm_sil}}: Remove silence segments from wave files
#'   
#'   \code{\link{rm_channels}}: Remove channels in wave files
#'   
#'   \code{\link{consolidate}}: Consolidate sound files into a single folder
#'   
#'   \code{\link{selection_table}}: Create double-checked and self-contained selection tables
#'   
#'   \code{\link{fix_extended_selection_table}}: Fix attributes of extended selection tables
#'   
#' @section Exploring/analyzing signal structure:
#'   
#'   \code{\link{autodetec}}: Automatically detect start and 
#'   end of acoustic signals
#'   
#'   \code{\link{manualoc}}: Interactive spectrographic view to measure start and 
#'   end of acoustic signals
#' 
#'   \code{\link{autodetec}}: Automatic detection of acoustic signals based on ampltiude 
#'
#'   \code{\link{seltailor}}: Interactive view of spectrograms to tailor start and end of selections
#'   
#'   \code{\link{sig2noise}}: Measure signal-to-noise ratio across multiple files
#'   
#'   \code{\link{trackfreqs}}: Create spectrograms to visualize frequency 
#'   measurements
#'   
#'   \code{\link{filtersels}}: Filter selection data frames based on filtered image files
#'      
#'   \code{\link{frange}}: Detect frequency range iteratively from signals in a selection table
#'   
#'   \code{\link{frange.detec}}: Detect frequency range in a Wave object   
#'      
#'   \code{\link{specan}}: Measure acoustic parameters on selected acoustic 
#'   signals
#'   
#'   \code{\link{mfcc_stats}}: Calculate descriptive statistics on Mel-frequency cepstral coefficients
#'   
#'   \code{\link{xcorr}}: Pairwise cross-correlation of multiple signals  
#'   
#'   \code{\link{dfts}}: Extract the dominant frequency values across the signal as a time series
#'   
#'   \code{\link{ffts}}: Extract the fundamental frequency values across the signal as a time series
#'   
#'   \code{\link{find_peaks}}: Find peaks in cross-correlation scores from \code{\link{xcorr}}
#'   
#'   \code{\link{find_annotations}}: Download sound file annotations and metadata from \href{https://audioblast.org/annotations/}{audioblast.org}.
#'   
#'   \code{\link{sp.en.ts}}: Extract the spectral entropy values across the signal as a time series
#'   
#'   \code{\link{dfDTW}}: Calculate acoustic dissimilarity using dynamic time warping
#'    on dominant frequency contours
#'   
#'   \code{\link{ffDTW}}: Calculate acoustic dissimilarity using dynamic time warping
#'   on fundamental frequency contours
#'   
#'   \code{\link{wpd_features}}: Measure wavelet packet decomposition features 
#'   
#'   \code{\link{compare.methods}}: Produce graphs to visually assess performance of acoustic 
#'   distance measurements 
#'   
#'   \code{\link{coor.test}}: Assess statistical significance of singing coordination 
#'   
#'   \code{\link{ovlp_sels}}: Find selections that overlap in time within a given sound file
#'   
#'   \code{\link{track_harm}}:  Track harmonic frequency contour
#'   
#' @section Graphical outputs:
#'   
#'   \code{\link{xcmaps}}: Create maps to visualize the geographic spread of 'Xeno-Canto' recordings
#'   
#'   \code{\link{catalog}}: Produce a vocalization catalog with spectrograms in and array with
#'   several rows and columns  
#'   
#'   \code{\link{catalog2pdf}}: Combine catalog images to single pdf files
#'   
#'   \code{\link{coor.graph}}: Create graphs of coordinated singing 
#'   
#'   \code{\link{color.spectro}}: Highlight spectrogram regions
#'   
#'   \code{\link{lspec}}: Produce spectrograms of whole recordings split into 
#'   multiple rows
#'   
#'   \code{\link{lspec2pdf}}: Combine lspec images to single pdf files
#'   
#'   \code{\link{specreator}}: Create spectrograms of manualoc selections
#'   
#'   \code{\link{snrspecs}}: Create spectrograms to visualize margins over which 
#'   noise will be measured by sig2noise
#'   
#'   \code{\link{phylo_spectro}}: Add spectrograms onto phylogenetic trees
#'   
#' @exportClass selection_table
#' @exportClass extended_selection_table
#' @import NatureSounds
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
#' @importFrom methods formalArgs new is slotNames
# #' @importFrom pracma findpeaks
# #' @importFrom Sim.DiffProc BB GBM
#' @importFrom dtw dtwDist
#' @importFrom stats cor dist aggregate approx ave prcomp time ts predict smooth.spline complete.cases spline weighted.mean na.omit var sd rlnorm fft
#' 
#' @author Marcelo Araya-Salas & Grace Smith Vidaurre
#'   
#'   Maintainer: Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#'   
#' @docType package
#' @name warbleR
#' @details License: GPL (>= 2)  
NULL
#> NULL 
#'
