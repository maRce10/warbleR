# *warbleR 1.1.15*
## *(CRAN release date: )*

</br>

## New functions:

* multi_DTW: wrapper on dtw for comparing signals described by multiple contours

## Changes and additions:

* 'selection_table` now can take files found in different directories and create extended selection tables

## Bug fixes:

* Using 'wav.path' from 'warbleR_options' in 'catalog'

===============================================================================================

# *warbleR 1.1.14*
## *(CRAN release date: 2018-06-22)*

</br>

## Changes and additions:

* 'open_wd' example fixed (requested by CRAN)

===============================================================================================

# *warbleR 1.1.13*
## *(CRAN release date: 2018-05-18)*

</br>

## Changes and additions:

* new function names (print(new_function_names))
* object class 'extended_selection_table' to create self-contained acoustic data sets
* 'before' and 'eq.dur' arguments in 'snrspec' function
* 'ts.df' argument in 'seltailor' function to adjust frequency contours
* 'ts.df' argument in 'seltailor' function to adjust frequency contours
* 'by.song' argument in 'specreator' to create full song spectrograms with labeled elements

## Bug fixes:

* Change "http" to "https" protocol for accessing Xeno-Canto recordings in "querxc"

## New functions:

* selection_table: converts data frames into an object of classes 'selection_table' or 'extended_selection_table'
* generic S3 methods for and 'print', 'cbind' and '[' for  classes 'selection_table' or 'extended_selection_table'
* song_param: calculate acoustic parameters at the song (or other hierarchical) level
* spec_param: plot catalog of a single selection with varying spectrogram (display) parameters
* read_wave: wrapper for tuneR's readWave that ignores exntesion file case mismatches
* warbleR_options: function to set global parameters
*open_wd: open current working directory in wave browser

===============================================================================================

# *warbleR 1.1.12*
## *(CRAN release date: 2018-03-13)*

</br> 

## Changes and additions:

* Parallel computing available in 'querxc' funtion when getting metadata
* Parallel computing available in 'checksels' funtion 
* New arguments in 'seltailor' to allow tailoring frequency contours 
* Parallel computing with and without progress bar in all iterative functions
* Parallel computing in windows OS with and without progress bar
* New arguments in 'specreator' to customize control lines/box around selections 
* Replace internal "message" functions by "cat" (so warnings are printed in black, no red)

## New functions:

* track_harm: track harmonic frequency contour
* sim_song: simulate songs 


===============================================================================================

# *warbleR 1.1.10*
## *(CRAN release date: 2018-08-28)/

</br>

## Changes and additions:

* Example sound files have been moved to the new R package 'NatureSounds' (which is now a dependency)

## Bug fixes:

* Problem with multiple tags in 'catalog'

## New functions:

* rm_sil: removes silence segments (i.e. segments with very low amplitude values) from wave files
* consolidate: copies (sound) files scattered in several directories into a single folder

===============================================================================================

# *warbleR 1.1.9*
## *(CRAN release date: 2017-11-18)*

</br> 

## Changes and additions:

* "low.freq" and "high.freq" column names in default table format are now "bottom.freq" and "top.freq" respectively
* Moved 'imp.syrinx' and 'imp.raven' functions to new R package Rraven
* Added a new object class 'selection.table' 
* Split vignette into a series of three with new functions added to the workflow
* New arguments "title", "by.row", "prop.mar", "box" and "rm.axes" in "catalog" function to further customize catalog setup
* New arguments "spec.mar", "lab.mar" and "group.tag" in "catalog" function to color groups of selections
* "trackfreqs", "dfts", and "dfDTW"  functions can use "frange.detec" internally to set bandpass limits (see "frange.detec" argument)
* New argument "index" in "seltailor" function to indicate which selection should be tailored
* New argument "wl.freq" in "specan" function to set windows length independently for time and frequency domain measurements
* New measurement "meanpeakf" in specan
* "imp.raven" and "imp.syrinx" now import low and high frequency in kHz (if all.data == FALSE)
* Mew argument "bp" (bandpass) and "wl" in "sig2noise" function to calculate signal-to-noise ratio within a frequency range

## New functions:

* is.selection.table: checks if an object is of class 'selection.table'
* make.selection.table: creates an object of class 'selection.table'
* frange.detec: detects the frequency range of signals in wave objects
* frange: applies 'frange.detec' function iteratively on signals listed in a selection table
* move.imgs: copy/cut image files to folders
* ovlp_sels: find overlapping/duplicated selections
* cut_sels: generates individual sound files (.wav) for each selection in a selection table
* color_spectro: produces spectrograms in which signals are highlighted with colors
* exp.raven: exports selection tables to a .txt file that can be opened in Raven

## Bug fixes:

* Reading wave files with no stereo slot in "specan" and "checksels"

===============================================================================================

# *warbleR 1.1.8*
## *(CRAN release date: 2017-04-26)*

## Bug fixes:

* installation in some linux distributions

===============================================================================================

# *warbleR 1.1.7*
## *(CRAN release date: 2017-04-24)*

</br> 

## Changes and additions:

* "seltailor" function now can use external graphic devices (X11 or quartz)

## Bug fixes:

* installation in windows OS fixed (pbmcapply conflict in previous version)

===============================================================================================

# *warbleR 1.1.6* 
## *(CRAN release date: 2017-04-05)*

</br>

## New functions:

* catalog: create song catalogs (i.e. images with multiple spectrograms in several rows/columns that can be color-labeled)
* catalog2pdf: convert several catalog jpeg's images into single pdf files
* fixwavs: convert corrupted files to a format that can be imported into R

</br> 

## Changes and additions:

* added new acoustic parameters to "specan" function related to distribution of energy in the time axis. Also time entropy ("time.ent") and overall entropy ("entropy") were added
* "centroid" and "mode" parameters were removed from "specan" function
* sig2noise function can return signal-to-noise ratio in dB ("in.dB" argument). It can also measure noise at both sides of the signal or just right before ("before" argument)   
* scale argument in dfDTW and ffDTW to allow to focus dynamic time warping comparisons on the frequency contours while ignoring differences in pitch
* NAs can be removed from xcorr function output. They can also be ignored when using XCORR in compare.methods function 
* filtersels can return selections with or without image files ("missing" argument). It can also return a data frame or the row index of the selections ("index" argument)
* imp.raven function can import raven selections from files that do not share all the columns and extract the sound file name from the selection table name
* FF function (from tuneR package) now can be used for measuring fundamental frequency (in trackfreqs, specan, ffts and ffDTW functions). Seems to be more accurate and faster than fund function from seewave 
* window overlap can now be defined by users for fundamental and dominant frequency 
measures in specan function
* More arguments to control dynamic time warping settings in dfDTW and ffDTW
* Progress bar is available for parallel computing (when parallel > 1) in Linux and for some funtions also in iOS. It works using the function mcpbapply::pbmclapply
*Spectrogram creating functions (manualoc, specreator, trackfreqs, dfts, ffts, dfDTW, ffDTW, lspec, catalog) can performe faster (4-10 times) using a different image algorithm. This can be called with the argument "fast.spec". Note that not all spectrogram options are available when fast.spec = TRUE 
* selections longer than 20 s can be analyzed with specan function (previously it returned an error, this is going to be slow anyways)
* "clip.edges" argument in compare.methods function to set removing mising values at the start and end of signals when using dfDTW and ffDTW methods  
* "treshold" argument in compare.methods function to set the amplitude detection threshold for ffDTW, dfDTW and SP methods  
* "exclude" agument in imp.syrinx function to exclude selection files that cannot be read
* "rm.imcomp" argument in coor.test function to exclude singing events that only have one individual
* "cutoff" argument in coor.test function to determine the minimum number of signals per individual needed for a singing event to
be included
* "rm.solo" argument in coor.test to control if signals that are not intercalated at the start or end of the 
sequence are removed. For instances the sequence of signals A-A-A-B-A-B-A-B-B-B (in which A and B represent 
different individuals) would be subset to A-B-A-B-A-B
* "incl.wav" argument in filtersels function to indicate if the sound file extension is included in the image files
* 3 different options (formulas) for calculating signal-to-noise ratio are now available (sig2noise function). In addition the "eq.dur" argument allows to measure a noise segment of the same duration than the signal
* grid can be removed from spectrograms in compare.methods function

## Bug fixes:

* error produced when calculating frequency limits based on dominant frequency contours in xcorr (when frange is not provided)
* error for identifying readable sound files in some specific .wav formats in checkwavs function
* error when comparing signals with a high duration difference in xcorr

===============================================================================================

# *warbleR 1.1.5* 
## *(CRAN release date: 2017-01-19)*

</br> 

## Changes and additions:

* "frange" argument in seltailor to define also the frequency range (low.f and high.f)
* trackfreqs, specan now can use low.f and high.f as bandpass limits
* trackfreqs and specreator can plot boxes around signals if low.f and high.f are provided
* manualoc.df data frame example replaced by selec.table (wich includes low.f and high.f columns)
* complex searches are now allowed in querxc() following xeno-canto advance query syntax
* added 'clip.edges' argument to dfts/ ffts/ dfDTW/ ffDTW/sp.en.ts functions to remove missing values at the start or end of signals
* more detailed checksels output (now including, sampling rate, duration, number of samples and bits)

## New functions:

*  sp.en.ts: extracts the spectral entropy across signals as a time series iteratively


===============================================================================================

# *warbleR 1.1.4* 
## *(CRAN release date: 2016-11-14)*

</br> 

## Changes and additions:

* "pb" argument to allow users to knock down progress bars and messages (in looping functions)
* modification to (slightly) improve 'lspec' performance 
* new "sp" argument in compare.methods to allow users to input their own spectral parameters
* new "custom.contour" argument in trackfreqs to allow users to input their own frequency contours 

## Bug fixes:

* bug fix for setting 'path' argument in several functions
* bug fix to due to missing values (undetected) in amplitude detection in trackfreqs, ffts, dfts, ffDTW, and dfDTW  
 
## New functions:

*  filtersels: subsets selection data frames based on manually filtered image files
*  lspec2pdf: combines lspec images to single pdf files iteratively

</br>