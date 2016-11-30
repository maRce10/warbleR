# NEWS


# warbleR 1.1.5 
##### (Release date: XXX)

## Changes and additions:

* complex searches are now allowed in querxc() following xeno-canto advance query syntax
* added 'clip.edges' argument to dfts/ ffts/ dfDTW/ ffDTW/sp.en.ts functions to remove missing values at the start or end of signals
* more detailed checksels output (now including, sampling rate, duration, number of samples and bits)

## New functions:

*  sp.en.ts: extracts the spectral entropy across signals as a time series iteratively


===============================================================================================

# warbleR 1.1.4 
##### (Release date: 2016-11-14)

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


