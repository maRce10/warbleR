# warbleR

warbleR is a package designed to streamline acoustic analysis in R. This package 
allows users to collect open-access acoustic data or input their own data into a workflow that facilitates automated spectrographic visualization and acoustic measurements. Basic familiarity with the R environment is highly suggested, and the installation of required packages from the Comprehensive R Archive Network (CRAN) is assumed. 

The functions in this package facilitate downloading avian calls from 
Xeno-Canto, creating maps of Xeno-Canto recordings, converting .mp3 files to .wav files, checking .wav files, automatically detecting calls by amplitude, manually selecting calls within recordings, printing spectrograms of recordings, printing spectrograms of individual calls, measuring signal to noise ratio
and performing acoustic measurements. 


To install warbleR from UNIX (without RStudio):
1. Download the file warbleR <version number>.tar.gz.
2. Ensure that the following R packages, available from CRAN, are installed:
seewave, tuneR, maps, rjson, pbapply, RCurl.
3. Open a command window or terminal, and navigate to the folder to which the file was saved.
4. Type the command R CMD INSTALL warbleR

Upon successful installation, type library(warbleR) in the R Console to load the package, then type library(help=warbleR) for an index of commands.
