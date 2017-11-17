# warbleR

A tool to streamline the analysis of animal acoustic signal structure. The package offers functions for downloading avian vocalizations from the open-access online repository Xeno-Canto, displaying the geographic extent of the recordings, manipulating sound files, detecting acoustic signals or importing detected signals from other software, assessing performance of methods that measure acoustic similarity, conducting cross-correlations, dynamic time warping, measuring acoustic parameters and analysing interactive vocal signals, among others. Functions working iteratively allow parallelization to improve computational efficiency.The code in warbleR can be executed by less experienced R users, but has also been thoroughly commented, which will facilitate further customization by advanced users.


Install/load the package from CRAN as follows:

```r

# From CRAN would be
#install.packages("warbleR")

#load package
library(Rraven)

```

To install the latest developmental version from [github](http://github.com/) you will need the R package [devtools](https://cran.r-project.org/package=devtools):

```r
# From CRAN would be
#install.packages("warbleR")

# From github
devtools::install_github("maRce10/warbleR")


#load package
library(Rraven)

```

The package vignettes provides detailed examples for most functions in `warbleR`. You can pull up the vignettes as follows:

```r

vignette("warbleR_worflow_phase1")

vignette("warbleR_worflow_phase2")

vignette("warbleR_worflow_phase3")

```