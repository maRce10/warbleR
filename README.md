# warbleR

A tool to streamline the analysis of animal acoustic signal structure. The package offers functions for downloading avian vocalizations from the open-access online repository [Xeno-Canto](http://xeno-canto.org/), displaying the geographic extent of the recordings, manipulating sound files, detecting acoustic signals, assessing performance of methods that measure acoustic similarity, conducting cross-correlations, dynamic time warping, measuring acoustic parameters and analysing interactive vocal signals, among others. Functions working iteratively allow parallelization to improve computational efficiency.The code in warbleR can be executed by less experienced R users, but has also been thoroughly commented, which will facilitate further customization by advanced users.


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
library(warbleR)

```

The package vignettes provide detailed examples for most [warbleR](https://cran.r-project.org/package=warbleR) functions. You can pull up the vignettes as follows:

```r

vignette("warbleR_workflow_phase1")

vignette("warbleR_workflow_phase2")

vignette("warbleR_workflow_phase3")

```

A full description of the package can be founf in this journal article.


Please cite [warbleR](https://cran.r-project.org/package=warbleR) as follows:

Araya-Salas, M. and Smith-Vidaurre, G. (2017), *warbleR: an r package to streamline analysis of animal acoustic signals*.   Methods Ecol Evol. 8, 184-191. [PDF](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12624/epdf)

NOTE: please also cite the [tuneR](https://cran.r-project.org/package=tuneR) and [seewave](https://cran.r-project.org/package=seewave) packages if you use any spectrogram-creating or acoustic-measuring function"
