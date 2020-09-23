## ---- echo = FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------------

# remove all objects
rm(list = ls())

# unload all non-based packages
out <- sapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), function(x) try(detach(x, unload = FALSE, character.only = TRUE), silent = TRUE))

# load packages
X <- c("warbleR", "knitr")
invisible(lapply(X, library, character.only = TRUE))
# library(kableExtra)

options(knitr.table.format = "html") 
# opts_chunk$set(comment = "")
# opts_knit$set(root.dir = tempdir())
options(width = 150, max.print = 100)

# from https://stackoverflow.com/questions/28961431/computationally-heavy-r-vignettes, so that vignettes will be built upon installation, but not executed during R CMD check (which is contributing to the /doc being too large)
is_check <- ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_",
             "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))
knitr::opts_chunk$set(eval = !is_check, comment = "")

# for vignette checking and image file output
# setwd("~/Desktop/R/warbleR_example2/")

#website to fix gifs
#https://ezgif.com/optimize


## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  library(warbleR)
#  
#  # set your working directory appropriately
#  # setwd("/path/to/working directory")
#  
#  # run this if you have restarted RStudio between vignettes without saving your workspace (assuming that you are in your /home/username directory)
#  setwd(file.path(getwd(),"warbleR_example"))
#  
#  # Check your location
#  getwd()
#  

## ---- echo = TRUE, eval=FALSE-----------------------------------------------------------------------------------------------------------------------
#  
#  # The package must be loaded in your working environment
#  ls("package:warbleR")
#  

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------------------------------------------------------------
#  
#  # To run this example:
#  # Open Phae_hisnr.csv and modify the start coordinate of the first selection and the end coordinate of the second selection so that the signals overlap
#  
#  Phae.hisnr <- read.csv("Phae_hisnr.csv", header = TRUE)
#  str(Phae.hisnr)
#  head(Phae.hisnr, n = 15)
#  
#  # yields a data frame with an additional column (ovlp_sels) that indicates which selections overlap
#  Phae.hisnr <- ovlp_sels(X = Phae.hisnr, max.ovlp = 0)
#  
#  # run the function again but this time retain only the signals that don't overlap
#  Phae.hisnr <- ovlp_sels(X = Phae.hisnr, max.ovlp = 0, drop = TRUE)
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  specreator(Phae.hisnr, wl = 300, flim = c(2, 10), it = "jpeg", res = 150, osci = TRUE, ovlp = 90)
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # remove selections after deleting corresponding image files
#  Phae.hisnr2 <- filtersels(Phae.hisnr, it = "jpeg", incl.wav = TRUE)
#  nrow(Phae.hisnr2)
#  

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------------------------------------------------------------
#  
#  # if selections can be read, "OK" will be printed to check.res column
#  checksels(Phae.hisnr2, check.header = FALSE)
#  

## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # ### Cut selections into individual sound files
#  #
#  # Listening to signals complements visual inspection and classification. The function `cut_sels` can be very useful for aural comparison of selected signals. Selected signals can be played as individual sounds rather than having to open up entire sound files. As a word of caution, generating cuts of sound files will also propagate any naming errors present in your original files.
#  #
#  # `cut_sels` can also be used to your advantage if your original recordings are long (over 10-15 minutes). Some _warbleR_ functions, in particular `manualoc` will run slowly with long recordings, so it's helpful to use shorter duration sound files. You can make selections of shorter pieces of long original recordings, either in _Raven_ or _Syrinx_, and use `cut_sels` to generate shorter segments for smoother signal detection in `warbleR`.
#  
#  cut_sels(X = Phae.hisnr2, mar = 0.01, labels = c("sound.files", "selec"))
#  
#  # bug in the above cut_sels code
#  
#  # Error in apply(X[, sapply(X, is.factor)], 2, as.character) :
#  #   dim(X) must have a positive length
#  
#  # cut_sels(selec.table) # this works!
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  seltailor(Phae.hisnr2, wl = 300, flim = c(2,10), wn = "hanning", mar = 0.1, osci = TRUE, title = c("sound.files", "selec"), auto.next = TRUE)
#  
#  # Read in seltailor output after renaming the csv file
#  Phae.hisnrt <- read.csv("Phae_hisnrt.csv", header = TRUE)
#  str(Phae.hisnrt)
#  

## ---- eval=TRUE, echo=FALSE-------------------------------------------------------------------------------------------------------------------------

Phae.hisnrt <- read.csv("Phae_hisnrt.csv", header = TRUE)
str(Phae.hisnrt)


## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # highlight selected signals
#  lspec(Phae.hisnrt, wl = 300, flim = c(2, 10), ovlp = 10, sxrow = 6, rows = 15, it = "jpeg")
#  
#  # concatenate lspec image files into a single PDF per recording
#  # lspec images must be jpegs
#  lspec2pdf(keep.img = FALSE, overwrite = TRUE)
#  

## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # Note for later...lspec2pdf works on autodetec files in the working directory too...maybe including a suffix argument would help
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # we will use Phaethornis songs and selections from the warbleR package
#  data(list = c("Phae.long1", "selec.table"))
#  writeWave(Phae.long1, "Phae.long1.wav") #save sound files
#  
#  # subset selection table
#  # already contains the frequency range for these signals
#  st <- selec.table[selec.table$sound.files == "Phae.long1.wav",]
#  
#  # read wave file as an R object
#  sgnl <- tuneR::readWave(as.character(st$sound.files[1]))
#  
#  # create color column
#  st$colors <- c("red2", "blue", "green")
#  
#  # highlight selections
#  color.spectro(wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-90, 0, 5), dB = "B", X = st, col.clm = "colors", base.col = "skyblue",  t.mar = 0.07, f.mar = 0.1)
#  

## ---- eval = FALSE, echo = FALSE--------------------------------------------------------------------------------------------------------------------
#  
#  # was getting bugs using the xeno-canto recordings
#  # but code sort of works for the following code:
#  # problem is that code takes a while to run and then shows the whole long spectrogram
#  # suggestion for color spectro - an argument to zoom in on section of x-axis?
#  
#  X <- Phae.hisnrt[Phae.hisnrt$sound.files == "Phaethornis-longirostris-154072.wav", ]
#  X$colors <- c("red2", "blue", "green", "yellow", "orange")
#  
#  X2 <- frange(X)
#  # View(X2)
#  
#  color.spectro(wave = readWave("Phaethornis-longirostris-154072.wav"), wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-90, 0, 5),
#                dB = "B", X = X2, col.clm = "colors", base.col = "skyblue",  t.mar = 0.07, f.mar = 0.1)
#  

## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # spec_param takes a single selection from the selection table as input
#  spec_param(Phae.hisnrt[1, ], length.out = 5, ovlp = 90, wl = c(150, 900), wn = c("hanning", "bartlett"), collev.min = c(-60, -30), pal = "reverse.gray.colors.2", path = NULL, rm.axes = TRUE, cex = 0.45, flim = c(2, 10))
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # create a column of recording IDs for friendlier catalog labels
#  rec_ID <- sapply(1:nrow(Phae.hisnrt), function(x){
#      gsub(x = strsplit(as.character(Phae.hisnrt$sound.files[x]), split = "-")[[1]][3], pattern = ".wav$", replacement = "")
#  })
#  rec_ID
#  
#  Phae.hisnrt$rec_ID <- rec_ID
#  str(Phae.hisnrt)
#  
#  # set color palette
#  # alpha controls transparency for softer colors
#  cmc <- function(n) cm.colors(n, alpha = 0.8)
#  
#  catalog(X = Phae.hisnrt, flim = c(2, 10), nrow = 4, ncol = 3, height = 10, width = 10, tag.pal = list(cmc), cex = 0.8, same.time.scale = TRUE, mar = 0.01, wl = 300, gr = FALSE, labels = "rec_ID", tags = "rec_ID", hatching = 1, group.tag = "rec_ID", spec.mar = 0.4, lab.mar = 0.8, max.group.cols = 5)
#  
#  catalog2pdf(keep.img = FALSE, overwrite = TRUE)
#  
#  # assuming we are working from the warbleR_example directory
#  # the ~/ format does not apply to Windows
#  # make sure you have already moved or deleted all other pdf files
#  move.imgs(from = ".", it = "pdf", create.folder = TRUE, folder.name = "Catalog_image_files")
#  

## ---- eval = FALSE, echo = FALSE--------------------------------------------------------------------------------------------------------------------
#  
#  # suggestion for move.imgs
#  # add argument for regex so as not to delete/move all image files of a given type
#  # and be able to move just "Cat*.pdf"...etc

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  # now create a catalog without labels, tags, groups or axes
#  Phae.hisnrt$no_label <- ""
#  
#  # catalog(X = Phae.hisnrt, flim = c(1, 10), nrow = 4, ncol = 3, height = 10, width = 10, cex = 0.8, same.time.scale = TRUE, mar = 0.01, wl = 300, spec.mar = 0.4, rm.axes = TRUE, labels = "no_label", lab.mar = 0.8, max.group.cols = 5, img.suffix = "nolabel")
#  
#  catalog(X = Phae.hisnrt, flim = c(1, 10), nrow = 4, ncol = 3, height = 10, width = 10, tag.pal = list(cmc), cex = 0.8, same.time.scale = TRUE, mar = 0.01, wl = 300, gr = FALSE, labels = "no_label", spec.mar = 0.4, lab.mar = 0.8, max.group.cols = 5, img.suffix = "nolabels")
#  
#  catalog2pdf(keep.img = FALSE, overwrite = TRUE)
#  

