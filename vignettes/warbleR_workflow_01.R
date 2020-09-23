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
opts_knit$set(root.dir = tempdir())
options(width = 150, max.print = 100)

# from https://stackoverflow.com/questions/28961431/computationally-heavy-r-vignettes, so that vignettes will be built upon installation, but not executed during R CMD check (which is contributing to the /doc being too large)
is_check <- ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_",
             "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))
knitr::opts_chunk$set(eval = !is_check, comment = "")

# for vignette checking and image file output
# setwd("~/Desktop/R/warbleR_example2/")

#website to fix gifs
#https://ezgif.com/optimize

vgn.path <- getwd()

# read data example for Rraven code
sels <- read.csv("Raven_sels.csv", stringsAsFactors = FALSE)


## ---- echo = TRUE, eval=FALSE-----------------------------------------------------------------------------------------------------------------------
#  
#  ### Install packages from CRAN
#  # Note that if you install from CRAN, then don't run the code to install from GitHub below, and vice versa
#  install.packages("warbleR")
#  install.packages("Rraven")
#  
#  ### Alternatively, install warbleR and Rraven from GitHub repositories, which contain the latest updates
#  # Run this ONLY if devtools is not already installed
#  install.packages("devtools")
#  
#  # Load devtools to access the install_github function
#  library(devtools)
#  
#  # Install packages from GitHub
#  # install_github("maRce10/warbleR")
#  # install_github("maRce10/Rraven")
#  # install_github("maRce10/NatureSounds")
#  
#  # Load warbleR and Rraven into your global environment
#  X <- c("warbleR", "Rraven")
#  invisible(lapply(X, library, character.only = TRUE))
#  

## ---- echo = TRUE, eval=FALSE-----------------------------------------------------------------------------------------------------------------------
#  
#  # The package must be loaded in your working environment
#  ls("package:warbleR")
#  

## ---- echo = TRUE, eval=FALSE-----------------------------------------------------------------------------------------------------------------------
#  
#  # Create a new directory and set your working directory (assuming that you are in your /home/username directory)
#  dir.create(file.path(getwd(),"warbleR_example"))
#  setwd(file.path(getwd(),"warbleR_example"))
#  
#  # Check your location
#  getwd()
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # Load Raven example selection tables
#  data("selection_files")
#  
#  # Write out Raven example selection tables as physical files
#  out <- lapply(1:2, function(x)
#    writeLines(selection_files[[x]], con = names(selection_files)[x]))
#  
#  # Write example sound files out as physical .wav files
#  data(list = c("Phae.long1", "Phae.long2"))
#  
#  writeWave(Phae.long1, "Phae.long1.wav")
#  writeWave(Phae.long2, "Phae.long2.wav")
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # Import selections
#  sels <- imp_raven(all.data = FALSE, freq.cols = FALSE, warbler.format = TRUE)
#  str(sels)
#  
#  # Write out the imported selections as a .csv for later use
#  write.csv(sels, "Raven_sels.csv", row.names = FALSE)
#  

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------------------------------------------------------------
#  
#  sels <- selection_table(X = sels)
#  str(sels)
#  class(sels)
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Query xeno-canto for all Phaethornis recordings (e.g., by genus)
#  Phae <- querxc(qword = "Phaethornis", download = FALSE)
#  
#  # Check out the structure of resulting the data frame
#  str(Phae)
#  

## ---- eval = TRUE, echo = FALSE, message = FALSE----------------------------------------------------------------------------------------------------

# Phae <- querxc(qword = "Phaethornis", download = FALSE) 

# write.csv(Phae, file = "~/Dropbox/warbleR/vignettes/Phae.XC.csv", row.names = FALSE)

Phae <- read.csv(file.path(vgn.path, "Phae.XC.csv"), stringsAsFactors = FALSE)

# Check out the structure of resulting the data frame
str(Phae)



## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Query xeno-canto for all Phaethornis longirostris recordings
#  Phae.lon <- querxc(qword = "Phaethornis longirostris", download = FALSE)
#  
#  # Check out the structure of resulting the data frame
#  str(Phae.lon)
#  

## ---- eval = TRUE, echo = FALSE, message = FALSE----------------------------------------------------------------------------------------------------

# Phae.lon <- querxc(qword = "Phaethornis longirostris", download = FALSE)

# write.csv(Phae.lon, file = "~/Dropbox/warbleR/vignettes/Phae.lon.XC.csv", row.names = FALSE)

Phae.lon <- read.csv(file.path(vgn.path, "Phae.lon.XC.csv"), stringsAsFactors = FALSE)

# Check out the structure of resulting the data frame
str(Phae.lon)


## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Image type default is jpeg, but tiff files have better resolution
#  
#  # When the data frame contains multiple species, this will yield one map per species
#  xcmaps(X = Phae, img = TRUE, it = "tiff") # all species in the genus
#  xcmaps(X = Phae.lon, img = FALSE) # a single species
#  

## ---- eval=TRUE, echo=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------

xcmaps(X = Phae.lon, img = FALSE) 


## ---- eval=TRUE, echo=TRUE--------------------------------------------------------------------------------------------------------------------------

# How many recordings are available for Phaethornis longirostris?
nrow(Phae.lon) 

# How many signal types exist in the xeno-canto metadata?
levels(Phae.lon$Vocalization_type)

# How many recordings per signal type?
table(Phae.lon$Vocalization_type)


## ---- eval=TRUE, echo=TRUE--------------------------------------------------------------------------------------------------------------------------

# Filter the metadata to select the signals we want to retain

# First by quality
Phae.lon <- Phae.lon[Phae.lon$Quality == "A", ]
nrow(Phae.lon)

# Then by signal type
Phae.lon.song <- Phae.lon[grep("song", Phae.lon$Vocalization_type, ignore.case = TRUE), ]
nrow(Phae.lon.song)

# Finally by locality
Phae.lon.LS <- Phae.lon.song[grep("La Selva Biological Station, Sarapiqui, Heredia", Phae.lon.song$Locality, ignore.case = FALSE), ]

# Check resulting data frame, 6 recordings remain
str(Phae.lon.LS)


## ---- eval=TRUE, echo=TRUE--------------------------------------------------------------------------------------------------------------------------

# map in the RStudio graphics device (img = FALSE)
xcmaps(Phae.lon.LS, img = FALSE)


## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # Not working as of 01 Aug 2017
#  # This copies the selected sound files to a dropbox folder so they can be shared
#  # do not show this code
#  fn <- with(Phae.lon.LS, paste(paste(Genus, Specific_epithet, Recording_ID, sep = "-"), ".wav", sep = " "))
#  file.copy(from = file.path("/home/m/Documents/Biblioteca de cantos/Trochilidae/XC/wavs",fn), to = file.path("/home/m/Dropbox/Projects/warbleR package/vignette files", fn), overwrite = TRUE)
#  
#  wlist <- lapply(fn,function(x) downsample(readWave(file.path("/home/m/Dropbox/Projects/warbleR package/vignette files", x)), samp.rate = 22500))
#  
#  names(wlist) <- fn
#  
#  saveRDS(wlist, file = "/home/m/Dropbox/Sharing/warbleR/recs.RDS")
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Download sound files
#  querxc(X = Phae.lon.LS)
#  
#  # Save the metadata object as a .csv file
#  write.csv(Phae.lon.LS, "Phae_lon.LS.csv", row.names = FALSE)
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Always check you're in the right directory beforehand
#  # getwd()
#  
#  # here we are downsampling the original sampling rate of 44.1 kHz to speed up downstream analyses in the vignette series
#  mp32wav(samp.rate = 22.05)
#  
#  # Use checkwavs to see if wav files can be read
#  checkwavs()
#  

## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # Not working 01 Aug 2017
#  
#  ### If you were unable to convert _mp3_ to _wav_ format:
#    # + download the file in [this link](https://www.dropbox.com/s/htpbxbdw8s4i23k/recs.RDS?dl=0) and put it in your working directory
#    # + then run the following code:
#  
#  
#  # recs <- readRDS(file = "recs.RDS")
#  #
#  # for(i in 1:length(recs))
#  #   writeWave(recs[[i]], filename = names(recs)[i])
#  

## ---- echo=TRUE, eval=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------
#  
#  # Make sure you are in the right working directory
#  # Note that all the example sound files begin with the pattern "Phae.long"
#  wavs <- list.files(pattern = "wav$")
#  wavs
#  
#  rm <- wavs[grep("Phae.long", wavs)]
#  
#  file.remove(rm)
#  
#  # Check that the right wav files were removed
#  # Only xeno-cant wav files should remain
#  list.files(pattern = "wav$")
#  

## ---- echo=TRUE, eval=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------
#  
#  # For this example, set your working directory to an empty temporary directory
#  setwd(tempdir())
#  
#  # Here we will simulate the problem of having files scattered in multiple directories
#  
#  # Load .wav file examples from the NatureSounds package
#  data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
#  
#  # Create first folder inside the temporary directory and write new .wav files inside this new folder
#  dir.create("folder1")
#  writeWave(Phae.long1, file.path("folder1","Phae_long1.wav"))
#  writeWave(Phae.long2, file.path("folder1","Phae_long2.wav"))
#  
#  # Create second folder inside the temporary directory and write new .wav files inside this second new folder
#  dir.create("folder2")
#  writeWave(Phae.long3, file.path("folder2","Phae_long3.wav"))
#  
#  # Consolidate the scattered files into a single folder, and make a .csv file that contains metadata (location, old and new names in the case that files were renamed)
#  invisible(consolidate(path = tempdir(), save.csv = TRUE))
#  
#  list.files(path = "./consolidated_folder")
#  
#  # set your working directory back to "/home/user/warbleR_example" for the rest of the vignette, or to whatever working directory you were using originally
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Create a vector of all the recordings in the directory
#  wavs <- list.files(pattern = "wav$")
#  
#  # Print this object to see all sound files
#  # 6 sound files from xeno-canto
#  wavs
#  
#  # Select a subset of recordings to explore lspec() arguments
#  # Based on the list of wav files we created above
#  sub <- wavs[c(1, 5)]
#  
#  # How long are these files? this will determine number of pages returned by lspec
#  wavdur(sub)
#  
#  # ovlp = 10 to speed up function
#  # tiff image files are better quality and are faster to produce
#  lspec(flist = sub, ovlp = 10, it = "tiff")
#  
#  # We can zoom in on the frequency axis by changing flim,
#  # the number of seconds per row, and number of rows
#  lspec(flist = sub, flim = c(2, 10), sxrow = 6, rows = 15, ovlp = 10, it = "tiff")
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Make long spectrograms for the xeno-canto sound files
#  lspec(flim = c(2, 10), ovlp = 10, sxrow = 6, rows = 15, it = "jpeg", flist = fl)
#  
#  # Concatenate lspec image files into a single PDF per recording
#  # lspec images must be jpegs to do this
#  lspec2pdf(keep.img = FALSE, overwrite = TRUE)
#  

## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # make all page-size images 700 pxls width
#  

## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  ### Remove silence in sound files
#  
#  # The function below removes silent segments of sound files. This can help reduce file size, which can speed up functions (in particular, `manualoc`).
#  
#  # giving error: Error in file.copy(from = wv, to = file.path(getwd(), "removed_silence_files",  :
#    # more 'from' files than 'to' files
#  
#  # here we will produce spectrograms of the silent gaps that were removed
#  # perform this on only the longer xeno-canto recordings
#  rm_sil(flist = wavs, min.sil.dur = 0.2, img = TRUE, it = "jpeg", flim = c(0, 12))
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # Select a subset of sound files
#  # Reinitialize the wav object
#  wavs <- list.files(pattern = ".wav$", ignore.case = TRUE)
#  
#  # Set a seed so we all have the same results
#  set.seed(1)
#  sub <- wavs[sample(1:length(wavs), 3)]
#  
#  # Run autodetec() on subset of recordings
#  # The data frame object output is printed to the console, we are not saving this in an object yet, since we are just playing around with argument settings
#  # you can run this in parallel to speed up computation time
#  autodetec(flist = sub, bp = c(1, 10), threshold = 10, mindur = 0.05, maxdur = 0.5, envt="abs", ssmooth = 300, ls = TRUE, res = 100, flim = c(1, 12), wl = 300, set = TRUE, sxrow = 6, rows = 15, redo = FALSE)
#  

## ---- eval=FALSE, echo = TRUE-----------------------------------------------------------------------------------------------------------------------
#  
#  autodetec(flist = sub, bp = c(2, 10), threshold = 20, mindur = 0.09, maxdur = 0.22, envt = "abs", ssmooth = 900, ls = TRUE, res = 100, flim= c(1, 12), wl = 300, set =TRUE, sxrow = 6, rows = 15, redo = TRUE, it = "tiff", img = TRUE, smadj = "end")
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  Phae.ad <- autodetec(bp = c(2, 10), threshold = 20, mindur = 0.09, maxdur = 0.22, envt = "abs", ssmooth = 900, ls = TRUE, res = 100, flim = c(2, 10), wl = 300, set =TRUE, sxrow = 6, rows = 15, redo = TRUE, it = "tiff", img = TRUE, smadj = "end")
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  table(Phae.ad$sound.files)
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # A margin that's too large causes other signals to be included in the noise measurement
#  # Re-initialize X as needed, for either autodetec or manualoc output
#  
#  # Try this with 10% of the selections first
#  # Set a seed first, so we all have the same results
#  set.seed(5)
#  
#  X <- Phae.ad[sample(1:nrow(Phae.ad),(nrow(Phae.ad)*0.05)), ]
#  nrow(X)
#  
#  snrspecs(X = X, flim = c(2, 10), snrmar = 0.5, mar = 0.7, it = "jpeg")
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # This smaller margin is better
#  snrspecs(X = X, flim = c(2, 10), snrmar = 0.04, mar = 0.7, it = "jpeg")
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  Phae.snr <- sig2noise(X = Phae.ad[seq(1, nrow(Phae.ad), 2), ], mar = 0.04)
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  Phae.hisnr <- Phae.snr[ave(-Phae.snr$SNR, Phae.snr$sound.files, FUN = rank) <= 5, ]
#  
#  # save the selections as a physical file
#  write.csv(Phae.hisnr, "Phae_hisnr.csv", row.names = FALSE)
#  
#  # Double check the number of selection per sound files
#  # Only the xeno-canto sound files will have 5 selections, the other sound files started off with less than 5 selections
#  table(Phae.hisnr$sound.files)
#  

## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  Phae.hisnr <- read.csv("Phae_hisnr.csv", header = TRUE)
#  table(Phae.hisnr$sound.files)
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Run manualoc() with frequency range set for Phaethornis longirostris
#  # Recording comments are enabled to mark recording quality
#  # Selection comments enabled to include visual classifications
#  manualoc(flim = c(2, 10), reccomm = TRUE, selcomm = TRUE, osci = TRUE, seltime = 2)
#  
#  # Read manualoc() output back into R as an object
#  # This data frame object can be used as input for later functions
#  manualoc_out <- read.csv("manualoc_output.csv", header = TRUE)
#  

