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
#  # run this if you have restarted RStudio between vignettes without saving your workspace
#  # assumes that you are in your /home/username directory
#  setwd(file.path(getwd(),"warbleR_example"))
#  
#  # Check your location
#  getwd()
#  

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------------------------------------------------------------
#  
#  # The package must be loaded in your working environment
#  ls("package:warbleR")
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  tin <- querxc(qword = 'Tinamus', download = FALSE)
#  
#  # select a single recording
#  tin <- tin[tin$Recordist == "Marcelo Araya-Salas", ]
#  
#  # download this recording
#  querxc(X = tin, download = TRUE)
#  
#  mp32wav()
#  

## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # Hiding the text that goes with the chunk below
#  
#  # If you have _Raven_ installed on your local machine, you can use _Rraven_ to call this software and make selections. Make sure to include arguments from imp_raven to ensure that the selection table is imported with the correct columns for downstream functions. We will use the _Tinamus major_ signals for detecting frequency range below, so if you do not have _Raven_ installed on your machine, you can use the code below as a reference for your own signals.
#  

## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # commenting this out because this fails on my machine, although it worked when I first wrote this code...
#  
#  # here you will replace the raven.path argument with the path specifying where Raven is located on your own machine
#  Tin.sels <- run_raven(raven.path = "/home/gsvidaurre/opt/Raven-1.5.0.0035/", sound.files = "Tinamus-major-154191.wav", import = TRUE, all.data = FALSE, name.from.file = TRUE, ext.case = "lower", freq.cols = FALSE)
#  str(Tin.sels)
#  
#  # write the selection table as a physical file you you can read them back in at any time
#  # good way to save all your work
#  write.csv(Tin.sels, "Tinamus-major-154191_sels.csv", row.names = FALSE)
#  
#  # generate individual cuts for freqeuency range measurements below
#  cut_sels(Tin.sels, mar = 0.05, labels = c("sound.files", "selec"))
#  

## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # Tin.sels <- read.csv("Tinamus-major-154191_sels.csv", header = TRUE)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # here we will use manualoc to select signals within this recording
#  # display only part of the recording to make manualoc run faster
#  manualoc(flist = "Tinamus-major-154191.wav", flim = c(0, 4), tdisp = 60)
#  
#  # read the selections back into the global environment
#  Tin.sels <- read.csv("manualoc_output.csv")
#  str(Tin.sels)
#  
#  # cut the original wave file by selections for frange.detec below
#  writeWave(seewave::cutw(readWave("Tinamus-major-154191.wav"), from = Tin.sels$start[1], to = Tin.sels$end[1], f = 44100, plot = FALSE, output = "Wave"), filename = "Tinamus-major-154191-1.wav")
#  
#  writeWave(seewave::cutw(readWave("Tinamus-major-154191.wav"), from = Tin.sels$start[2], to = Tin.sels$end[2], f = 44100, plot = FALSE, output = "Wave"), filename = "Tinamus-major-154191-2.wav")
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # note that changing the threshold argument in combination with the bandpass argument can improve the detection
#  frange.detec(readWave("Tinamus-major-154191-1.wav"), flim = c(0, 2.5), bp = c(0, 3), threshold = 15, plot = TRUE)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # here, giving a strict bandpass with very low threshold improves frange detection
#  # since the curving end of the tinamou signal is lower amplitude than the rest of the signal
#  frange.detec(readWave("Tinamus-major-154191-1.wav"), flim = c(0, 2.5), bp = c(0, 3), threshold = 1, plot = TRUE)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # use arguments from frange.detec above
#  fr <- frange(Tin.sels, threshold = 1, res = 100, flim = c(0, 2.5), bp = c(0.5, 2.5))
#  str(fr)
#  

## ---- eval = FALSE, echo=FALSE----------------------------------------------------------------------------------------------------------------------
#  
#  Phae.hisnrt <- read.csv("Phae_hisnrt.csv", header = TRUE)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  Phae.hisnrt <- read.csv("Phae_hisnrt.csv", header = TRUE)
#  str(Phae.hisnrt)
#  
#  se <- sp.en.ts(Phae.hisnrt, wl = 300, length.out = 10, threshold = 10, img = TRUE, img.suffix = "sp.en.ts", type = "b", ovlp = 90, sp.en.range = c(-25, 10), flim = c(2, 10), picsize = 0.75, title = FALSE)
#  
#  str(se)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # Note that the dominant frequency measurements are almost always more accurate
#  trackfreqs(Phae.hisnrt, wl = 300, flim = c(2, 10), bp = c(1, 12), it = "jpeg")
#  
#  # We can change the lower end of bandpass to make the frequency measurements more precise
#  trackfreqs(Phae.hisnrt, wl = 300, flim = c(2, 10), bp = c(2, 12), col = c("purple", "orange"), pch = c(17, 3), res = 100, it = "jpeg", picsize = 0.8)
#  

## ---- echo=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # decided to remove track_harms, not working well for either Phaethornis or Tinamou signals
#  
#  # the text for above this chunk
#  # `track_harms` is a modified function from `seewave` that allows you to track the dominant frequency for harmonic calls, even when the amplitude fluctuates among harmonics.
#  
#  # with a Phaethornis harmonic signal
#  nm <- paste(paste(as.character(Phae.hisnrt$sound.files[1]), as.character(Phae.hisnrt$selec[1]), sep = "-"), ".wav", sep = "")
#  
#  writeWave(seewave::cutw(readWave(as.character(Phae.hisnrt$sound.files[1])), from = Phae.hisnrt$start[1], to = Phae.hisnrt$end[1], f = 44100, plot = FALSE, output = "Wave"), filename = nm)
#  
#  trck_hrm <- track_harm(readWave(nm), f = 44100, ovlp = 70, fftw = FALSE, threshold = 15, bandpass = NULL, clip = 0.1, plot = TRUE, xlab = "Time (s)", ylab = "Frequency (kHz)",  adjust.wl = FALSE, dfrq = FALSE)
#  
#  # plot spectrogram
#  spectro(readWave(nm), grid = FALSE, scale = FALSE, f = 22050, ovlp = 90, palette = reverse.gray.colors.2, collevels = seq(-40, 0, 1), wl = 300, osc = FALSE, flim = c(2, 10), main = "warbleR's 'track_harm'")
#  
#  # plot detected frequency contour
#  points(x = trck_hrm[ , 1] + 0.1, y =  trck_hrm[ , 2], cex = 1, col = "red", pch = 20)
#  

## ---- echo=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------
#  
#  # with a Tinamou tonal signal
#  trck_hrm <- track_harm(readWave("Tinamus-major-154191-1.wav"), f = 44100, ovlp = 70, fftw = FALSE, threshold = 15, bandpass = NULL, plot = TRUE, xlab = "Time (s)", ylab = "Frequency (kHz)",  adjust.wl = FALSE, dfrq = FALSE)
#  
#  # plot spectrogram
#  spectro(readWave("Tinamus-major-154191-2.wav"), grid = FALSE, scale = FALSE, f = 44100, ovlp = 90, palette = reverse.gray.colors.2, collevels = seq(-40, 0, 1), wl = 300, osc = FALSE, flim = c(0, 4), main = "warbleR's 'track_harm'")
#  
#  # plot detected frequency contour
#  points(x = trck_hrm[ , 1] + 0.1, y =  trck_hrm[ , 2], cex = 1, col = "red", pch = 20)
#  

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------------------------------------------------------------
#  
#  # Fundamental frequency contour
#  ff_df <- ffts(Phae.hisnrt, wl = 300, length.out = 20, threshold = 15, img = TRUE, img.suffix = "ff", type = "p", ovlp = 70, clip.edges = FALSE, leglab = "ffts", ff.method = "tuneR")
#  
#  str(ff_df)
#  

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------------------------------------------------------------
#  
#  # Dominant frequency contour
#  
#  # Uses seewave function dfreq by default
#  df_df <- ffts(Phae.hisnrt, wl = 300, length.out = 20, threshold = 15, img = TRUE, img.suffix = "ff", type = "p", ovlp = 70, clip.edges = FALSE, leglab = "ffts", fsmooth = 0.2)
#  
#  str(df_df)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # Use the original data frame of songs for the main seltailor dataset
#  # the data frame with the fundamental frequency contours is provided for manual tracing
#  seltailor(Phae.hisnrt, wl = 300, flim = c(2, 10), wn = "hanning", mar = 0.1,
#   osci = TRUE, title = c("sound.files", "selec"), auto.contour = TRUE, ts.df = ff_df, col = "red", alpha = 0.6)
#  
#  # rename your seltailor output csv as desired, then read it back into R
#  mff <- read.csv("seltailor_output_mff.csv")
#  str(mff)
#  
#  trackfreqs(Phae.hisnrt, wl = 300, flim = c(2, 10), bp = c(1, 12), it = "jpeg", custom.contour = mff)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  df_inf <- inflections(X = df_df, pb = TRUE)
#  str(df_inf)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  Phae.hisnrt <- read.csv("Phae_hisnrt.csv", header = TRUE)
#  
#  compare.methods(X = Phae.hisnrt, flim = c(0, 10), bp = c(0, 10),
#                  wl = 300, n = 10, methods = c("XCORR", "dfDTW"))
#  

## ---- eval=TRUE, echo=FALSE-------------------------------------------------------------------------------------------------------------------------

params <- read.csv("acoustic_parameters.csv")


## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  params <- specan(Phae.hisnrt, bp = c(2, 10), threshold = 15)
#  write.csv(params, "acoustic_parameters.csv", row.names = FALSE)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  params <- params[, grep("fun|peakf", colnames(params), invert = TRUE)]
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#  writeWave(Phae.long1,"Phae.long1.wav")
#  writeWave(Phae.long2,"Phae.long2.wav")
#  writeWave(Phae.long3,"Phae.long3.wav")
#  writeWave(Phae.long4,"Phae.long4.wav")
#  
#  # Add a 'song' column
#  selec.table$song <- rep(1:4, each = 3)[1:11]
#  
#  # Measure acoustic parameters
#  sp <- specan(selec.table, bp = c(1, 11), 300, fast = TRUE)
#  
#  # Add song data
#  sp <- merge(sp, selec.table, by = c("sound.files", "selec"))
#  
#  # Caculate song-level parameters for all numeric parameters
#  sng <- song_param(X = sp, song_colm = "song", parallel = 1, pb = TRUE)
#  str(sng)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # Harmonic Phaethornis signals
#  dm <- dfDTW(Phae.hisnrt, length.out = 30, flim = c(2, 10), bp = c(2, 9), wl = 300, img = TRUE)
#  
#  str(dm)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  # Tonal Tinamou signals
#  Tin.sels <- read.csv("Tinamus-major-154191_sels.csv", header = TRUE)
#  
#  dm <- dfDTW(Tin.sels, length.out = 30, flim = c(0, 2.5), bp = c(0.5, 2.5), wl = 512, img = TRUE)
#  str(dm)
#  

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------
#  
#  xc <- xcorr(Phae.hisnrt, wl = 300, na.rm = FALSE)
#  str(xc)
#  

## ---- eval=TRUE, dpi=220----------------------------------------------------------------------------------------------------------------------------

# Run the PCA with only numeric variables of params
pca <- prcomp(x = params[, sapply(params, is.numeric)], scale. = TRUE)

# Check loadings
summary(pca)

# Extract PCA scores
pcascor <- as.data.frame(pca[[5]])

# Plot the 2 first PCs
plot(pcascor[, 1], pcascor[, 2], col = as.numeric(params$sound.files), pch = 20, 
     cex = 1, xlab = "PC1", ylab = "PC2")

# Add recordings/individuals labels 
x <- tapply(pcascor[, 1], params$sound.files, mean)
y <- tapply(pcascor[, 2], params$sound.files, mean)

labs <- gsub(".wav", "", unique(sapply(as.character(params$sound.files), function(x){
  strsplit(x, split = "-", fixed = TRUE)[[1]][3]
  }, USE.NAMES = FALSE)))

text(x, y, labs, cex = 0.75)


## ---- eval=TRUE, dpi=220----------------------------------------------------------------------------------------------------------------------------

# Create a song type variable

# First, extract recording ID
songtype <- gsub(".wav", "", sapply(as.character(params$sound.files), function(x){
  strsplit(x, split = "-", fixed = TRUE)[[1]][3]
  }, USE.NAMES = FALSE))

# Now change IDs for letters representing song types
songtype <- gsub("154070|154072", "A", songtype)
songtype <- gsub("154129|154161", "B", songtype)
songtype <- gsub("154138", "C", songtype)

# Add song type as a variable representing symbol type
plot(pcascor[, 1], pcascor[, 2], col = as.numeric(params$sound.files), 
pch = as.numeric(as.factor(songtype)), 
     cex = 1, xlab = "PC1", ylab = "PC2")

# Add song type labels 
x <- tapply(pcascor[, 1], songtype, mean)
y <- tapply(pcascor[, 2], songtype, mean)

text(x, y, unique(songtype), cex = 1)


## ---- eval = FALSE, echo = TRUE---------------------------------------------------------------------------------------------------------------------
#  
#  data(sim.coor.sing)
#  str(sim.coor.sing)
#  

## ---- eval = FALSE, echo = TRUE---------------------------------------------------------------------------------------------------------------------
#  
#  # save plots in a list
#  g <- coor.graph(sim.coor.sing, it = "jpeg", img = FALSE, res = 300)
#  
#  # print list of plots to graphics device
#  g
#  

## ---- eval = FALSE, echo = TRUE---------------------------------------------------------------------------------------------------------------------
#  
#  cs <- coor.test(sim.coor.sing, iterations = 1000, less.than.chance = TRUE, cutoff = 10)
#  str(cs)
#  

## ---- eval = FALSE, echo = TRUE---------------------------------------------------------------------------------------------------------------------
#  
#  # simulate a song with 3 tonal elements
#  ss <- sim_songs(n = 3, harms = 1)
#  
#  # plot the simulated song
#  # seewave::spectro(ss)
#  
#  # simulate a song with 3 harmonic elements of differing amplitude
#  ss <- sim_songs(n = 3, harms = 3)
#  
#  # plot the simulated song
#  seewave::spectro(ss)
#  

