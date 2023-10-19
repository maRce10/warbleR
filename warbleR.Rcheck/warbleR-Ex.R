pkgname <- "warbleR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('warbleR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("auto_detec")
### * auto_detec

flush(stderr()); flush(stdout())

### Name: auto_detec
### Title: 'auto_detec' automatically detects the start and end of
###   vocalizations in sound files based on amplitude, duration, and
###   frequency range attributes.
### Aliases: auto_detec

### ** Examples

{
  # Save to temporary working directory
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  ad <- auto_detec(
    threshold = 5, ssmooth = 300,
    bp = c(2, 9), wl = 300, path = tempdir()
  )
}




cleanEx()
nameEx("by_element_est")
### * by_element_est

flush(stderr()); flush(stdout())

### Name: by_element_est
### Title: Convert a by-song extended selection table to by-element
### Aliases: by_element_est

### ** Examples

## Not run: 
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
##D writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
##D 
##D # create extended selection table
##D by_song_est <- selection_table(lbh_selec_table,
##D   path = tempdir(),
##D   extended = TRUE, by.song = "song", confirm.extended = FALSE
##D )
##D 
##D # conver o by element
##D by_element_est <- by_element_est(by_song_est, mar = 0.05)
## End(Not run)



cleanEx()
nameEx("catalog")
### * catalog

flush(stderr()); flush(stdout())

### Name: catalog
### Title: Create catalogs of vocal signals
### Aliases: catalog

### ** Examples

## Not run: 
##D # save sound file examples
##D data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) 
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
##D  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
##D 
##D 
##D catalog(X = lbh_selec_table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = T,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE,
##D  orientation = "v", labels = c("sound.files", "selec"), legend = 0, 
##D  path = tempdir())
##D  
##D  #different time scales and tag palette
##D catalog(X = lbh_selec_table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = F,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, 
##D  orientation = "v",  labels = c("sound.files", "selec"), legend = 0, 
##D  tag.pal = list(terrain.colors), 
##D  path = tempdir())
##D  
##D  #adding tags and changing spectro palette
##D catalog(X = lbh_selec_table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = F,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, pal = reverse.heat.colors,
##D  orientation = "v",  labels = c("sound.files", "selec"), legend = 1, 
##D  tag.pal = list(terrain.colors), tags = "sound.files", 
##D  path = tempdir())
##D 
##D  #create a bigger selection table
##D  X <- rbind(lbh_selec_table, lbh_selec_table, lbh_selec_table, lbh_selec_table)
##D  X <- rbind(X, X)
##D  
##D  #create some simulated labels
##D  X$songtype <- sample(letters[13:15], nrow(X), replace = T)
##D  X$indiv <- sample(letters[1:12], nrow(X), replace = T)
##D 
##D # 12 columns in 5 rows, 2 tags
##D catalog(X = X, flim = c(1, 10), nrow = 5, ncol = 12, same.time.scale = F,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, 
##D  orientation = "v",  labels = c("sound.files", "selec"), legend = 3, 
##D  collevels = seq(-65, 0, 5), tag.pal = list(terrain.colors), tags = c("songtype", "indiv"), 
##D  path = tempdir())
##D 
##D # with legend
##D catalog(X = X, flim = c(1, 10), nrow = 5, ncol = 12, same.time.scale = F,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE,
##D  orientation = "v",  labels = c("sound.files", "selec"), legend = 3, 
##D  width = 20, collevels = seq(-65, 0, 5), tag.pal = list(terrain.colors),
##D   tags = c("songtype", "indiv"), 
##D   path = tempdir())
##D   
##D   # horizontal orientation
##D catalog(X = X, flim = c(1, 10), nrow = 5, ncol = 12, same.time.scale = F,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE,
##D  orientation = "h",  labels = c("sound.files", "selec"), legend = 3, 
##D  width = 20, collevels = seq(-65, 0, 5), tag.pal = list(terrain.colors),
##D   tags = c("songtype", "indiv"), 
##D   path = tempdir())
##D 
##D check this floder
##D tempdir()
## End(Not run)



cleanEx()
nameEx("catalog2pdf")
### * catalog2pdf

flush(stderr()); flush(stdout())

### Name: catalog2pdf
### Title: Combine 'catalog' images into pdfs
### Aliases: catalog2pdf

### ** Examples

## Not run: 
##D # save sound file examples
##D data(list = c("Phae.long1", "Phae.long2"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D 
##D catalog(X = lbh_selec_table, nrow = 2, ncol = 4)
##D 
##D # now create single pdf removing jpeg
##D catalog2pdf(keep.img = FALSE, path = tempdir())
##D 
##D # check this floder
##D tempdir()
## End(Not run)



cleanEx()
nameEx("check_sels")
### * check_sels

flush(stderr()); flush(stdout())

### Name: check_sels
### Title: Check selection data frames
### Aliases: check_sels

### ** Examples

{
# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))

check_sels(X = lbh_selec_table, path = tempdir())
}



cleanEx()
nameEx("check_sound_files")
### * check_sound_files

flush(stderr()); flush(stdout())

### Name: check_sound_files
### Title: Check sound files
### Aliases: check_sound_files

### ** Examples

{
# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# without selection data frame
check_sound_files(path = tempdir())

# with selection data frame
check_sound_files(X = lbh_selec_table, path = tempdir())
}



cleanEx()
nameEx("color_spectro")
### * color_spectro

flush(stderr()); flush(stdout())

### Name: color_spectro
### Title: Highlight spectrogram regions
### Aliases: color_spectro

### ** Examples

## Not run: 
##D data(list = c("Phae.long1", "lbh_selec_table"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
##D 
##D # subset selection table
##D st <- lbh_selec_table[lbh_selec_table$sound.files == "Phae.long1.wav", ]
##D 
##D # read wave file as an R object
##D sgnl <- tuneR::readWave(file.path(tempdir(), st$sound.files[1]))
##D 
##D # create color column
##D st$colors <- c("red2", "blue", "green")
##D 
##D # highlight selections
##D color_spectro(
##D   wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-40, 0, 5),
##D   dB = "B", X = st, col.clm = "colors", base.col = "skyblue", t.mar = 0.07, f.mar = 0.1,
##D   interactive = NULL
##D )
##D 
##D # interactive (selected manually: you have to select them by clicking on the spectrogram)
##D color_spectro(
##D   wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-40, 0, 5),
##D   dB = "B", col.clm = "colors", t.mar = 0.07, f.mar = 1, interactive = 2
##D )
## End(Not run)




cleanEx()
nameEx("compare_methods")
### * compare_methods

flush(stderr()); flush(stdout())

### Name: compare_methods
### Title: Assessing the performance of acoustic distance measurements
### Aliases: compare_methods

### ** Examples

## Not run: 
##D # Save to temporary working directory
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
##D writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
##D 
##D compare_methods(
##D   X = lbh_selec_table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
##D   ovlp = 90, res = 200, n = 10, length.out = 30,
##D   methods = c("XCORR", "dfDTW"), parallel = 1, it = "jpeg", path = tempdir()
##D )
##D 
##D # remove progress bar
##D compare_methods(
##D   X = lbh_selec_table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
##D   ovlp = 90, res = 200, n = 10, length.out = 30,
##D   methods = c("XCORR", "dfDTW"), parallel = 1, it = "jpeg", pb = FALSE, path = tempdir()
##D )
##D 
##D # check this folder!
##D getwd()
##D 
##D 
##D # compare SP and XCORR
##D compare_methods(
##D   X = lbh_selec_table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
##D   ovlp = 90, res = 200, n = 10, length.out = 30,
##D   methods = c("XCORR", "SP"), parallel = 1, it = "jpeg", path = tempdir()
##D )
##D 
##D # compare SP method against dfDTW
##D compare_methods(
##D   X = lbh_selec_table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
##D   ovlp = 90, res = 200, n = 10, length.out = 30,
##D   methods = c("dfDTW", "SP"), parallel = 1, it = "jpeg",
##D   path = tempdir()
##D )
##D 
##D # alternatively we can provide our own SP matrix
##D Y <- spectro_analysis(lbh_selec_table, path = tempdir())
##D 
##D # selec a subset of variables
##D Y <- Y[, 1:7]
##D 
##D # PCA
##D Y <- prcomp(Y[, 3:ncol(Y)])$x
##D 
##D # add sound files and selec columns
##D Y <- data.frame(lbh_selec_table[, c(1, 3)], Y[, 1:2])
##D 
##D compare_methods(
##D   X = lbh_selec_table, methods = c("dfDTW"), custom1 = Y,
##D   path = tempdir()
##D )
## End(Not run)




cleanEx()
nameEx("consolidate")
### * consolidate

flush(stderr()); flush(stdout())

### Name: consolidate
### Title: Consolidate (sound) files into a single directory
### Aliases: consolidate

### ** Examples

{
# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

# create first folder with 2 sound files
dir.create(file.path(tempdir(), "folder1"))
writeWave(Phae.long1, file.path(tempdir(), "folder1", "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "folder1", "Phae.long2.wav"))

# create second folder with 2 sound files
dir.create(file.path(tempdir(), "folder2"))
writeWave(Phae.long3, file.path(tempdir(), "folder2", "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "folder2", "Phae.long4.wav"))

# consolidate in a single folder
# consolidate(path = tempdir(), dest.path = tempdir())

# check this folder
tempdir()
}




cleanEx()
nameEx("cross_correlation")
### * cross_correlation

flush(stderr()); flush(stdout())

### Name: cross_correlation
### Title: Time-frequency cross-correlation
### Aliases: cross_correlation

### ** Examples

{
  # load data
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

  # save sound files
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  # run cross correlation on spectrograms (SPCC)
  xcor <- cross_correlation(X = lbh_selec_table, wl = 300, ovlp = 90, path = tempdir())

  # run cross correlation on Mel cepstral coefficients (mfccs)
  xcor <- cross_correlation(
    X = lbh_selec_table, wl = 300, ovlp = 90, path = tempdir(),
    type = "mfcc"
  )

  # using the 'compare.matrix' argument to specify pairwise comparisons
  # create matrix with ID of signals to compare
  cmp.mt <- cbind(
    paste(lbh_selec_table$sound.files[1:10], lbh_selec_table$selec[1:10], sep = "-"),
    paste(lbh_selec_table$sound.files[2:11], lbh_selec_table$selec[2:11], sep = "-")
  )

  # run cross-correlation on the selected pairwise comparisongs
  xcor <- cross_correlation(
    X = lbh_selec_table, compare.matrix = cmp.mt,
    wl = 300, ovlp = 90, path = tempdir()
  )
}



cleanEx()
nameEx("cut_sels")
### * cut_sels

flush(stderr()); flush(stdout())

### Name: cut_sels
### Title: Cut selections into individual sound files
### Aliases: cut_sels
### Keywords: internal

### ** Examples

{
# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# cut selections
cut_sels(lbh_selec_table, path = tempdir())

#check this folder!!
tempdir()
}




cleanEx()
nameEx("duration_sound_files")
### * duration_sound_files

flush(stderr()); flush(stdout())

### Name: duration_sound_files
### Title: Measure the duration of sound files
### Aliases: duration_sound_files

### ** Examples

{
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))

  duration_sound_files(path = tempdir())
}




cleanEx()
nameEx("envelope")
### * envelope

flush(stderr()); flush(stdout())

### Name: envelope
### Title: Calculates the absolute amplitude envelope
### Aliases: envelope

### ** Examples

{
data(tico)

amp_env <- envelope(tico@left, ssmooth = 100)
}



cleanEx()
nameEx("filter_sels")
### * filter_sels

flush(stderr()); flush(stdout())

### Name: filter_sels
### Title: Subset selection data frames based on manually filtered image
###   files
### Aliases: filter_sels

### ** Examples

## Not run: 
##D # save wav file examples
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
##D 
##D spectrograms(lbh_selec_table,
##D   flim = c(0, 11), inner.mar = c(4, 4.5, 2, 1), outer.mar = c(4, 2, 2, 1),
##D   picsize = 2, res = 300, cexlab = 2, mar = 0.05, wl = 300, path = tempdir()
##D )
##D 
##D # go to the working directory (tempdir()) and delete some images
##D 
##D # filter selection data frame
##D fmloc <- filter_sels(X = lbh_selec_table, path = tempdir())
##D 
##D # this data frame does not have the selections corresponding to the images that were deleted
##D fmloc
##D 
##D # now using lspec images
##D full_spectrograms(
##D   sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300, ovlp = 10,
##D   path = tempdir()
##D )
##D 
##D # go to the working directory (tempdir()) and delete lspec
##D # images (the ones with several rows of spectrograms)
##D 
##D # filter selection data frame
##D fmloc2 <- filter_sels(
##D   X = lbh_selec_table, lspec = TRUE,
##D   path = tempdir()
##D )
## End(Not run)




cleanEx()
nameEx("find_clipping")
### * find_clipping

flush(stderr()); flush(stdout())

### Name: find_clipping
### Title: Find clipped selections
### Aliases: find_clipping

### ** Examples

{
  # load data
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

  find_clipping(X = lbh_selec_table[1:5, ], path = tempdir())
}



cleanEx()
nameEx("find_peaks")
### * find_peaks

flush(stderr()); flush(stdout())

### Name: find_peaks
### Title: Find cross-correlation peaks
### Aliases: find_peaks

### ** Examples

{
  # load data
  data(list = c("Phae.long4", "Phae.long2", "lbh_selec_table2", "comp_matrix"))

  # save sound files
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

  # run cross-correlation
  xc.output <- cross_correlation(
    X = lbh_selec_table2, output = "list",
    compare.matrix = comp_matrix, path = tempdir()
  )

  # find peaks
  pks <- find_peaks(xc.output = xc.output, path = tempdir())
}



cleanEx()
nameEx("fix_extended_selection_table")
### * fix_extended_selection_table

flush(stderr()); flush(stdout())

### Name: fix_extended_selection_table
### Title: Fix extended selection tables
### Aliases: fix_extended_selection_table

### ** Examples

{
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# create extended selection table
ext_st <- selection_table(lbh_selec_table, extended = TRUE,
path = tempdir())

# remove attributes
st <- as.data.frame(ext_st)

# check class
class(st)

# fix selection table
st <- fix_extended_selection_table(X = st, Y = ext_st)

# check class
class(st)
}



cleanEx()
nameEx("fix_wavs")
### * fix_wavs

flush(stderr()); flush(stdout())

### Name: fix_wavs
### Title: Fix .wav files to allow importing them into R
### Aliases: fix_wavs

### ** Examples

## Not run: 
##D # Load example files and save to temporary working directory
##D #
##D 
##D # check this folder
##D tempdir()
## End(Not run)




cleanEx()
nameEx("freq_DTW")
### * freq_DTW

flush(stderr()); flush(stdout())

### Name: freq_DTW
### Title: Acoustic dissimilarity using dynamic time warping on dominant
###   frequency contours
### Aliases: freq_DTW

### ** Examples

{
  # load data
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

  # dominant frequency
  freq_DTW(lbh_selec_table,
    length.out = 30, flim = c(1, 12), bp = c(2, 9),
    wl = 300, path = tempdir()
  )

  # fundamental frequency
  freq_DTW(lbh_selec_table,
    type = "fundamental", length.out = 30, flim = c(1, 12),
    bp = c(2, 9), wl = 300, path = tempdir()
  )
}




cleanEx()
nameEx("freq_range")
### * freq_range

flush(stderr()); flush(stdout())

### Name: freq_range
### Title: Detect frequency range iteratively
### Aliases: freq_range

### ** Examples

{
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  freq_range(
    X = lbh_selec_table, wl = 112, fsmooth = 1, threshold = 13, widths = c(4, 1),
    img = TRUE, pb = TRUE, it = "tiff", line = TRUE, mar = 0.1, bp = c(1, 10.5),
    flim = c(0, 11), path = tempdir()
  )
}




cleanEx()
nameEx("freq_range_detec")
### * freq_range_detec

flush(stderr()); flush(stdout())

### Name: freq_range_detec
### Title: Detect frequency range on wave objects
### Aliases: freq_range_detec

### ** Examples

{
  data(tico)
  freq_range_detec(
    wave = tico, wl = 512, fsmooth = 0.01, threshold = 1, bp = c(2, 8),
    widths = c(4, 2)
  )

  data(sheep)
  freq_range_detec(
    wave = sheep, wl = 512, fsmooth = 0.2, threshold = 50, bp = c(0.3, 1),
    flim = c(0, 1.5), pal = reverse.heat.colors, main = "sheep"
  )
}




cleanEx()
nameEx("freq_ts")
### * freq_ts

flush(stderr()); flush(stdout())

### Name: freq_ts
### Title: Extract frequency contours as time series
### Aliases: freq_ts

### ** Examples

{
#load data
data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) #save sound files
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) #save sound files

# run function with dominant frequency
freq_ts(X = lbh_selec_table, length.out = 30, flim = c(1, 12), bp = c(2, 9),
wl = 300, pb = FALSE, path = tempdir())

# note a NA in the row 4 column 3 (dfreq-1)
# this can be removed by clipping edges (removing NAs at the start and/or end
# when no freq was detected)

freq_ts(X = lbh_selec_table, length.out = 30, flim = c(1, 12), bp = c(2, 9),
wl = 300, pb = FALSE, clip.edges = TRUE, path = tempdir())

# run function with fundamental frequency
freq_ts(lbh_selec_table, type = "fundamental", length.out = 50,
flim = c(1, 12),  bp = c(2, 9), wl = 300, path = tempdir())

# run function with spectral entropy
# without clip edges
freq_ts(X = lbh_selec_table, type = "entropy", threshold = 10,
clip.edges = FALSE, length.out = 10, sp.en.range = c(-25, 10), path = tempdir(),
 img = FALSE)

# with clip edges and length.out 10
freq_ts(X = lbh_selec_table, type = "entropy", threshold = 10, bp = c(2, 12),
clip.edges = TRUE, length.out = 10, path = tempdir(), img = FALSE)
}



cleanEx()
nameEx("full_spectrogram2pdf")
### * full_spectrogram2pdf

flush(stderr()); flush(stdout())

### Name: full_spectrogram2pdf
### Title: 'full_spectrogram2pdf' combines 'full_spectrograms' images in
###   .jpeg format to a single pdf file.
### Aliases: full_spectrogram2pdf

### ** Examples

## Not run: 
##D # save sound file examples
##D data(list = c("Phae.long1", "Phae.long2"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D 
##D full_spectrograms(
##D   sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300,
##D   it = "jpeg", path = tempdir()
##D )
##D 
##D # now create single pdf removing jpeg
##D full_spectrogram2pdf(keep.img = FALSE, path = tempdir())
##D 
##D # check this floder
##D tempdir()
## End(Not run)




cleanEx()
nameEx("full_spectrograms")
### * full_spectrograms

flush(stderr()); flush(stdout())

### Name: full_spectrograms
### Title: Create long spectrograms of entire sound files
### Aliases: full_spectrograms

### ** Examples

## Not run: 
##D # save sound file examples to temporary working directory
##D data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D 
##D full_spectrograms(
##D   sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300,
##D   path = tempdir()
##D )
##D 
##D # including selections
##D full_spectrograms(
##D   sxrow = 2, rows = 8, X = lbh_selec_table,
##D   pal = reverse.heat.colors, overwrite = TRUE, wl = 300, path = tempdir()
##D )
##D 
##D # check this floder
##D # tempdir()
## End(Not run)




cleanEx()
nameEx("gaps")
### * gaps

flush(stderr()); flush(stdout())

### Name: gaps
### Title: Gap duration
### Aliases: gaps

### ** Examples

{
# get warbleR sound file examples
data(list = "lbh_selec_table")

# get gaps
gaps(X = lbh_selec_table)
}




cleanEx()
nameEx("image_to_wave")
### * image_to_wave

flush(stderr()); flush(stdout())

### Name: image_to_wave
### Title: Convert images into wave objects
### Aliases: image_to_wave

### ** Examples





cleanEx()
nameEx("inflections")
### * inflections

flush(stderr()); flush(stdout())

### Name: inflections
### Title: Count number of inflections in a frequency contour
### Aliases: inflections

### ** Examples

{
# get warbleR sound file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# measure frequency contours
dom.freq.ts <- freq_ts(X = lbh_selec_table, path = tempdir())

# get number of inflections
inflections(X = dom.freq.ts)
}




cleanEx()
nameEx("info_sound_files")
### * info_sound_files

flush(stderr()); flush(stdout())

### Name: info_sound_files
### Title: Get sound file parameter information
### Aliases: info_sound_files

### ** Examples

{
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

#get info
info_sound_files(path = tempdir())
}




cleanEx()
nameEx("is_extended_selection_table")
### * is_extended_selection_table

flush(stderr()); flush(stdout())

### Name: is_extended_selection_table
### Title: Class 'extended_selection_table': selection table containing
###   wave objects
### Aliases: is_extended_selection_table

### ** Examples

{
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

  is_extended_selection_table(lbh_selec_table)

  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  st <- selection_table(lbh_selec_table,
    extended = TRUE,
    path = tempdir()
  )

  is_extended_selection_table(st)

  class(st)
}



cleanEx()
nameEx("is_selection_table")
### * is_selection_table

flush(stderr()); flush(stdout())

### Name: is_selection_table
### Title: Class 'selection_table': double-checked frequency/time
###   coordinates of selections
### Aliases: is_selection_table

### ** Examples

{
  # load data
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

  is_selection_table(lbh_selec_table)

  # save wave files in temporary directory
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  st <- selection_table(lbh_selec_table, path = tempdir())

  is_selection_table(st)

  class(st)
}



cleanEx()
nameEx("map_xc")
### * map_xc

flush(stderr()); flush(stdout())

### Name: map_xc
### Title: Maps of 'Xeno-Canto' recordings by species
### Aliases: map_xc

### ** Examples

## Not run: 
##D # search in xeno-canto
##D X <- query_xc("Phaethornis anthophilus", download = FALSE)
##D 
##D # create image in R graphic device
##D map_xc(X, img = FALSE)
##D 
##D # create leaflet map
##D map_xc(X, leaflet.map = TRUE)
## End(Not run)




cleanEx()
nameEx("mfcc_stats")
### * mfcc_stats

flush(stderr()); flush(stdout())

### Name: mfcc_stats
### Title: Calculate descriptive statistics on Mel-frequency cepstral
###   coefficients
### Aliases: mfcc_stats

### ** Examples

{
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# run function
mel_st <- mfcc_stats(X = lbh_selec_table, pb = FALSE, path = tempdir())

head(mel_st)

# measure 12 coefficients
mel_st12 <- mfcc_stats(X = lbh_selec_table, numcep = 12, pb = FALSE, path = tempdir())

 head(mel_st)
}



cleanEx()
nameEx("move_images")
### * move_images

flush(stderr()); flush(stdout())

### Name: move_images
### Title: Move/copy image files between directories
### Aliases: move_images

### ** Examples

{
  # load data
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

  # create spectrograms
  spectrograms(lbh_selec_table[1:5, ], path = tempdir(), pb = FALSE)

  # create folder to move image files
  dir.create(file.path(tempdir(), "imgs"))

  # copy files
  move_images(cut = FALSE, from = tempdir(), to = file.path(tempdir(), "imgs"))

  # cut files
  move_images(
    cut = TRUE, from = tempdir(),
    to = file.path(tempdir(), "imgs"), overwrite = TRUE
  )

  # Check this folder
  file.path(tempdir(), "imgs")
}




cleanEx()
nameEx("mp32wav")
### * mp32wav

flush(stderr()); flush(stdout())

### Name: mp32wav
### Title: Convert .mp3 files to .wav
### Aliases: mp32wav

### ** Examples

## Not run: 
##D # download mp3 files from xeno-canto
##D query_xc(qword = "Phaethornis aethopygus", download = TRUE, path = tempdir())
##D 
##D # Convert all files to .wav format
##D mp32wav(path = tempdir(), dest.path = tempdir())
##D 
##D # check this folder!!
##D tempdir()
## End(Not run)




cleanEx()
nameEx("multi_DTW")
### * multi_DTW

flush(stderr()); flush(stdout())

### Name: multi_DTW
### Title: A wrapper on 'dtwDist' for comparing multivariate contours
### Aliases: multi_DTW

### ** Examples

## Not run: 
##D # load data
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
##D 
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
##D writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
##D 
##D # measure
##D df <- freq_ts(X = lbh_selec_table, threshold = 10, img = FALSE, path = tempdir())
##D se <- freq_ts(X = lbh_selec_table, threshold = 10, img = FALSE, path = tempdir(), type = "entropy")
##D 
##D # run function
##D multi_DTW(df, se)
## End(Not run)




cleanEx()
nameEx("open_wd")
### * open_wd

flush(stderr()); flush(stdout())

### Name: open_wd
### Title: Open working directory
### Aliases: open_wd

### ** Examples

{
}




cleanEx()
nameEx("overlapping_sels")
### * overlapping_sels

flush(stderr()); flush(stdout())

### Name: overlapping_sels
### Title: Find overlapping selections
### Aliases: overlapping_sels

### ** Examples

{
  # no overlap
  overlapping_sels(X = lbh_selec_table)

  # modified lbh_selec_table to make the first and second selection overlap
  Y <- lbh_selec_table
  Y$end[4] <- 1.5

  overlapping_sels(X = Y)

  # drop overlapping
  overlapping_sels(X = Y, drop = TRUE)

  # get index instead
  overlapping_sels(X = Y, index = TRUE)
}



cleanEx()
nameEx("phylo_spectro")
### * phylo_spectro

flush(stderr()); flush(stdout())

### Name: phylo_spectro
### Title: Add spectrograms onto phylogenetic trees
### Aliases: phylo_spectro

### ** Examples

{ 
}



cleanEx()
nameEx("plot_coordination")
### * plot_coordination

flush(stderr()); flush(stdout())

### Name: plot_coordination
### Title: Coordinated singing graphs
### Aliases: plot_coordination

### ** Examples

## Not run: 
##D # load simulate singing events (see data documentation)
##D data(sim_coor_sing)
##D 
##D #' # make plot_coordination in graphic device format
##D cgs <- plot_coordination(X = sim_coor_sing, ovlp = TRUE, only.coor = FALSE, img = FALSE)
##D 
##D cgs
## End(Not run)




cleanEx()
nameEx("query_xc")
### * query_xc

flush(stderr()); flush(stdout())

### Name: query_xc
### Title: Access 'Xeno-Canto' recordings and metadata
### Aliases: query_xc

### ** Examples

## Not run: 
##D # search without downloading
##D df1 <- query_xc(qword = "Phaethornis anthophilus", download = FALSE)
##D View(df1)
##D 
##D # downloading files
##D query_xc(qword = "Phaethornis anthophilus", download = TRUE, path = tempdir())
##D 
##D # check this folder
##D tempdir()
##D 
##D ## search using xeno-canto advance query ###
##D orth.pap <- query_xc(qword = "gen:orthonyx cnt:papua loc:tari", download = FALSE)
##D 
##D # download file using the output data frame as input
##D query_xc(X = orth.pap, path = tempdir())
##D 
##D # use quotes for queries with more than 1 word (e.g. Costa Rica),note that the
##D # single quotes are used for the whole 'qword' and double quotes for the 2-word term inside
##D # Phaeochroa genus in Costa Rica
##D phae.cr <- query_xc(qword = 'gen:phaeochroa cnt:"costa rica"', download = FALSE)
##D 
##D # several terms can be searched for in the same field
##D # search for all female songs in sound type
##D femsong <- query_xc(qword = "type:song type:female", download = FALSE)
## End(Not run)




cleanEx()
nameEx("read_sound_file")
### * read_sound_file

flush(stderr()); flush(stdout())

### Name: read_sound_file
### Title: An extended version of read_wave that reads several sound file
###   formats and files from selection tables
### Aliases: read_sound_file

### ** Examples

## Not run: 
##D # write wave files with lower case file extension
##D data(list = c("Phae.long1"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
##D 
##D # read from selection table
##D read_sound_file(X = lbh_selec_table, index = 1, path = tempdir())
##D 
##D # from extended selection table
##D library(NatureSounds)
##D read_sound_file(X = lbh.est, index = 1)
##D 
##D # read from selection table
##D read_sound_file(X = lbh_selec_table, index = 1, path = tempdir())
##D 
##D # read WAV
##D filepath <- system.file("extdata", "recording.wav", package = "bioacoustics")
##D read_sound_file(filepath)
##D 
##D # read MP3
##D filepath <- system.file("extdata", "recording.mp3", package = "bioacoustics")
##D read_sound_file(filepath)
##D 
##D # read WAC
##D filepath <- system.file("extdata", "recording_20170716_230503.wac", package = "bioacoustics")
##D read_sound_file(filepath, from = 0, to = 0.2)
##D 
##D # URL file
##D read_sound_file(X = "https://www.xeno-canto.org/513948/download")
## End(Not run)




cleanEx()
nameEx("read_wave")
### * read_wave

flush(stderr()); flush(stdout())

### Name: read_wave
### Title: A wrapper for tuneR's readWave that read sound files listed
###   within selection tables
### Aliases: read_wave

### ** Examples

{
  # write wave files with lower case file extension
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

  # read from selection table
  read_wave(X = lbh_selec_table, index = 1, path = tempdir())

  # from extended selection table
  library(NatureSounds)
  read_wave(X = lbh.est, index = 1)

  # read WAV
  filepath <- system.file("extdata", "recording.wav", package = "bioacoustics")
  read_wave(filepath)

  # read MP3
  filepath <- system.file("extdata", "recording.mp3", package = "bioacoustics")
  read_wave(filepath)

  # URL file
  read_wave(X = "https://www.xeno-canto.org/513948/download")
}




cleanEx()
nameEx("remove_channels")
### * remove_channels

flush(stderr()); flush(stdout())

### Name: remove_channels
### Title: Remove channels in wave files
### Aliases: remove_channels

### ** Examples

{
# save sound file examples
data("Phae.long1")
Phae.long1.2 <- stereo(Phae.long1, Phae.long1)

writeWave(Phae.long1.2, file.path(tempdir(), "Phae.long1.2.wav"))

remove_channels(channels = 1, path = tempdir())

#check this floder
tempdir()
}




cleanEx()
nameEx("remove_silence")
### * remove_silence

flush(stderr()); flush(stdout())

### Name: remove_silence
### Title: Remove silence in wave files
### Aliases: remove_silence

### ** Examples

{
# save sound file examples
data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
sil <- silence(samp.rate = 22500, duration = 3, xunit = "time")


wv1 <- pastew(pastew(Phae.long1, sil, f = 22500, output = "Wave"),
Phae.long2, f = 22500, output = "Wave")

#check silence in between amplitude peaks
env(wv1)

 #save wave file
 writeWave(object = wv1, filename = file.path(tempdir(), "wv1.wav"),
  extensible = FALSE)

#remove silence
# remove_silence(files = "wv1.wav", pb = FALSE, path = tempdir())

#check this floder
tempdir()
}




cleanEx()
nameEx("rename_est_waves")
### * rename_est_waves

flush(stderr()); flush(stdout())

### Name: rename_est_waves
### Title: Rename wave objects and associated metadata in extended
###   selection tables
### Aliases: rename_est_waves

### ** Examples

{
data("lbh.est")

# order by sound file name
lbh.est <- lbh.est[order(lbh.est$sound.files),]

# create new sound file name
nsf <- sapply(strsplit(lbh.est$sound.files, ".wav",fixed = TRUE), "[",1)

slc <- vector(length = nrow(lbh.est))
slc[1] <- 1

for(i in 2:length(slc))
if (nsf[i - 1] == nsf[i]) slc[i] <- slc[i - 1] + 1 else
slc[i] <- 1

nsf <- paste(nsf, slc, sep = "_")

# rename sound files
Y <- rename_est_waves(X = lbh.est, new.sound.files = nsf)
}




cleanEx()
nameEx("resample_est")
### * resample_est

flush(stderr()); flush(stdout())

### Name: resample_est
### Title: Resample wave objects in a extended selection table
### Aliases: resample_est

### ** Examples

## Not run: 
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec_table"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
##D writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
##D 
##D # create extended selection table
##D X <- selection_table(
##D   X = lbh_selec_table, extended = TRUE, confirm.extended = FALSE, pb = FALSE,
##D   path = tempdir()
##D )
##D 
##D # resample
##D Y <- resample_est(X)
## End(Not run)



cleanEx()
nameEx("selection_table")
### * selection_table

flush(stderr()); flush(stdout())

### Name: selection_table
### Title: Create 'selection_table' and 'extended_selection_table' objects
### Aliases: selection_table

### ** Examples

{
  data(list = c(
    "Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4",
    "lbh_selec_table"
  ))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  # make selection table
  st <- selection_table(X = lbh_selec_table, path = tempdir())

  is_selection_table(st)

  #' # make extended selection table
  st <- selection_table(
    X = lbh_selec_table, extended = TRUE,
    path = tempdir()
  )

  is_extended_selection_table(st)

  ### make extended selection by song
  # create a song variable
  lbh_selec_table$song <- as.numeric(as.factor(lbh_selec_table$sound.files))

  st <- selection_table(
    X = lbh_selec_table, extended = TRUE,
    by.song = "song", path = tempdir()
  )
}




cleanEx()
nameEx("sig2noise")
### * sig2noise

flush(stderr()); flush(stdout())

### Name: sig2noise
### Title: Measure signal-to-noise ratio
### Aliases: sig2noise

### ** Examples

{
  data(list = c("Phae.long1", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files

  # specifying the correct margin is important
  # use snr_spectrograms to troubleshoot margins for sound files
  sig2noise(lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ],
    mar = 0.2,
    path = tempdir()
  )

  # this smaller margin doesn't overlap neighboring signals
  sig2noise(lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ],
    mar = 0.1,
    path = tempdir()
  )
}




cleanEx()
nameEx("simulate_songs")
### * simulate_songs

flush(stderr()); flush(stdout())

### Name: simulate_songs
### Title: Simulate animal vocalizations
### Aliases: simulate_songs

### ** Examples

## Not run: 
##D # simulate a song with 3 elements and no harmonics
##D sm_sng <- simulate_songs(n = 3, harms = 1)
##D 
##D # plot spectro
##D seewave::spectro(sm_sng)
##D 
##D # simulate a song with 5 elements and 2 extra harmonics
##D sm_sng2 <- simulate_songs(n = 5, harms = 3)
##D 
##D # plot spectrogram
##D seewave::spectro(sm_sng2)
##D 
##D # six pure tones with frequency ranging form 4 to 6 and returning selection table
##D sm_sng <- simulate_songs(
##D   n = 6, harms = 1, seed = 1, diff.fun = "pure.tone",
##D   freqs = seq(4, 6, length.out = 6), selec.table = TRUE,
##D   path = tempdir()
##D )
##D 
##D # plot spectro
##D seewave::spectro(sm_sng$wave, flim = c(2, 8))
##D 
##D # selection table
##D sm_sng$selec.table
## End(Not run)




cleanEx()
nameEx("snr_spectrograms")
### * snr_spectrograms

flush(stderr()); flush(stdout())

### Name: snr_spectrograms
### Title: Spectrograms with background noise margins
### Aliases: snr_spectrograms

### ** Examples

## Not run: 
##D data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound.files
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D 
##D # make Phae.long1 and Phae.long2 spectrograms
##D # snrmar needs to be smaller before moving on to sig2noise()
##D 
##D snr_spectrograms(lbh_selec_table,
##D   flim = c(0, 14), inner.mar = c(4, 4.5, 2, 1),
##D   outer.mar = c(4, 2, 2, 1), picsize = 2, res = 300, cexlab = 2, mar = 0.2,
##D   snrmar = 0.1, it = "jpeg", wl = 300, path = tempdir()
##D )
##D 
##D # make only Phae.long1 spectrograms
##D # snrmar now doesn't overlap neighboring signals
##D 
##D snr_spectrograms(lbh_selec_table[grepl(c("Phae.long1"), lbh_selec_table$sound.files), ],
##D   flim = c(3, 14), inner.mar = c(4, 4.5, 2, 1), outer.mar = c(4, 2, 2, 1),
##D   picsize = 2, res = 300, cexlab = 2, mar = 0.2, snrmar = 0.01, wl = 300,
##D   path = tempdir()
##D )
##D 
##D # check this folder!
##D tempdir()
## End(Not run)



cleanEx()
nameEx("song_analysis")
### * song_analysis

flush(stderr()); flush(stdout())

### Name: song_analysis
### Title: Calculates acoustic parameters at the song level
### Aliases: song_analysis

### ** Examples

{
# get warbleR sound file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))

# add a 'song' column
lbh_selec_table$song <- c("song1", "song1", "song1", "song2",
  "song2", "song3", "song3", "song3", "song4", "song4", "song4")

# measure acoustic parameters
sp <- spectro_analysis(lbh_selec_table[1:8, ], bp = c(1, 11), 300, fast = TRUE, path = tempdir())

# add song data
sp <- merge(sp, lbh_selec_table[1:8, ], by = c("sound.files", "selec"))

# caculate song-level parameters for all numeric parameters
song_analysis(X = sp, song_colm = "song", parallel = 1, pb = TRUE)

# caculate song-level parameters selecting parameters with mean_colm
song_analysis(X = sp, song_colm = "song",mean_colm = c("dfrange", "duration"),
 parallel = 1, pb = TRUE)

# caculate song-level parameters for selecting parameters with mean_colm, max_colm
# and min_colm and weighted by duration
song_analysis(X = sp, weight = "duration", song_colm = "song",
mean_colm =  c("dfrange", "duration"), min_colm =  "mindom", max_colm = "maxdom",
  parallel = 1, pb = TRUE)

# with two weights
song_analysis(X = sp, weight = c("duration", "dfrange"), song_colm = "song",
mean_colm = c("kurt", "sp.ent"), parallel = 1, pb = TRUE)

# with two weights no progress bar
song_analysis(X = sp, weight = c("duration", "dfrange"), song_colm = "song",
mean_colm = c("kurt", "sp.ent"), parallel = 1, pb = FALSE)
}




cleanEx()
nameEx("sort_colms")
### * sort_colms

flush(stderr()); flush(stdout())

### Name: sort_colms
### Title: Sort columns in a more intuitive order
### Aliases: sort_colms

### ** Examples

library(warbleR)
data("selec.table")

# mess column order
selec.table <- selec.table[, sample(seq_len(ncol(selec.table)))]

# check names
names(selec.table)

selec.table <- sort_colms(X = selec.table)

# check names again
names(selec.table)




cleanEx()
nameEx("sound_pressure_level")
### * sound_pressure_level

flush(stderr()); flush(stdout())

### Name: sound_pressure_level
### Title: Measure relative sound pressure level
### Aliases: sound_pressure_level

### ** Examples

{
  data(list = c("Phae.long1", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files

  spl <- sound_pressure_level(
    X = lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ],
    parallel = 1, pb = TRUE, path = tempdir()
  )
}




cleanEx()
nameEx("spectro_analysis")
### * spectro_analysis

flush(stderr()); flush(stdout())

### Name: spectro_analysis
### Title: Measure acoustic parameters in batches of sound files
### Aliases: spectro_analysis

### ** Examples

{
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))

# measure acoustic parameters
sp_param <- spectro_analysis(X = lbh_selec_table[1:8,], pb = FALSE, path = tempdir())

# measuring peakf
sp_param <- spectro_analysis(X = lbh_selec_table[1:8,], pb = FALSE, fast = FALSE, path = tempdir())

}



cleanEx()
nameEx("spectrograms")
### * spectrograms

flush(stderr()); flush(stdout())

### Name: spectrograms
### Title: Spectrograms of selected signals
### Aliases: spectrograms

### ** Examples

{
  # load and save data
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

  # make spectrograms
  spectrograms(
    X = lbh_selec_table, flim = c(0, 11), res = 300, mar = 0.05,
    wl = 300, path = tempdir()
  )

  # check this folder
  tempdir()
}




cleanEx()
nameEx("split_sound_files")
### * split_sound_files

flush(stderr()); flush(stdout())

### Name: split_sound_files
### Title: Splits sound files
### Aliases: split_sound_files

### ** Examples

{
  # load data and save to temporary working directory
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))

  # split files in 1 s files
  split_sound_files(sgmt.dur = 1, path = tempdir())

  # Check this folder
  tempdir()
}




cleanEx()
nameEx("tailor_sels")
### * tailor_sels

flush(stderr()); flush(stdout())

### Name: tailor_sels
### Title: Interactive view of spectrograms to tailor selections
### Aliases: tailor_sels

### ** Examples

## Not run: 
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
##D writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
##D 
##D tailor_sels(X = lbh_selec_table, flim = c(1, 12), wl = 300, auto.next = TRUE, path = tempdir())
##D 
##D # Read output .csv file
##D seltailor.df <- read.csv(file.path(tempdir(), "seltailor_output.csv"))
##D seltailor.df
##D 
##D # check this directory for .csv file after stopping function
##D tempdir()
## End(Not run)



cleanEx()
nameEx("test_coordination")
### * test_coordination

flush(stderr()); flush(stdout())

### Name: test_coordination
### Title: Randomization test for singing coordination
### Aliases: test_coordination

### ** Examples

{
#load  simulated singing data (see data documentation)
data(sim_coor_sing)

# set global options (this can also be set within the function call)
warbleR_options(iterations = 100, pb = FALSE)

# testing if coordination happens less than expected by chance
test_coordination(sim_coor_sing)

# testing if coordination happens more than expected by chance
test_coordination(sim_coor_sing, less.than.chance = FALSE)

# using "duration" method and "keep.song.order" as randomization procedure
test_coordination(sim_coor_sing, ovlp.method =  "time.overlap",
randomization = "keep.song.order")
}




cleanEx()
nameEx("track_freq_contour")
### * track_freq_contour

flush(stderr()); flush(stdout())

### Name: track_freq_contour
### Title: Spectrograms with frequency measurements
### Aliases: track_freq_contour

### ** Examples

{
  # load data
  data("Cryp.soui")
  writeWave(Cryp.soui, file.path(tempdir(), "Cryp.soui.wav")) # save sound files

  # autodetec location of signals
  ad <- auto_detec(
    threshold = 6, bp = c(1, 3), mindur = 1.2, flim = c(0, 5),
    maxdur = 3, img = FALSE, ssmooth = 600, wl = 300, flist = "Cryp.soui.wav",
    path = tempdir()
  )

  # track dominant frequency graphs with freq range detection
  track_freq_contour(
    X = ad[!is.na(ad$start), ], flim = c(0, 5), ovlp = 90,
    it = "tiff", bp = c(1, 3), contour = "df", wl = 300, frange = TRUE,
    path = tempdir()
  )

  # using users frequency data (custom.contour argument)
  # first get contours using freq_ts
  df <- freq_ts(
    X = ad[!is.na(ad$start), ], flim = c(0, 5), ovlp = 90, img = FALSE,
    bp = c(1, 3), wl = 300, path = tempdir()
  )

  # now input the freq_ts output into track_freq_contour
  track_freq_contour(
    X = ad[!is.na(ad$start), ], custom.contour = df, flim = c(0, 5), ovlp = 90,
    it = "tiff", path = tempdir()
  )

  # Check this folder
  tempdir()

  # track both frequencies
  track_freq_contour(
    X = ad[!is.na(ad$start), ], flim = c(0, 5), ovlp = 90,
    it = "tiff", bp = c(1, 3), contour = "both", wl = 300, path = tempdir()
  )
}




cleanEx()
nameEx("try_na")
### * try_na

flush(stderr()); flush(stdout())

### Name: try_na
### Title: Wrapper for "try" function
### Aliases: try_na

### ** Examples

{
# try a function that does not exists to produce an error
try_na(crazy78(12))

# try a real function (no error)
try_na(mean(1:5))
}




cleanEx()
nameEx("tweak_spectro")
### * tweak_spectro

flush(stderr()); flush(stdout())

### Name: tweak_spectro
### Title: Plot a mosaic of spectrograms with varying display parameters
### Aliases: tweak_spectro

### ** Examples

## Not run: 
##D # Save to temporary working directory
##D 
##D # save sound file examples
##D data(list = c("Phae.long1", "lbh_selec_table"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) 
##D 
##D # variable collevels
##D tweak_spectro(X = lbh_selec_table, wl = 164, ovlp = c(90), wn = c("flattop"), 
##D length.out = 16, nrow = 4, ncol = 4, width = 20, height = 11.3, rm.axes = TRUE, 
##D cex = 1, box = F, collev.min = c(-20, -150), path = tempdir(), flim = c(0, 10))
##D 
##D # variable overlap and wn
##D tweak_spectro(X = lbh_selec_table, wl = 164, ovlp = c(50, 90), 
##D wn = c("hanning", "hamming", "rectangle", "bartlett", "blackman", "flattop"),
##D length.out = 7, nrow = 6, ncol = 7, width = 20, height = 11.3, rm.axes = TRUE, 
##D cex = 1, box = F, path = tempdir(), flim = c(0, 10))
##D 
##D # variable wl and wn
##D tweak_spectro(X = lbh_selec_table, wl = c(100, 1000), ovlp = c(50, 90), wn = "all", 
##D length.out = 5, nrow = 10, ncol = 14, width = 20, height = 11.3, rm.axes = TRUE, 
##D cex = 0.7, path = tempdir(), flim = c(0, 10))
##D 
##D # variable wl, collev.min and wn 
##D tweak_spectro(X = lbh_selec_table, wl = c(100, 1000), ovlp = 90, 
##D wn = c("hanning", "hamming", "rectangle"), collev.min = c(-110, -25), 
##D length.out = 3, nrow = 10, ncol = 14, width = 20, height = 11.3, rm.axes = TRUE,
##D  cex = 0.7, path = tempdir(), flim = c(0, 10))
##D  
##D  # variable wl, wn and pal
##D  tweak_spectro(X = lbh_selec_table, wl = c(100, 1000), ovlp = 90, 
##D  wn = c("hanning", "hamming", "rectangle"), 
##D  pal = c("reverse.gray.colors.2", "reverse.topo.colors", 
##D  "reverse.terrain.colors", "reverse.cm.colors"), 
##D  length.out = 4, nrow = 5, ncol = 10, width = 20, height = 11.3,
##D   rm.axes = TRUE, cex = 0.7, lab.mar = 2, path = tempdir(), flim = c(0, 10))
##D   
##D   # wl, wn and pal
##D   tweak_spectro(X = lbh_selec_table, wl = c(100, 1000), ovlp = 90,
##D    wn = c("hanning", "hamming", "rectangle"), 
##D   pal = c("reverse.gray.colors.2", "reverse.topo.colors", 
##D   "reverse.terrain.colors", "reverse.cm.colors"), 
##D   length.out = 4, nrow = 5, ncol = 10, width = 20, height = 11.3, rm.axes = TRUE,
##D    cex = 0.7, group.tag = "wn",  spec.mar = 0.4, lab.mar = 0.8, box = FALSE, 
##D    tag.pal = list(reverse.cm.colors), path = tempdir(), flim = c(0, 10))
##D 
##D check this floder
##D tempdir()
## End(Not run)



cleanEx()
nameEx("warbleR_options")
### * warbleR_options

flush(stderr()); flush(stdout())

### Name: warbleR_options
### Title: Setting warbleR options
### Aliases: warbleR_options

### ** Examples

{
  # load data and save in temporary working directory
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  # sig2noise with progress bar (by default is TRUE)
  a <- sig2noise(X = lbh_selec_table, mar = 0.1, path = tempdir())

  # set progress bar to FALSE with warbleR_options
  warbleR_options(pb = FALSE, path = tempdir())

  # sig2noise without progress bar
  a <- sig2noise(X = lbh_selec_table, mar = 0.1)

  # sig2noise with progress bar by setting it within the function call (overwritting options)
  a <- sig2noise(X = lbh_selec_table, pb = TRUE, mar = 0.1)

  # sig2noise without progress bar using warbleR_options setting again
  a <- sig2noise(X = lbh_selec_table, mar = 0.1)
}



cleanEx()
nameEx("wav_2_flac")
### * wav_2_flac

flush(stderr()); flush(stdout())

### Name: wav_2_flac
### Title: Convert .wav files to .flac
### Aliases: wav_2_flac

### ** Examples

## Not run: 
##D # create some .wav files
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
##D writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
##D writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
##D writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
##D writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
##D 
##D # Convert all files to .flac format
##D wav_2_flac(path = tempdir())
##D 
##D # check this folder!!
##D open_wd(tempdir())
## End(Not run)




cleanEx()
nameEx("wpd_features")
### * wpd_features

flush(stderr()); flush(stdout())

### Name: wpd_features
### Title: Measure wavelet packet decomposition features (EXPERIMENTAL)
### Aliases: wpd_features

### ** Examples

{
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

  # not normalize
  wpd_features(lbh_selec_table[1:5, ], threshold2 = 0.3, nor = FALSE, path = tempdir())
}




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
