pkgname <- "warbleR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "warbleR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('warbleR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("autodetec")
### * autodetec

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autodetec
### Title: Automatically detect vocalizations in sound files
### Aliases: autodetec

### ** Examples

## Not run: 
##D # Set temporary working directory
##D setwd(tempdir())
##D 
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D writeWave(Phae.long4,"Phae.long4.wav") 
##D 
##D ad <- autodetec(threshold = 5, env = "hil", ssmooth = 300, power=1,
##D bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE,
##D wl = 300, ls = FALSE, sxrow = 2, rows = 4, mindur = 0.1, maxdur = 1, set = TRUE)
##D 
##D #run it with different settings
##D ad <- autodetec(threshold = 90, env = "abs", ssmooth = 300, power = 1, redo = TRUE,
##D bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE, 
##D wl = 300, ls = FALSE,  sxrow = 2, rows = 4, mindur=0.1, maxdur=1, set = TRUE)
##D 
##D #check this folder!!
##D getwd()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autodetec", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("catalog")
### * catalog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: catalog
### Title: Create catalog of vocal signals
### Aliases: catalog

### ** Examples

## Not run: 
##D # Set temporary working directory
##D setwd(tempdir())
##D # save sound file examples
##D data(list = c("Phae.long1", "Phae.long2","selec.table"))
##D writeWave(Phae.long1,"Phae.long1.wav") 
##D writeWave(Phae.long2,"Phae.long2.wav")
##D  writeWave(Phae.long3,"Phae.long3.wav")
##D  writeWave(Phae.long4,"Phae.long4.wav")
##D 
##D 
##D catalog(X = selec.table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = T,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE,
##D  orientation = "v",  labels = c("sound.files", "selec"), legend = 0)
##D  
##D  #different time scales and tag palette
##D catalog(X = selec.table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = F,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, 
##D  orientation = "v",  labels = c("sound.files", "selec"), legend = 0, 
##D  tag.pal = list(terrain.colors))
##D  
##D  #adding tags and changing spectro palette
##D catalog(X = selec.table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = F,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, pal = reverse.heat.colors,
##D  orientation = "v",  labels = c("sound.files", "selec"), legend = 1, 
##D  tag.pal = list(terrain.colors), tags = "sound.files")
##D 
##D  #create a bigger selection table
##D  X <- rbind(selec.table, selec.table, selec.table, selec.table)
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
##D  collev = seq(-65, 0, 5), tag.pal = list(terrain.colors), tags = c("songtype", "indiv"))
##D 
##D 
##D # with legend
##D catalog(X = X, flim = c(1, 10), nrow = 5, ncol = 12, same.time.scale = F,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE,
##D  orientation = "v",  labels = c("sound.files", "selec"), legend = 3, 
##D  width = 20, collev = seq(-65, 0, 5), tag.pal = list(terrain.colors),
##D   tags = c("songtype", "indiv"))
##D   
##D   # horizontal orientation
##D catalog(X = X, flim = c(1, 10), nrow = 5, ncol = 12, same.time.scale = F,
##D  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE,
##D  orientation = "h",  labels = c("sound.files", "selec"), legend = 3, 
##D  width = 20, collev = seq(-65, 0, 5), tag.pal = list(terrain.colors),
##D   tags = c("songtype", "indiv"))
##D check this floder
##D getwd()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("catalog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("catalog2pdf")
### * catalog2pdf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: catalog2pdf
### Title: 'catalog2pdf' combines 'catalog' images into pdfs
### Aliases: catalog2pdf

### ** Examples

## Not run: 
##D # Set temporary working directory
##D setwd(tempdir())
##D 
##D # save sound file examples
##D data(list = c("Phae.long1", "Phae.long2"))
##D writeWave(Phae.long1,"Phae.long1.wav") 
##D writeWave(Phae.long2,"Phae.long2.wav")
##D 
##D catalog(X = selec.table, nrow = 2, ncol = 4)
##D 
##D # now create single pdf removing jpeg
##D catalog2pdf(keep.img = FALSE)
##D 
##D # check this floder
##D getwd()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("catalog2pdf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("checksels")
### * checksels

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: checksels
### Title: Check selection data frames
### Aliases: checksels

### ** Examples

{
# First set temporary folder
setwd(tempdir())

# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "selec.table"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")

checksels(X = selec.table)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("checksels", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("checkwavs")
### * checkwavs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: checkwavs
### Title: Check .wav files
### Aliases: checkwavs

### ** Examples

{
# First set temporary folder
setwd(tempdir())

# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")

# without selection data frame
checkwavs()

# without selection data frame
checkwavs(X = selec.table)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("checkwavs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("color.spectro")
### * color.spectro

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: color.spectro
### Title: Highlight spectrogram regions
### Aliases: color.spectro

### ** Examples

## Not run: 
##D  
##D # First set empty folder
##D setwd(tempdir())
##D data(list = c("Phae.long1", "selec.table"))
##D writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
##D 
##D  # subset selection table
##D  st <- selec.table[selec.table$sound.files == "Phae.long1.wav",]
##D  
##D  # read wave file as an R object
##D  sgnl <- tuneR::readWave(as.character(st$sound.files[1]))
##D  
##D  # create color column
##D  st$colors <- c("red2", "blue", "green")
##D  
##D  # highlight selections
##D  color.spectro(wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-90, 0, 5), 
##D  dB = "B", X = st, col.clm = "colors", base.col = "skyblue",  t.mar = 0.07, f.mar = 0.1, 
##D  interactive = NULL)
##D  
##D  # interactive (selected manually: you have to select them by clicking on the spectrogram)
##D  color.spectro(wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-90, 0, 5),
##D   dB = "B", col.clm = "colors", t.mar = 0.07, f.mar = 1, interactive = 2)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("color.spectro", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compare.methods")
### * compare.methods

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compare.methods
### Title: Assessing the performance of acoustic distance measurements
### Aliases: compare.methods

### ** Examples

{
# Set temporary working directory
setwd(tempdir())

data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")

compare.methods(X = selec.table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
ovlp = 90, res = 200, n = 10, length.out = 30,
methods = c("XCORR", "dfDTW"), parallel = 1, it = "jpeg")

#remove progress bar
compare.methods(X = selec.table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
ovlp = 90, res = 200, n = 10, length.out = 30,
methods = c("XCORR", "dfDTW"), parallel = 1, it = "jpeg", pb = FALSE)

#check this folder!
getwd()


#compare SP and XCORR
#first we need to create a larger data set as the PCA that summarizes the spectral parameters
#needs more units (rows) that variables (columns)
#so I just create a new selection table repeating 3 times selec.table
st2 <- rbind(selec.table, selec.table, selec.table)

#note that the selection labels should be also changed
st2$selec <- 1:nrow(st2)
#now we can compare SP method against XCORR
compare.methods(X = st2, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
ovlp = 90, res = 200, n = 10, length.out = 30,
methods = c("XCORR", "SP"), parallel = 1, it = "jpeg")

#compare SP method against dfDTW
compare.methods(X = st2, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
ovlp = 90, res = 200, n = 10, length.out = 30,
methods = c("dfDTW", "SP"), parallel = 1, it = "jpeg")

#alternatively we can provide our own SP matrix
sp <- specan(selec.table, bp = c(0, 10))

#and selec just a few variables to avoid the problem of # observations vs # parameters in PCA
sp <- sp[, 1:7]

compare.methods(X = selec.table, flim = c(0, 10), sp = sp, bp = c(0, 10), mar = 0.1, wl = 300,
ovlp = 90, res = 200, n = 10, length.out = 30,
methods = c("XCORR", "SP"), parallel = 1, it = "jpeg")

#note that "SP" should also be included as a method in 'methods'
#again, all images are saved in the working directory
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compare.methods", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("consolidate")
### * consolidate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: consolidate
### Title: Consolidate sound files into a single folder
### Aliases: consolidate

### ** Examples

{ 
# First set empty folder
setwd(tempdir())

# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))

# create first folder
dir.create("folder1")
writeWave(Phae.long1, file.path("folder1","Phae.long1.wav"))
writeWave(Phae.long2, file.path("folder1","Phae.long2.wav"))

# create second folder
dir.create("folder2")
writeWave(Phae.long3, file.path("folder2","Phae.long3.wav"))
writeWave(Phae.long4, file.path("folder2","Phae.long4.wav"))

# consolidate in a single folder
consolidate(path = tempdir())
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("consolidate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("coor.graph")
### * coor.graph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: coor.graph
### Title: Coordinated singing graphs
### Aliases: coor.graph

### ** Examples

{

# First set temporary folder
setwd(tempdir())

# load simulate singing events  (see data documentation)
data(sim.coor.sing)

# make coor.graphs in tiff format
coor.graph(X = sim.coor.sing, ovlp = TRUE, only.coor = FALSE, xl =2, res =80, 
it = "tiff", img = TRUE)


#' # make coor.graphs in graphic device format
cgs <- coor.graph(X = sim.coor.sing, ovlp = TRUE, only.coor = FALSE, img = FALSE)

cgs
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("coor.graph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("coor.test")
### * coor.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: coor.test
### Title: Randomization test for singing coordination
### Aliases: coor.test

### ** Examples

{
#load  simulated singing data (see data documentation)
data(sim.coor.sing)

# testing if coordination happens less than expected by chance
coor.test(sim.coor.sing, iterations = 100, less.than.chance = TRUE)

# testing if coordination happens more than expected by chance
coor.test(sim.coor.sing, iterations = 100, less.than.chance = FALSE)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("coor.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cut_sels")
### * cut_sels

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cut_sels
### Title: Cut selections into individual sound files
### Aliases: cut_sels

### ** Examples

{ 
# First set empty folder
setwd(tempdir())

# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")

# make spectrograms

cut_sels(selec.table)
 
cut_sels(selec.table, overwrite = TRUE, labels = c("sound.files", "selec", "sel.comment"))
 
 #check this folder!!
getwd()
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cut_sels", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfDTW")
### * dfDTW

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfDTW
### Title: Acoustic dissimilarity using dynamic time warping on dominant
###   frequency contours
### Aliases: dfDTW

### ** Examples

{
# set the temp directory
setwd(tempdir())

#load data
data(list = c("Phae.long1", "Phae.long2","selec.table"))
writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
writeWave(Phae.long1, "Phae.long1.wav")

# run function 
dfDTW(selec.table, length.out = 30, flim = c(1, 12), bp = c(2, 9), wl = 300)

}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfDTW", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dfts")
### * dfts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dfts
### Title: Extract the dominant frequency values as a time series
### Aliases: dfts

### ** Examples

{
# set the temp directory
setwd(tempdir())

#load data
data(list = c("Phae.long1", "Phae.long2","selec.table"))
writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
writeWave(Phae.long1, "Phae.long1.wav")

# run function 
dfts(X = selec.table, length.out = 30, flim = c(1, 12), bp = c(2, 9), wl = 300)

}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dfts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ffDTW")
### * ffDTW

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ffDTW
### Title: Acoustic dissimilarity using dynamic time warping on fundamental
###   frequency contours
### Aliases: ffDTW

### ** Examples

{
# set the temp directory
setwd(tempdir())

#load data
data(list = c("Phae.long1", "Phae.long2","selec.table"))
writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
writeWave(Phae.long1, "Phae.long1.wav")

# run function 
ffDTW(selec.table[1:4,], length.out = 30, flim = c(1, 12), img = TRUE, bp = c(1, 9), wl = 300)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ffDTW", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ffts")
### * ffts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ffts
### Title: Extract the fundamental frequency values as a time series
### Aliases: ffts

### ** Examples

{
# set the temp directory
setwd(tempdir())

#load data
data(list = c("Phae.long1", "Phae.long2","selec.table"))
writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
writeWave(Phae.long2, "Phae.long2.wav") #save sound files 

# run function 
ffts(selec.table, length.out = 50, flim = c(1, 12), bp = c(2, 9), wl = 300)

# Fundamental frequency is not accurate for noisy signals, works better with pure tones

}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ffts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("filtersels")
### * filtersels

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: filtersels
### Title: Subset selection data frames based on manually filtered image
###   files
### Aliases: filtersels

### ** Examples

{ 
# First set temporary folder
setwd(tempdir())

# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "selec.table"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")

specreator(selec.table, flim = c(0, 11), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), 
picsize = 2, res = 300, cexlab = 2, mar = 0.05, wl = 300)

#go to the working directory and delete some images

#filter selection data frame
fmloc <- filtersels(X = selec.table)

#this data frame does not have the selections corresponding to the images that were deleted
fmloc

#now using lspec images
lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300, ovlp = 10)

#go to the working directory and delete lspec images (the ones with several rows of spectrograms)

#filter selection data frame

}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("filtersels", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fixwavs")
### * fixwavs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fixwavs
### Title: Fix .wav files to allow importing them into R
### Aliases: fixwavs

### ** Examples

## Not run: 
##D # Set temporary working directory
##D setwd(tempdir())
##D 
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D writeWave(Phae.long4,"Phae.long4.wav") 
##D 
##D fixwavs(files = selec.table$sound.files)
##D 
##D #check this folder
##D getwd()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fixwavs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("frange")
### * frange

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: frange
### Title: Detect frequency range iteratively
### Aliases: frange

### ** Examples

{
# First set temporary folder
setwd(tempdir())

data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")

frange(X = selec.table, wl = 112, fsmooth = 1, threshold = 13, widths = c(4, 1), 
img = TRUE, pb = TRUE, it = "tiff", line = TRUE, mar = 0.1, bp = c(1,10.5), 
flim = c(0, 11))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("frange", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("frange.detec")
### * frange.detec

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: frange.detec
### Title: Detect frequency range on wave objects
### Aliases: frange.detec

### ** Examples

{
data(tico)
frange.detec(wave = tico, wl = 512, fsmooth = 0.01, threshold = 1, bp = c(2, 8),
 widths = c(4, 2))

data(sheep)
frange.detec(wave = sheep, wl = 512, fsmooth = 0.2, threshold = 50, bp = c(0.3, 1), 
flim = c(0, 1.5), pal = reverse.heat.colors, main = "sheep")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("frange.detec", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is.selection.table")
### * is.selection.table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is.selection.table
### Title: Check if object is of class "selection.table"
### Aliases: is.selection.table

### ** Examples

{
# First set temporary folder
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))

is.selection.table(selec.table)

setwd(tempdir())

writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")

st <- make.selection.table(selec.table)

is.selection.table(st)

class(st)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is.selection.table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lspec")
### * lspec

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lspec
### Title: Create long spectrograms of whole sound files
### Aliases: lspec

### ** Examples

{
# Set temporary working directory
setwd(tempdir())

# save sound file examples
data(list = c("Phae.long1", "Phae.long2","selec.table"))
writeWave(Phae.long1,"Phae.long1.wav") 
writeWave(Phae.long2,"Phae.long2.wav")

lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300)

# including selections
lspec(sxrow = 2, rows = 8, X = selec.table, pal = reverse.heat.colors, redo = TRUE, wl = 300)

#check this floder
getwd()
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lspec", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lspec2pdf")
### * lspec2pdf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lspec2pdf
### Title: 'lspec2pdf' combines 'lspec' images in .jpeg format to a single
###   pdf file.
### Aliases: lspec2pdf

### ** Examples

## Not run: 
##D # Set temporary working directory
##D setwd(tempdir())
##D 
##D # save sound file examples
##D data(list = c("Phae.long1", "Phae.long2"))
##D writeWave(Phae.long1,"Phae.long1.wav") 
##D writeWave(Phae.long2,"Phae.long2.wav")
##D 
##D lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300, it = "jpeg")
##D 
##D #now create single pdf removing jpeg
##D lspec2pdf(keep.img = FALSE)
##D 
##D # check this floder
##D getwd()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lspec2pdf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make.selection.table")
### * make.selection.table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make.selection.table
### Title: Create 'selection.table' class objects
### Aliases: make.selection.table

### ** Examples

{
# First set temporary folder
setwd(tempdir())

data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")

st <- make.selection.table(X = selec.table)

class(st)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make.selection.table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("manualoc")
### * manualoc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: manualoc
### Title: Interactive view of spectrograms
### Aliases: manualoc

### ** Examples

## Not run: 
##D #Set temporary working directory
##D setwd(tempdir())
##D 
##D # save wav file examples
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D writeWave(Phae.long4,"Phae.long4.wav")
##D 
##D manualoc(wl = 300)
##D # need to use the buttoms to manipulate function
##D # check working directory for .csv file after stopping function
##D #check here:
##D getwd()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("manualoc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("move.imgs")
### * move.imgs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: move.imgs
### Title: Move/copy image files between directories
### Aliases: move.imgs

### ** Examples

{
#Set temporary working directory
setwd(tempdir())

#load data
data("Cryp.soui")
writeWave(Cryp.soui, "Cryp.soui.wav") #save sound files 

#autodetec location of signals
ad <- autodetec(threshold = 6, bp = c(1, 3), mindur = 1.2,
maxdur = 3, img = FALSE, ssmooth = 600, wl = 300, flist = "Cryp.soui.wav")

#track dominant frequency graphs with freq reange detection
trackfreqs(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, it = "tiff",
bp = c(1, 3), contour = "df", wl = 300, frange = TRUE)

#copy files
move.imgs(cut = FALSE)

#cut files
move.imgs(cut = TRUE, to = "image_files")

# Check this folder
getwd()
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("move.imgs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mp32wav")
### * mp32wav

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mp32wav
### Title: Convert .mp3 files to .wav
### Aliases: mp32wav

### ** Examples

## Not run: 
##D # First set temporary folder
##D setwd(tempdir())
##D  
##D #Then download mp3 files from xeno-canto
##D querxc(qword = "Phaethornis aethopygus", download = TRUE)
##D 
##D # Convert all files to .wav format
##D mp32wav()
##D 
##D #check this folder!!
##D getwd()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mp32wav", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ovlp_sels")
### * ovlp_sels

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ovlp_sels
### Title: Find overlapping selections
### Aliases: ovlp_sels

### ** Examples

{
#no overlap
ovlp_sels(X =  selec.table)

# modified selec.table to make the first and second selection overlap
Y <- selec.table
Y$end[4] <- 1.5
  
 ovlp_sels(X =  Y)

# drop overlapping
 ovlp_sels(X =  Y, drop = TRUE)

# get index instead
 ovlp_sels(X =  Y, index = TRUE)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ovlp_sels", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("querxc")
### * querxc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: querxc
### Title: Access 'Xeno-Canto' recordings and metadata
### Aliases: querxc

### ** Examples

## Not run: 
##D # Set temporary working directory
##D setwd(tempdir())
##D 
##D # search without downloading
##D df1 <- querxc(qword = 'Phaethornis anthophilus', download = FALSE)
##D View(df1)
##D 
##D # downloading files
##D querxc(qword = 'Phaethornis anthophilus', download = TRUE)
##D 
##D # check this folder
##D getwd()
##D 
##D ## search using xeno-canto advance query ###
##D orth.pap <- querxc(qword = 'gen:orthonyx cnt:papua loc:tari', download = FALSE)
##D  
##D # download file using the output data frame as input
##D querxc(X = orth.pap)
##D 
##D # use quotes for queries with more than 1 word (e.g. Costa Rica),note that the 
##D # single quotes are used for the whole 'qword' and double quotes for the 2-word term inside
##D #Phaeochroa genus in Costa Rica 
##D phae.cr <- querxc(qword = 'gen:phaeochroa cnt:"costa rica"', download = FALSE)
##D 
##D # several terms can be searched for in the same field
##D # search for all female songs in sound type
##D femsong <- querxc(qword = 'type:song type:female', download = FALSE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("querxc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rm_sil")
### * rm_sil

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rm_sil
### Title: Remove silence in wave files
### Aliases: rm_sil

### ** Examples

{
# Set temporary working directory
setwd(tempdir())

# save sound file examples
data(list = c("Phae.long1", "Phae.long2","selec.table"))
sil <- silence(samp.rate = 22500, duration = 3, xunit = "time")

wv1 <- pastew(pastew(Phae.long1, sil, f = 22500, output = "Wave"), 
Phae.long2, f = 22500, output = "Wave")

#check silence in between amplitude peaks
env(wv1)

 #save wave file
 writeWave(object = wv1, filename = "wv1.wav", extensible = FALSE)

#remove silence
rm_sil(path = tempdir(), flist = "wv1.wav")

#check this floder
getwd()
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rm_sil", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("seltailor")
### * seltailor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: seltailor
### Title: Interactive view of spectrograms to tailor selections
### Aliases: seltailor

### ** Examples

## Not run: 
##D #Set temporary working directory
##D setwd(tempdir())
##D 
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D writeWave(Phae.long4,"Phae.long4.wav")
##D 
##D seltailor(X =  selec.table, flim = c(1,12), wl = 300, auto.next = TRUE)
##D 
##D # Read output .csv file
##D seltailor.df <- read.csv("seltailor_output.csv")
##D seltailor.df
##D 
##D # check this directory for .csv file after stopping function
##D getwd()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("seltailor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sig2noise")
### * sig2noise

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sig2noise
### Title: Measure signal-to-noise ratio
### Aliases: sig2noise

### ** Examples

{
# First set temporary folder
setwd(tempdir())

data(list = c("Phae.long1","selec.table"))
writeWave(Phae.long1, "Phae.long1.wav") #save sound files 

# specifying the correct margin is important
# use snrspecs to troubleshoot margins for sound files
sig2noise(selec.table[grep("Phae.long1", selec.table$sound.files), ], mar = 0.2)

# this smaller margin doesn't overlap neighboring signals
sig2noise(selec.table[grep("Phae.long1", selec.table$sound.files), ], mar = 0.1)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sig2noise", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("snrspecs")
### * snrspecs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: snrspecs
### Title: Spectrograms with background noise margins
### Aliases: snrspecs

### ** Examples

{
# Set temporary working directory
setwd(tempdir())
 
data(list = c("Phae.long1", "Phae.long2", "selec.table"))
writeWave(Phae.long1, "Phae.long1.wav") #save sound.files
writeWave(Phae.long2, "Phae.long2.wav") 

# make Phae.long1 and Phae.long2 spectrograms
# snrmar needs to be smaller before moving on to sig2noise()

snrspecs(selec.table, flim = c(0, 14), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), 
picsize = 2, res = 300, cexlab = 2, mar = 0.2, snrmar = 0.1, it = "jpeg", wl = 300)

# make only Phae.long1 spectrograms
# snrmar now doesn't overlap neighboring signals

snrspecs(selec.table[grepl(c("Phae.long1"), selec.table$sound.files), ], flim = c(3, 14),
inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), picsize = 2, res = 300, cexlab = 2,
mar = 0.2, snrmar = 0.01, wl = 300)

#check this folder!!
getwd()
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("snrspecs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sp.en.ts")
### * sp.en.ts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sp.en.ts
### Title: Extract the spectral entropy across signals as a time series
### Aliases: sp.en.ts

### ** Examples

{
# set the temp directory
setwd(tempdir())

#load data
data(list = c("Phae.long1", "Phae.long2",  "Phae.long3",  "Phae.long4","selec.table"))
writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
writeWave(Phae.long1, "Phae.long1.wav")
writeWave(Phae.long3, "Phae.long3.wav") #save sound files 
writeWave(Phae.long4, "Phae.long4.wav")

# without clip edges
sp.en.ts(X = selec.table, threshold = 10, bp = NULL, clip.edges = FALSE, length.out = 10,
 type = "b", sp.en.range = c(-25, 10))

# with clip edges and length.out 10
sp.en.ts(X = selec.table, threshold = 10, bp = c(2, 12), clip.edges = TRUE, length.out = 10)

}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sp.en.ts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("specan")
### * specan

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: specan
### Title: Measure acoustic parameters in batches of sound files
### Aliases: specan

### ** Examples

{
# First set temporary folder
setwd(tempdir())

data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")

a <- specan(X = selec.table, bp = c(0, 22))

# using a diferent threshold
a <- specan(X = selec.table, bp = c(0, 22), threshold = 20)
# View(a)

}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("specan", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("specreator")
### * specreator

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: specreator
### Title: Spectrograms of selected signals
### Aliases: specreator

### ** Examples

{ 
# First set empty folder
setwd(tempdir())
data(list = c("Phae.long1", "Phae.long2","selec.table"))
writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
writeWave(Phae.long2, "Phae.long2.wav")

# make spectrograms

specreator(selec.table, flim = c(0, 11), res = 300, mar = 0.05, wl = 300)
 
 #check this folder
getwd()
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("specreator", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("trackfreqs")
### * trackfreqs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: trackfreqs
### Title: Spectrograms with frequency measurements
### Aliases: trackfreqs

### ** Examples

{
#Set temporary working directory
setwd(tempdir())

#load data
data("Cryp.soui")
writeWave(Cryp.soui, "Cryp.soui.wav") #save sound files 

#autodetec location of signals
ad <- autodetec(threshold = 6, bp = c(1, 3), mindur = 1.2,
maxdur = 3, img = FALSE, ssmooth = 600, wl = 300, flist = "Cryp.soui.wav")

#track dominant frequency graphs with freq reange detection
trackfreqs(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, it = "tiff",
bp = c(1, 3), contour = "df", wl = 300, frange = TRUE)

#using users frequency data (custom.contour argument) 
#first get contours using dfts
df <- dfts(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, img = FALSE,
bp = c(1, 3),  wl = 300)

# now input the dfts output into trackfreqs         
trackfreqs(X = ad[!is.na(ad$start),], custom.contour = df ,flim = c(0, 5), ovlp = 90, it = "tiff")

# Check this folder
getwd()

#track both frequencies 
trackfreqs(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, it = "tiff",
bp = c(1, 3), contour = "both", wl = 300)

}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("trackfreqs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wavdur")
### * wavdur

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wavdur
### Title: Measure the duration of sound files
### Aliases: wavdur

### ** Examples

{
# Set temporary working directory
setwd(tempdir())

data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")

wavdur()
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wavdur", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("xcmaps")
### * xcmaps

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: xcmaps
### Title: Maps of 'Xeno-Canto' recordings by species
### Aliases: xcmaps

### ** Examples

## Not run: 
##D # search in xeno-canto
##D X <- querxc("Phaethornis anthophilus", download = FALSE)
##D 
##D #create image in R graphic device
##D xcmaps(X, img = FALSE)
##D 
##D #or save it as a file in the working directory
##D xcmaps(X)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("xcmaps", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("xcorr")
### * xcorr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: xcorr
### Title: Spectrogram cross-correlation
### Aliases: xcorr

### ** Examples

{
#First set temporary working directory
setwd(tempdir())

#load data
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4","selec.table"))
writeWave(Phae.long1, "Phae.long1.wav") #save sound files
writeWave(Phae.long2, "Phae.long2.wav")
writeWave(Phae.long3, "Phae.long3.wav")
writeWave(Phae.long4, "Phae.long4.wav")

xcor <- xcorr(X = selec.table, wl = 300, frange = c(2, 9), ovlp = 90,
dens = 1, wn = 'hanning', cor.method = "pearson")

}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("xcorr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("xcorr.graph")
### * xcorr.graph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: xcorr.graph
### Title: Pairwise plots of spectrogram cross-correlation scores
### Aliases: xcorr.graph

### ** Examples

{
#load data
#First set temporary working directory]
setwd(tempdir())

#load data
data(list = c("Phae.long1", "Phae.long2", "selec.table"))
writeWave(Phae.long1, "Phae.long1.wav") #save sound files
writeWave(Phae.long2, "Phae.long2.wav")

 #run cross correlation first
 xcor<-xcorr(X = selec.table[1:5,], wl =300, frange= c(2, 9), ovlp=90, dens=0.8, 
 wn='hanning', cor.method = "pearson", cor.mat = FALSE) 
 
 #plot pairwise scores
  xcorr.graph(X = xcor, cex.cor = 2, cex.lab = 1, rel.cex = FALSE)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("xcorr.graph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
