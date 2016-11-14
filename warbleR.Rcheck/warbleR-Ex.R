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
##D # First create empty folder
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
nameEx("checksels")
### * checksels

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: checksels
### Title: Check selection data frames
### Aliases: checksels

### ** Examples

## Not run: 
##D # First set temporary folder
##D setwd(tempdir())
##D 
##D # save wav file examples
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "manualoc.df"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D 
##D checksels(X = manualoc.df)
## End(Not run)



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

## Not run: 
##D # First set temporary folder
##D setwd(tempdir())
##D 
##D # save wav file examples
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D 
##D checkwavs()
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("checkwavs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compare.methods")
### * compare.methods

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compare.methods
### Title: Assessing the performance of acoustic distance measurements
### Aliases: compare.methods

### ** Examples

## Not run: 
##D # First create empty folder
##D setwd(tempdir())
##D 
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "manualoc.df"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D writeWave(Phae.long4,"Phae.long4.wav") 
##D 
##D compare.methods(X = manualoc.df, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
##D ovlp = 90, res = 200, n = 10, length.out = 30,
##D methods = c("XCORR", "dfDTW"), parallel = 1, it = "jpeg")
##D 
##D #check this folder!
##D getwd()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compare.methods", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("coor.graph")
### * coor.graph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: coor.graph
### Title: Coordinated singing graphs
### Aliases: coor.graph

### ** Examples

## Not run: 
##D 
##D # First set temporary folder
##D setwd(tempdir())
##D 
##D # load simulate singing events  (see data documentation)
##D , data(sim.coor.sing)
##D 
##D # make coor.graphs in tiff format
##D coor.graph(X = sim.coor.sing, ovlp = TRUE, only.coor = FALSE, xl =2, res =80, 
##D it = "tiff", img = TRUE)
##D 
##D 
##D #' # make coor.graphs in graphic device format
##D cgs <- coor.graph(X = sim.coor.sing, ovlp = TRUE, only.coor = FALSE, img = FALSE)
##D 
##D cgs
## End(Not run)



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

## Not run: 
##D #load  simulated singing data (see data documentation)
##D , data(sim.coor.sing)
##D 
##D # testing if coordination happens less than expected by chance
##D coor.test(sim.coor.sing, iterations = 1000, less.than.chance = TRUE)
##D 
##D # testing if coordination happens more than expected by chance
##D coor.test(sim.coor.sing, iterations = 1000, less.than.chance = FALSE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("coor.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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

## Not run: 
##D # set the temp directory
##D setwd(tempdir())
##D 
##D #load data
##D data(list = c("Phae.long1", "Phae.long2","manualoc.df"))
##D writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
##D writeWave(Phae.long1, "Phae.long1.wav")
##D 
##D # run function 
##D dfDTW(manualoc.df, length.out = 30, flim = c(1, 12), bp = c(2, 9), wl = 300)
##D 
## End(Not run)



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

## Not run: 
##D # set the temp directory
##D setwd(tempdir())
##D 
##D #load data
##D data(list = c("Phae.long1", "Phae.long2","manualoc.df"))
##D writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
##D writeWave(Phae.long1, "Phae.long1.wav")
##D 
##D # run function 
##D dfts(manualoc.df, length.out = 30, flim = c(1, 12), bp = c(2, 9), wl = 300)
##D 
## End(Not run)



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

## Not run: 
##D # set the temp directory
##D setwd(tempdir())
##D 
##D #load data
##D data(list = c("Phae.long1", "Phae.long2","manualoc.df"))
##D writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
##D writeWave(Phae.long1, "Phae.long1.wav")
##D 
##D # run function 
##D ffDTW(manualoc.df, length.out = 30, flim = c(1, 12), img = T, bp = c(1, 9), wl = 300)
##D 
## End(Not run)



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

## Not run: 
##D # set the temp directory
##D setwd(tempdir())
##D 
##D #load data
##D data(list = c("Phae.long1", "Phae.long2","manualoc.df"))
##D writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
##D writeWave(Phae.long2, "Phae.long2.wav") #save sound files 
##D 
##D # run function 
##D ffts(manualoc.df, length.out = 50, flim = c(1, 12), bp = c(2, 9), wl = 300)
##D 
##D Note that fundamental frequency is not accurate for noisy signals, works better with pure tones
##D 
## End(Not run)



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

## Not run: 
##D  
##D # First set temporary folder
##D setwd(tempdir())
##D 
##D # save wav file examples
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "manualoc.df"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D 
##D specreator(manualoc.df, flim = c(0, 11), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), 
##D picsize = 2, res = 300, cexlab = 2, mar = 0.05, wl = 300)
##D 
##D #go to the working directory and delete some images
##D 
##D #filter selection data frame
##D 
##D #this data frame does not have the selections corresponding to the images that were deleted
##D fmloc
##D 
##D #now using lspec images
##D lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300, ovlp = 10)
##D 
##D #go to the working directory and delete lspec images (the ones with several rows of spectrograms)
##D 
##D #filter selection data frame
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("filtersels", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("imp.raven")
### * imp.raven

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: imp.raven
### Title: Import Raven selections
### Aliases: imp.raven

### ** Examples

## Not run: 
##D # First set temporary folder
##D setwd(tempdir())
##D 
##D data(selection.files)
##D 
##D write.table(selection.files[[1]],file = "100889-Garrulax monileger.selections.txt",
##D row.names = FALSE, sep= "\t")
##D 
##D write.table(selection.files[[2]],file = "1023-Arremonops rufivirgatus.selections.txt",
##D row.names = FALSE, sep= "\t")
##D 
##D #providing the name of the column with the sound file names
##D rav.dat<-imp.raven(sound.file.col = "End.File", all.data = FALSE)
##D 
##D View(rav.dat)
##D 
##D #getting all the data
##D rav.dat<-imp.raven(all.data = TRUE)
##D View(rav.dat)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("imp.raven", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("imp.syrinx")
### * imp.syrinx

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: imp.syrinx
### Title: Import Syrinx selections
### Aliases: imp.syrinx

### ** Examples

## Not run: 
##D # First set temporary folder
##D setwd(tempdir())
##D 
##D #load data 
##D data(selection.files)
##D 
##D write.table(selection.files[[3]],file = "harpyeagle.wav.txt",row.names = FALSE,
##D  col.names = FALSE, sep= "\t")
##D 
##D write.table(selection.files[[4]],file = "Phae.long4.wav.txt",row.names = FALSE, 
##D col.names = FALSE, sep= "\t")
##D 
##D syr.dat<-imp.syrinx(all.data = FALSE)
##D 
##D View(syr.dat)
##D 
##D #getting all the data
##D syr.dat<-imp.syrinx(all.data = TRUE)
##D 
##D View(syr.dat)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("imp.syrinx", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lspec")
### * lspec

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lspec
### Title: Create long spectrograms of whole sound files
### Aliases: lspec

### ** Examples

## Not run: 
##D # First create empty folder
##D setwd(tempdir())
##D # save sound file examples
##D data(list = c("Phae.long1", "Phae.long2","manualoc.df"))
##D writeWave(Phae.long1,"Phae.long1.wav") 
##D writeWave(Phae.long2,"Phae.long2.wav")
##D 
##D lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300)
##D 
##D # including selections
##D lspec(sxrow = 2, rows = 8, X = manualoc.df, pal = reverse.heat.colors, redo = TRUE, wl = 300)
##D 
##D check this floder
##D getwd()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lspec", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lspec2pdf")
### * lspec2pdf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lspec2pdf
### Title: 'lspec2pdf' combines lspec images in .jpeg format to a single
###   pdf file.
### Aliases: lspec2pdf

### ** Examples

## Not run: 
##D # First create empty folder
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
##D lspec2pdf(keep.jpeg = FALSE)
##D 
##D check this floder
##D getwd()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lspec2pdf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
##D #First create empty folder
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
nameEx("querxc")
### * querxc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: querxc
### Title: Access Xeno-Canto recordings and metadata
### Aliases: querxc

### ** Examples

## Not run: 
##D # First create empty folder
##D setwd(tempdir())
##D 
##D #search without downloading
##D df1 <- querxc(qword = "Phaethornis anthophilus", download = FALSE)
##D View(df1)
##D 
##D #downloading files
##D querxc(qword = "Phaethornis anthophilus", download = TRUE)
##D #check this folder!!
##D getwd()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("querxc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("seltailor")
### * seltailor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: seltailor
### Title: Interactive view of spectrograms to tailor start and end of
###   selections
### Aliases: seltailor

### ** Examples

## Not run: 
##D #First create empty folder
##D setwd(tempdir())
##D 
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "manualoc.df"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D writeWave(Phae.long4,"Phae.long4.wav")
##D 
##D seltailor(X =  manualoc.df, flim = c(1,12), wl = 300, auto.next = FALSE)
##D 
##D # need to use the buttoms to manipulate function
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

## Not run: 
##D # First set temporary folder
##D setwd(tempdir())
##D 
##D data(list = c("Phae.long1","manualoc.df"))
##D writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
##D 
##D # specifying the correct margin is important
##D # use snrspecs to troubleshoot margins for sound files
##D sig2noise(manualoc.df[grep("Phae.long1", manualoc.df$sound.files), ], mar = 0.2)
##D 
##D # this smaller margin doesn't overlap neighboring signals
##D sig2noise(manualoc.df[grep("Phae.long1", manualoc.df$sound.files), ], mar = 0.1)
## End(Not run)




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

## Not run: 
##D # First create empty folder
##D setwd(tempdir())
##D  
##D data(list = c("Phae.long1", "Phae.long2", "manualoc.df"))
##D writeWave(Phae.long1, "Phae.long1.wav") #save sound.files
##D writeWave(Phae.long2, "Phae.long2.wav") 
##D 
##D # make Phae.long1 and Phae.long2 spectrograms
##D # snrmar needs to be smaller before moving on to sig2noise()
##D 
##D snrspecs(manualoc.df, flim = c(0, 14), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), 
##D picsize = 2, res = 300, cexlab = 2, mar = 0.2, snrmar = 0.1, it = "jpeg", wl = 300)
##D 
##D # make only Phae.long1 spectrograms
##D # snrmar now doesn't overlap neighboring signals
##D 
##D snrspecs(manualoc.df[grepl(c("Phae.long1"), manualoc.df$sound.files), ], flim = c(3, 14),
##D inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), picsize = 2, res = 300, cexlab = 2,
##D mar = 0.2, snrmar = 0.01, wl = 300)
##D 
##D #check this folder!!
##D getwd()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("snrspecs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("specan")
### * specan

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: specan
### Title: Measure acoustic parameters in batches of sound files
### Aliases: specan

### ** Examples

## Not run: 
##D # First set temporary folder
##D setwd(tempdir())
##D 
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "manualoc.df"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D writeWave(Phae.long4,"Phae.long4.wav")
##D 
##D a <- specan(X = manualoc.df, bp = c(0, 22))
##D 
##D # using a diferent threshold
##D a <- specan(X = manualoc.df, bp = c(0, 22), threshold = 20)
##D # View(a)
##D 
## End(Not run)



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

## Not run: 
##D  
##D # First set empty folder
##D setwd(tempdir())
##D data(list = c("Phae.long1", "Phae.long2","manualoc.df"))
##D writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
##D writeWave(Phae.long2, "Phae.long2.wav")
##D 
##D # make spectrograms
##D 
##D specreator(manualoc.df, flim = c(0, 11), res = 300, mar = 0.05, wl = 300)
##D  
##D  #' #check this folder!!
##D getwd()
## End(Not run)




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

## Not run: 
##D #Set temporal folder as working directory
##D setwd(tempdir())
##D 
##D #load data
##D data("Cryp.soui")
##D writeWave(Cryp.soui, "Cryp.soui.wav") #save sound files 
##D 
##D #autodetec location of signals
##D ad <- autodetec(threshold = 6, bp = c(1, 3), mindur = 1.2,
##D maxdur = 3, img = FALSE, ssmooth = 600, wl = 300, flist = "Cryp.soui.wav")
##D 
##D #track dominant frequency graphs
##D trackfreqs(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, it = "tiff",
##D bp = c(1, 3), contour = "df", wl = 300)
##D  
##D # Check this folder
##D getwd()
##D 
##D #track both frequencies 
##D trackfreqs(X = ad[!is.na(ad$start),], flim = c(0, 5), ovlp = 90, it = "tiff",
##D bp = c(1, 3), contour = "both", wl = 300)
##D 
## End(Not run)



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

## Not run: 
##D # First create empty folder
##D setwd(tempdir())
##D 
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
##D writeWave(Phae.long1,"Phae.long1.wav")
##D writeWave(Phae.long2,"Phae.long2.wav")
##D writeWave(Phae.long3,"Phae.long3.wav")
##D 
##D wavdur()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wavdur", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("xcmaps")
### * xcmaps

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: xcmaps
### Title: Maps of Xeno-Canto recordings by species
### Aliases: xcmaps

### ** Examples

## Not run: 
##D X <- querxc("Phaethornis anthophilus", download = FALSE)
##D View(X)
##D xcmaps(X)
##D xcmaps(X, img = FALSE, it = "jpeg")
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

## Not run: 
##D #First set temporal working directory
##D setwd(tempdir())
##D 
##D #load data
##D data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4","manualoc.df"))
##D writeWave(Phae.long1, "Phae.long1.wav") #save sound files
##D writeWave(Phae.long2, "Phae.long2.wav")
##D writeWave(Phae.long3, "Phae.long3.wav")
##D writeWave(Phae.long4, "Phae.long4.wav")
##D 
##D xcor <- xcorr(X = manualoc.df, wl = 300, frange = c(2, 9), ovlp = 90, 
##D dens = 1, wn = 'hanning', cor.method = "pearson") 
##D 
## End(Not run)



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

## Not run: 
##D #load data
##D #First set temporal working directory]
##D setwd(tempdir())
##D 
##D #load data
##D data(list = c("Phae.long1", "Phae.long2", "manualoc.df"))
##D writeWave(Phae.long1, "Phae.long1.wav") #save sound files
##D writeWave(Phae.long2, "Phae.long2.wav")
##D 
##D  #run cross correlation first
##D  xcor<-xcorr(X = manualoc.df[1:5,], wl =300, frange= c(2, 9), ovlp=90, dens=0.8, wn='hanning', 
##D  cor.method = "pearson") 
##D  
##D  #plot pairwise scores
##D   xcorr.graph(X = xcor, cex.cor = 2, cex.lab = 1, rel.cex = FALSE)
## End(Not run)



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
