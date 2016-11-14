library(devtools)
# install_github("maRce10/warbleR")
# library(warbleR)
# library(dtw)

setwd("/home/m/Dropbox/warbleR/github version/R")

rfiles <- list.files(pattern = ".R")

#check were a word is found
for(f in rfiles){
  x <- readLines(f)
  y <- grep('dodgerblue', x, fixed = T, value = T, ignore.case = T)
  if(length(y)>0) {print(f) 
    for(i in y) print(i)}
}


#replace frases
for(f in rfiles){
  
  x <- readLines(f)
  y <- gsub('dodgerblue', 'red2', x, fixed = T)
  cat(y, file=f, sep="\n")
  
}


#NP

#go to package directory
setwd("/home/m/Dropbox/warbleR/github version/")

#delete NAMESPACE file
unlink("/home/m/Dropbox/warbleR/github version/NAMESPACE")

#run document twice
devtools::document()
devtools::document()

devtools::check("/home/m/Dropbox/warbleR/github version")


# unlink(list.files(path = "/home/m/Dropbox",pattern = "tar.gz$",full.names = T))
# 
# devtools::build(manual = T,vignettes = T)
# 
# devtools::release_checks(pkg = "/home/m/Dropbox/warbleR")

# check as in cran

system("R CMD build /home/m/Dropbox/warbleR --resave-data")

system("R CMD check /home/m/Dropbox/warbleR --as-cran")

# only build manual 
system("R CMD Rd2pdf /home/m/Dropbox/warbleR")

setwd(tempdir())

Tina.majo <- readWave("/media/m/Phaethornis/BACKUPS/Biblioteca de cantos/Tin.maj.2009.7.18.0004.wav", from = 11, to = 30, units = "seconds")

tm <- downsample(Tina.majo,samp.rate =  22050)


writeWave(Cryp.soui, "Cryp.soui.wav")

ad <- autodetec(threshold = 15, bp = c(1, 3), flim = c(0, 5), mindur = 1.2, maxdur = 3, sxrow = 5, rows = 3, ssmooth = 600, parallel = 1, ls = F, redo = F, img = T)


system.time(trackfreqs(X = manualoc.df, flim = c(1, 10), ovlp = 90, it = "tiff", bp = c(0, 100), parallel = 1, contour = "df",threshold = 2))


lspec(flim = c(0, 6), sxrow = 3, rows = 5, wl = 100, ovlp = 90, it = "tiff")

unlink(list.files(pattern = ".jpeg$|.tiff$"))

data(list = c("Phae.long1", "Phae.long2","manualoc.df"))
writeWave(Phae.long1, "Phae.long1.wav") #save sound files 
writeWave(Phae.long2, "Phae.long2.wav")

# make spectrograms
system.time(specreator(manualoc.df, flim = c(0, 11), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1),picsize = 3, res = 100, cexlab = 1, mar = 0.05, parallel = 1, osci = T))


system(paste("nautilus", getwd()))


library(warbleR)
setwd(tempdir())


data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "manualoc.df"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")

system.time(ad <- autodetec(X = X,threshold = 10, env = "abs", ssmooth = 300, power = 1, redo = T, img = T,
                bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,9), osci = TRUE,
                wl = 300, ls = T,  sxrow = 1, rows = 5, mindur=0.1, maxdur=1, set = TRUE, parallel = 2))


seltailor(X =  manualoc.df[c(1,4,8,11),], flim = c(1,12), wl = 300, auto.next = F, mar = 0.05, pause = 1, comments = F)

read.csv("seltailor_output.csv")

unlink("seltailor_output.csv")


seltailor(X =  aa[,-ncol(aa)], flim = c(1,12), wl = 300, auto.next = T, mar = 0.05, pause = 0.01)



unlink("seltailor_output.csv")

seltailor.df <- read.csv("seltailor_output.csv")

# in case you skipt some selections (using the 'next' button)
seltailor.df <- seltailor.df[!is.na(seltailor.df$selec), ]

#to include comments (or other columns) from the input data frame
newdf <- merge(seltailor.df, manualoc.df[,c(1, 2, 5, 6)], by = c("sound.files", "selec"), all = FALSE)

#if not all selection were adjusted you can put all selections back together 
newdf2 <- rbind(newdf, manualoc.df[!paste(manualoc.df$sound.files, manualoc.df$selec) %in% paste(newdf$sound.files, newdf$selec), ])

#order by sound file and selection
newdf2 <- newdf2[order(newdf2$sound.files, newdf2$selec), ]

newdf2




# check this directory for .csv file after stopping function
getwd()


install.packages("warbleR")

# a<-c("url", "evaluate", "git2r", "jsonlite", "mime", "R6", "Rcpp",
  # "rversions", "xml2")
# sapply(a,install.packages)

library(warbleR)

#go to package directory
setwd("/home/m/MEGAsync/WarbleR package/regular/")

# setwd("/home/m/MEGAsync/WarbleR package/ps/")

#delete NAMESPACE file
unlink("/home/m/MEGAsync/WarbleR package/regular/NAMESPACE")

#run document twice
devtools::document()
devtools::document()

devtools::check("/home/m/MEGAsync/WarbleR package/regular/")

unlink(list.files(path = "/home/m/MEGAsync/WarbleR package/",pattern = "tar.gz$",full.names = T))

devtools::build(manual = T,vignettes = T)

devtools::release_checks(pkg = "/home/m/MEGAsync/WarbleR package/regular/")



tools::checkRdaFiles("/home/m/Documents/warbleR/Data")

#citation file
basecit <- system.file("CITATION", package="base")
readLines(basecit)
source(basecit, echo=TRUE)
readCitationFile(basecit)
use_data(Arre.aura,Phae.cuvi,Phae.long1,Phae.long2,Phae.long3,Phae.long4,internal = F,overwrite = T)



setwd(tempdir())
a<-querxc(qword = "Phaethornis")
table(a[,3])

system.time(results <- querxc(qword = "Phaethornis aethopygus",download = T, parallel = 2, file.name = c("english name", "vocalization type")))

unlink(list.files(pattern = ".mp3"))

a <- querxc(X = results[,-7], qword = "Phaethornis aethopygus",download = T, parallel = 4, file.name = c("english name", "genus", "country", "LOCALITY"))



setwd(tempdir())

results <- querxc(qword = "Phaethornis aethopygus",download = F, parallel = 1)

system.time(querxc(X = results[1:2,], parallel = 1, file.name = c("english name", "vocalization type")))

unlink(list.files(pattern = ".mp3"))

system.time(querxc(X = results[1:2,], parallel = 2, file.name = c("english name", "vocalization type")))

unlink(list.files(pattern = ".mp3"))


mp32wav()
detach("package:maps")
detach("package:tuneR")
detach("package:seewave")
detach("package:warbleR")
help(warbleR)


rm(list=ls())

autodetec()

checkwavs()
specreator



data(list = c("Arre.aura", "Phae.cuvi"))
data(manualoc.df)

X<-manualoc.df[sample(1:nrow(manualoc.df),5),]
X$sel.comment[c(1,3)]<-NA
X$sel.comment<-NA



Arre.aura<-downsample(Arre.aura, 22500)
Phae.cuvi<-downsample(Phae.cuvi, 22500)
Arre.aura<-cutw(Arre.aura,from = 0,to = 19, output = "Wave")
Phae.cuvi<-cutw(Phae.cuvi,from = 0,to = 19, output = "Wave")

Phae.long1<-downsample(Phae.long1, 22500)
Phae.long2<-downsample(Phae.long2, 22500)
Phae.long3<-downsample(Phae.long3, 22500)
Phae.long4<-downsample(Phae.long4, 22500)


setwd(tempdir())
# data(list = c("Arre.aura", "Phae.cuvi"))
# writeWave(Arre.aura, "Arre.aura.wav") #save sound files
# writeWave(Phae.cuvi, "Phae.cuvi.wav")
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "manualoc.df"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")


system.time(lspec(flim = c(1,12),it = "tiff",sxrow = 0.5, parallel = 1, redo = T))


df<-data.frame(sound.files=list.files(pattern = "wav$"),
               selec=1,start=0,end=30)

ad <- autodetec(threshold=3, env="abs", msmooth=c(100,99), power=1, img = F, redo = T,
                 xl = 2, picsize = 7, res = 200, flim= c(0,5), osci = TRUE,
                wl = 512, ls = F,  sxrow = 1, rows = 10, set =T,it = "tiff", parallel = 1)

ad <- autodetec(threshold = 10, env = "abs", ssmooth = 300, power = 1, redo = T,
bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE, 
wl = 300, ls = FALSE,  sxrow = 1, rows = 10, mindur=0.1, maxdur=1, set = TRUE, parallel = 3)

ad2<-ad[grep("5s.wav",ad$sound.files),]
lspec(flim = c(1,5),it = "tiff",sxrow = 1,wl = 212, ovlp = 80,flist = list.files(pattern = "wav$")[1])


specreator(ad[1:5,],wl = 512,ovlp = 90,flim = c(0,4),mar = 0.1,trel = T,inner.mar = c(5,4,4,2),parallel = 2)

manualoc(osci = T)



X <- manualoc.df

trackfreqs(manualoc.df, flim = c(3, 14), inner.mar = c(4,4.5,2,1), 
           outer.mar = c(4,2,2,1), picsize = 2, res = 300, cexlab = 2,  
           bp = c(3, 14), cex = c(1.5, 2), col = c("blue", "red"),  mar = 0.09, 
           lpos = "bottomright", parallel = 2, contour = "ff")
           

# make Arre.aura and Phae.cuvi spectrograms

trackfreqs(X, flim = c(0, 14), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1),
           picsize = 2, res = 300, cexlab = 2, bp = c(0, 14), cex = c(1.5, 2),
           col = c("blue", "red"),  mar = 0.09, lpos = "bottomright", contour = "df")

# make only Arre.aura spectrograms

trackfreqs(X[grepl(c("Arre"), X[,1]), ], flim = c(3, 14), inner.mar = c(4,4.5,2,1),
           outer.mar = c(4,2,2,1), picsize = 2, res = 300, cexlab = 2, fmax = 14,
           bp = c(3, 14), cex = c(1.5, 2), col = c("blue", "red"),  mar = 0.09,
           lpos = "bottomright")

setwd(tempdir())

data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")
data(manualoc.df)
a <- specan(X = manualoc.df, bp = c(0, 22), parallel = 2, path = "ASS")
system.time(a <- specan(X = manualoc.df, bp = c(0, 22), parallel = 2))

# using a diferent threshold
a <- specan(X =ad, bp = c(0, 10), threshold = 15)
View(a)


data(list = c("Arre.aura","manualoc.df"))
writeWave(Arre.aura, "Arre.aura.wav") #save sound files 

# specifying the correct margin is important
# use snrspecs to troubleshoot margins for sound files
sig2noise(manualoc.df[grep("Arre", manualoc.df$sound.files), ], mar = 0.2)

# this smaller margin doesn't overlap neighboring calls
sig2noise(manualoc.df[grep("Arre", manualoc.df$sound.files), ], mar = 0.1)
#' 
#' # also works


###########checking examples


setwd("/home/m/Documents")

dir.create(file.path(getwd(),"temp"))
setwd(file.path(getwd(),"temp"))

data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav") 

ad <- autodetec(threshold=5, env="hil", msmooth=c(900,90), power=1, 
                bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE, 
                wl = 300, ls = FALSE,  sxrow = 2, rows = 4, mindur=0.1, maxdur=1, set = TRUE)

#run it with different settings
ad <- autodetec(threshold=10, env="abs", msmooth=c(900,90), power=1, 
                bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE, 
                wl = 300, ls = FALSE,  sxrow = 2, rows = 4, mindur=0.1, maxdur=1, set = TRUE)

#check this folder!!
getwd()


# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")

checkwavs()

#remove example directory
unlink(list.files())


data(list = c("Arre.aura", "Phae.cuvi","manualoc.df"))
writeWave(Arre.aura,"Arre.aura.wav") 
writeWave(Phae.cuvi,"Phae.cuvi.wav")

lspec(sxrow = 2, rows = 8, pal = reverse.heat.colors, ovlp = 99)
lspec(sxrow = 2, rows = 8, X = ad, 
      pal = reverse.heat.colors,ovlp = 99) #including selections




data(list = c("Arre.aura", "Phae.cuvi","manualoc.df"))
writeWave(Arre.aura, "Arre.aura.wav") #save sound files 
writeWave(Phae.cuvi, "Phae.cuvi.wav")

# make Arre.aura and Phae.cuvi spectrograms

specreator(manualoc.df, flim = c(0, 11),line = T, inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), picsize = 2, res = 300, cexlab = 2, mar = 0.05, parallel = 2)



write.

data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
writeWave(Phae.long1,"Phae.long1.wav")
writeWave(Phae.long2,"Phae.long2.wav")
writeWave(Phae.long3,"Phae.long3.wav")
writeWave(Phae.long4,"Phae.long4.wav")

manualoc(player=NULL)


dir.create(file.path(getwd(),"temp"))
setwd(file.path(getwd(),"temp"))
#' 
#' #Then download mp3 files from xeno-canto
#' querxc(qword = "Phaethornis aethopygus", download = TRUE)

querxc(qword = "Phaethornis aethopygus", download = TRUE)

library(warbleR)
setwd(file.path(getwd(),"temp"))

# Convert all files to .wav format
files <- list.files(path=getwd(), pattern = "mp3$", ignore.case = TRUE) #list .mp3 files in working directory


i<-files[1]
# for(i in files)
library(monitoR)
a<- monitoR::readMP3(i)




if(length(files) == 0) stop("no 'wav' files in working directory")
message("Start writing wav files:")
pbapply::pblapply(files, function(x) tuneR::writeWave(tuneR::readMP3(filename =  x),
                                                      paste(substr(x, 0, nchar(x) - 4), ".wav", sep="")))

unlink(list.files(pattern = ".jpeg$"))

system.time(
  dtwmat <- dfts(manualoc.df, length.out = 30, flim = c(1, 12), picsize = 2, res = 100, bp = c(2, 9),parallel = F, img = T)
            
  )


ffDTW(manualoc.df, length.out = 30, flim = c(1, 12), picsize = 2, res = 100, bp = c(0, 20),parallel = 1, img = T)
getwd()

library(dtw)

dd <- as.matrix(dtwmat[,3:ncol(dtwmat)])
dm <- dtwDist(dd, dd)       

rownames(dm) <- colnames(dm) <- paste(dtwmat$sound.files, dtwmat$selec, sep = "-")

system.time(b <- dfts2(X = manualoc.df, length.out = 30, flim = c(1, 12), picsize = 2, res = 100, bp = c(2, 9),parallel = F, img = T))


setwd(tempdir())

#load data
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4","manualoc.df"))
writeWave(Phae.long1, "Phae.long1.wav") #save sound files
writeWave(Phae.long2, "Phae.long2.wav")
writeWave(Phae.long3, "Phae.long3.wav")
writeWave(Phae.long4, "Phae.long4.wav")

system.time(xcor<-xcorr(X = manualoc.df, wl =300, frange= c(2, 9), ovlp=90, 
dens=1, wn='hanning', cor.method = "pearson", parallel = F) )

str(xcor)
system.time(a <- sig2noise(manualoc.df, mar = 0.2, parallel = F))

a
b
system.time(b <- sig2noise2(manualoc.df, mar = 0.2, parallel = F))


system.time(snrspecs(manualoc.df, flim = c(0, 14), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1), picsize = 2, res = 300, cexlab = 2, mar = 0.2, snrmar = 0.1, it = "jpeg", parallel = 1))

unlink(list.files(pattern = ".jpeg$"))

data(sim.coor.sing)

# testing if coordination happens less than expected by chance
system.time(coor.test(sim.coor.sing, iterations = 1000, less.than.chance = T, parallel = 2))

# testing if coordination happens more than expected by chance
coor.test(sim.coor.sing, iterations = 1000, less.than.chance = F)

X <- querxc("Phaethornis longirostris", download = FALSE)
View(X)
xcmaps(X, img = T, it = "tiff")

xcmaps(X, img = FALSE, it = "tiff")
dev.off()


## imp.raven####


setwd(tempdir())

data(selection.files)

write.table(selection.files[[1]],file = "100889-Garrulax monileger.selections.txt",
row.names = F, sep= "\t")

write.table(selection.files[[2]],file = "1023-Arremonops rufivirgatus.selections.txt",
row.names = F, sep= "\t")

#providing the name of the column with the sound file names
rav.dat<-imp.raven(sound.file.col = "End.File", all.data = FALSE)

View(rav.dat)

#getting all the data
rav.dat<-imp.raven(all.data = TRUE)
View(rav.dat)

unlink(list.files(pattern = ".txt$"))

write.table(selection.files[[3]],file = "harpyeagle.wav.txt",row.names = F,
 col.names = F, sep= "\t")

write.table(selection.files[[4]],file = "Phae.long4.wav.txt",row.names = F, 
col.names = F, sep= "\t")

syr.dat<-imp.syrinx(all.data = FALSE)

View(syr.dat)

#getting all the data
syr.dat<-imp.syrinx(all.data = TRUE)

View(rav.dat)

##compare.spec

X <- rbind(manualoc.df, manualoc.df)

X <- rbind(X, X)

X$selec <- 1:
  nrow(X)
X <- X[1:22,]

library(dtw)


setwd(tempdir())

unlink(list.files(pattern = ".tiff"))

source('~/MEGAsync/WarbleR package/current/R/compare.methods.R', echo=TRUE)

compare.methods(X= manualoc.df,flim = c(4, 20), bp = c(0, 2), mar = 0.1, wl = 512, ovlp = 90, res = 200, n = 1, length.out = 30, methods = c("dfDTW", "XCORR"), parallel = 1, it = "tiff")

system(paste("xdg-open",file.path(tempdir(), list.files(pattern = ".tiff$", full.names = F)[1])))


unlink(list.files(pattern = ".tiff"))
unlink(list.files(pattern = ".jpeg"))



unlink(list.files(pattern = ".png"))



writeWave(w32, "Cryp.soui.wav") #save sound files 

#autodetec location of signals
ad <- autodetec(threshold = 6, bp = c(1, 3), flim = c(0, 5), mindur = 1.2,
 maxdur = 3, sxrow = 5, rows = 3, ssmooth = 600, redo = T, parallel = 1)

#track frequency graphs
trackfreqs(X = ad, flim = c(0, 5), ovlp = 90, it = "tiff", bp = c(1, 3))

# Check this folder
getwd()




 data(sim.coor.sing)
 
 # testing if coordination happens less than expected by chance
 system.time(ct <- coor.test(sim.coor.sing, iterations = 100, less.than.chance = TRUE, parallel = 2))
 
 # testing if coordination happens more than expected by chance
 coor.test(sim.coor.sing, iterations = 1000, less.than.chance = FALSE)
#

ffts(manualoc.df, length.out = 30, flim = c(0, 11), bp = c(0, 11), parallel = 1, pal = reverse.terrain.colors)
 
 unlink(list.files(pattern = ".jpeg"))
 