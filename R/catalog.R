#' Create catalog of vocal signals
#' 
#' \code{catalog} produces spectrograms of selections (signals) split into multiple rows and columns.
#' @usage catalog(X, flim = c(0, 22), nrow = 4, ncol = 3, same.time.scale = TRUE, 
#' collev = seq(-40, 0, 1), ovlp = 50, parallel = 1, mar = 0.05, wl = 512, gr = FALSE, 
#' pal = reverse.gray.colors.2, it = "jpeg", path = NULL, pb = TRUE, fast = FALSE, 
#' res = 160, orientation = "v", labels = c("sound.files", "selec"), height = NULL, 
#' width = NULL, tags = NULL, tag.pal = NULL, legend = TRUE, cex = 1, leg.wd = 1)
#' @param X Data frame with columns for sound file name (sound.files), selection number (selec), 
#' and start and end time of signal (start and end). Default is \code{NULL}.
#' @param flim A numeric vector of length 2 indicating the highest and lowest 
#'   frequency limits (kHz) of the spectrogram, as in 
#'   \code{\link[seewave]{spectro}}. Default is c(0,22).
#' @param nrow A numeric vector of length 1. Specifies number of rows. Default is 4.
#' @param ncol A numeric vector of length 1.  Specifies number of columns. Default is 3.
#' @param same.time.scale Logical. Controls if all spectrograms are in the same time scale 
#' (i.e. have the same duration).
#' @param collev A numeric vector of length 3. Specifies levels to partition the 
#'   amplitude range of the spectrogram (in dB). The more levels the higher the
#'   resolution of the spectrogram. Default is seq(-40, 0, 1). seq(-115, 0, 1) will produces spectrograms
#'   similar to other acoustic analysis software packages. 
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two 
#'   consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 50. High values of ovlp 
#'   slow down the function but produce more accurate selection limits (when X is provided). 
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param mar Numeric vector of length 1. Specifies the margins adjacent to the start and end points of selections,
#' dealineating spectrogram limits. Default is 0.05.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram, default 
#'   is 512.
#' @param gr Logical argument to add grid to spectrogram. Default is \code{FALSE}.
#' @param pal Color palette function for spectrogram. Default is reverse.gray.colors.2. See 
#' \code{\link[seewave]{spectro}} for more palettes. Palettes as \code{\link[monitoR]{gray.2}} may work better when fast = T.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @param fast Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}} (which are already imported) seem
#' to work better with 'fast' spectograms. Palette colors  \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}} offer 
#' decreasing darkness levels. THIS IS STILL BEING TESTED.
#' @param res Numeric argument of length 1. Controls image resolution.
#'   Default is 100 (faster) although 300 - 400 is recommended for publication/ 
#'   presentation quality.
#' @param  orientation String. Indicates whether a letter page size image is produced in vertical ('v' option) or
#' horizontal orientation ('h' option). Note that width and height can also be specified.
#' @param labels String vector. Provides the column names that will be used as labels above the corresponding spectrograms. 
#' @param height Numeric. Single value (in inches) indicating the height of the output image files.
#' @param width Numeric. Single value (in inches) indicating the width of the output image files.
#' @param tags String vector. Provides the column names that will be used for the color tagging legend above
#'  spectrograms. 
#' @param tag.pal Color palette function for tags.
#' @param legend Logical. Controls if tag legend is plotted at the right side of the catalog. Default is 
#' \code{TRUE}. Ignored if no tags are provided. 
#' @param cex A numeric vector of length 1 giving the amount by which text 
#'   (including labels and axis) should be magnified. Default is 1. 
#' @param leg.wd Numeric. Controls the width of the legend column. Default is 1.
#' @return image files with spectrograms of whole sound files in the working directory. Multiple pages
#' can be returned, depending on the length of each sound file. 
#' @export
#' @name catalog
#' @details This functions aims to simplify the visual exploration of multiple vocalizations. The function plots a
#'  matrix of spectrograms from a selection table. Spectrograms can be labeled or color tagged to facilitate
#'   exploring variation related to a parameter of interest (e.g. location, song type).
#' @examples
#' \dontrun{
#' # Set temporary working directory
#' setwd(tempdir())
#' # save sound file examples
#' data(list = c("Phae.long1", "Phae.long2","selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav") 
#' writeWave(Phae.long2,"Phae.long2.wav")
#'  writeWave(Phae.long3,"Phae.long3.wav")
#'  writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' 
#' catalog(X = selec.table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = T,
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE, pal = gray.3, 
#'  orientation = "v",  labels = c("sound.files", "selec"), legend = T, 
#'  width = 20, collev = seq(-65, 0, 5))
#'  
#'  #different time scales and tag palette
#' catalog(X = selec.table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = F,
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE, pal = gray.3, 
#'  orientation = "v",  labels = c("sound.files", "selec"), legend = T, 
#'  width = 20, collev = seq(-65, 0, 5), tag.pal = terrain.colors)
#'  #'  
#'  #adding tags
#' catalog(X = selec.table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = F,
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE, pal = gray.3, 
#'  orientation = "v",  labels = c("sound.files", "selec"), legend = T, 
#'  width = 20, collev = seq(-65, 0, 5), tag.pal = terrain.colors, tags = "sound.files")
#' 
#'  #create a bigger selection table
#'  X <- rbind(selec.table, selec.table, selec.table, selec.table)
#'  X <- rbind(X, X)
#'  
#'  #create some simulated labels
#'  X$songtype <- sample(letters[1:3], nrow(X), replace = T)
#'  X$indiv <- sample(letters[1:12], nrow(X), replace = T)
#' 
#' # 12 columns in 5 rows, 2 tags
#' catalog(X = selec.table, flim = c(1, 10), nrow = 5, ncol = 12, same.time.scale = F,
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE, pal = gray.3, 
#'  orientation = "v",  labels = c("sound.files", "selec"), legend = F, 
#'  width = 20, collev = seq(-65, 0, 5), tag.pal = terrain.colors, 
#'  tags = c("songtype", "indiv"))
#' 
#'
#' # with legend
#' catalog(X = selec.table, flim = c(1, 10), nrow = 5, ncol = 12, same.time.scale = F,
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE, pal = gray.3, 
#'  orientation = "v",  labels = c("sound.files", "selec"), legend = T, 
#'  width = 20, collev = seq(-65, 0, 5), tag.pal = terrain.colors,
#'   tags = c("songtype", "indiv"))
#'   
#' check this floder
#' getwd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on mar-12-2017 (MAS)

catalog <- function(X, flim = c(0, 22), nrow = 4, ncol = 3, same.time.scale = TRUE, collev = seq(-40, 0, 1), 
                    ovlp = 50, parallel = 1, mar = 0.05, wl = 512, gr = FALSE, pal = reverse.gray.colors.2, 
                    it = "jpeg", path = NULL, pb = TRUE, fast = FALSE, res = 160, orientation = "v", 
                    labels = c("sound.files", "selec"), height = NULL, width = NULL, tags = NULL, 
                    tag.pal = NULL, legend = TRUE, cex = 1, leg.wd = 1)
{
  #check path to working directory
  if(!is.null(path))
  {wd <- getwd()
  if(class(try(setwd(path), silent = TRUE)) == "try-error") stop("'path' provided does not exist") else
    setwd(path)} #set working directory

  #nrow must be equal or higher than 2
  if(nrow < 2) stop("number of rows must be equal or higher than 2")

  #rows must be equal or higher than 2
  if(ncol < 1) stop("number of columns (ncol) must be equal or higher than 1")

  #read files
  files <- list.files(pattern = "\\.wav$", ignore.case = TRUE)


  #stop if files are not in working directory
  if(length(files) == 0) stop("no .wav files in working directory")

  #missing columns
  if(!all(c("sound.files", "selec",
            "start", "end") %in% colnames(X)))
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec",
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))

  #tag.pal must be a color function
  if(!is.function(tag.pal)) stop("'tag.pal' must be a color palette function")

  #pal must be a color function
  if(!is.function(pal)) stop("'pal' must be a color palette function")
  
    # orientation
  if(!orientation %in% c("v", "v")) stop("orientation should be either
                                                         'v' or 'v'")
  
  #missing label columns
  if(!all(labels %in% colnames(X)))
    stop(paste(paste(labels[!(labels %in% colnames(X))], collapse=", "), "label column(s) not found in data frame"))
  
  #if tags> 2
  if(length(tags) > 2) stop("No more than 2 tags can be used at a time")
  
    #missing tag columns
  if(!all(tags %in% colnames(X)))
    stop(paste(paste(tags[!(tags %in% colnames(X))], collapse=", "), "tag column(s) not found in data frame"))

  #if sel.comment column not found create it
  if(is.null(X$sel.comment) & !is.null(X)) X <- data.frame(X,sel.comment="")


  #if there are NAs in start or end stop
  if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")

  #if end or start are not numeric stop
  if(all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'end' and 'selec' must be numeric")

  #if any start higher than end stop
  if(any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))

  #if it argument is not "jpeg" or "tiff"
  if(!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))

  #wrap img creating function
  if(it == "jpeg") imgfun <- jpeg else imgfun <- tiff

  #return warning if not all sound files were found
  recs.wd <- list.files(pattern = "\\.wav$", ignore.case = TRUE)
  if(length(unique(X$sound.files[(X$sound.files %in% recs.wd)])) != length(unique(X$sound.files)))
    (paste(length(unique(X$sound.files))-length(unique(X$sound.files[(X$sound.files %in% recs.wd)])),
           ".wav file(s) not found"))

  #count number of sound files in working directory and if 0 stop
  d <- which(X$sound.files %in% recs.wd)
  if(length(d) == 0){
    stop("The .wav files are not in the working directory")
  }  else {
    X <- X[d, ]
  }

  #if flim is not vector or length!=2 stop
  if(is.null(flim)) stop("'flim' must be a numeric vector of length 2") else {
    if(!is.vector(flim)) stop("'flim' must be a numeric vector of length 2") else{
      if(!length(flim) == 2) stop("'flim' must be a numeric vector of length 2")}}

  #if wl is not vector or length!=1 stop
  if(is.null(wl)) stop("'wl' must be a numeric vector of length 1") else {
    if(!is.vector(wl)) stop("'wl' must be a numeric vector of length 1") else{
      if(!length(wl) == 1) stop("'wl' must be a numeric vector of length 1")}}

  #if rows is not vector or length!=1 stop
  if(is.null(nrow)) stop("'nrow' must be a numeric vector of length 1") else {
    if(!is.vector(nrow)) stop("'nrow' must be a numeric vector of length 1") else{
      if(!length(nrow) == 1) stop("'nrow' must be a numeric vector of length 1")}}

  #if ncol is not vector or length!=1 stop
  if(is.null(ncol)) stop("'ncol' must be a numeric vector of length 1") else {
    if(!is.vector(ncol)) stop("'ncol' must be a numeric vector of length 1") else{
      if(!length(ncol) == 1) stop("'ncol' must be a numeric vector of length 1")}}

  #set dimensions
  if(is.null(width))
  {if(orientation == "v")   width <- 8.5 else width <- 11}
  
  if(is.null(height))
  {if(orientation == "v")   height <- 11 else height <- 8.5}
    
  
  #box cols
  if(!is.null(tags))
    {
    if(length(tags) == 2)  
    { 
      #convert to character
      Y <- rapply(X, as.character, classes="factor", how="replace")
      n <- length(unique(Y[, tags[1]])) + length(unique(Y[, tags[2]]))
      } else {
        n <- length(unique(X[, tags]))
      }
    if(is.null(tag.pal))
      boxcols <- hcl(h = seq(15, 375, length =  n + 1), l = 65, c = 100)[1 : n] else
      boxcols <- tag.pal(n)
      
  #convert characters to factors
  X <- rapply(X, as.factor, classes="character", how="replace")
  X$col1 <- X[,tags[1]] 
  levels(X$col1) <- boxcols[1:length(unique(X$col1))]
  
  #add to df for legend
  tag.col.df <- X[!duplicated(X[,tags[1]]), c(tags[1], "col1")]
  tag.col.df$tag.col <- tags[1]
  names(tag.col.df) <- c("tag", "col", "tag.col")
  
  if(length(tags) == 2) 
    {
    X$col2 <- X[,tags[2]] 
    X$col2 <- as.factor(X$col2)
    levels(X$col2) <- boxcols[(length(unique(X$col1))+1):length(boxcols)]
  
    #add to df for legend
    W <- X[!duplicated(X[,tags[2]]), c(tags[2], "col2")]
    W$tag.col <- tags[2]
    names(W) <- c("tag", "col", "tag.col")
    W$tag <- as.character(W$tag)
    
    tag.col.df <- rbind(tag.col.df, W)
     }
  tag.col.df <- rapply(tag.col.df, as.character, classes="factor", how="replace")
  
  } else legend <- FALSE
  
    #calculate time and freq ranges based on all recs
if(same.time.scale)
    {
  rangs <- lapply(1:nrow(X), function(i){
    r <- tuneR::readWave(as.character(X$sound.files[i]), header = TRUE)
    f <- r$sample.rate
    t <- c(X$start[i] - mar, X$end[i] + mar)

    if (t[1] < 0) t[1] <- 0

    if(t[2] > r$samples/f) t[2] <- r$samples/f

    #in case flim its higher than can be due to sampling rate
    fl<- flim
    if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1
      return(data.frame(fl1 = fl[1], fl2 = fl[2], mardur = t[2] - t[1]))
  })

rangs <- do.call(rbind, rangs)

X2 <- lapply(1:nrow(X), function(x)
  {
  Y <- X[x, ]
  dur <- Y$end - Y$start
  if(dur < max(rangs$mardur)) {
    Y$end  <- Y$end + (max(rangs$mardur) - dur)/2
    Y$start  <- Y$start - (max(rangs$mardur) - dur)/2
    }
return(Y)
})


X <- do.call(rbind, X2)
} 

    
catalFUN <- function(X, nrow, ncol, page, labels, grid, fast, flim, wl, ovlp, pal, width, height, tag.col.df, legend, cex)
{
#set layout for screensplit
#rows
if(is.null(tags))
  rws <- rep(c(5, nrow / 8), nrow) else   rws <- rep(c(5, nrow / 4), nrow)

#last row thicker
if(same.time.scale) rws[1] <- 6.5
rws <- c(nrow / 8, rws)

#make last one thicker to fit axis
csrws <- cumsum(rws)
rws <- csrws/max(csrws)
minrws <- min(rws)
tp <- sort(rws[-1], decreasing = TRUE)
tp <- rep(tp, each = ncol + 1)
btm <- c(sort(rws[-length(rws)], decreasing = TRUE))
btm <- rep(btm, each = ncol + 1)

#columns
lfcol.width <- 0.1
if(ncol > 1)
{
  spectroclms <- c(lfcol.width, 0.007 * ncol, rep(1, ncol))
csclms <- cumsum(spectroclms)
cls <- csclms/max(csclms)
lf <- c(0, cls[-length(cls)])
rgh <- cls
} else { 
  lf <- c(0, lfcol.width)
  rgh <- c(lfcol.width, 1)
}

lf <- lf[-1]
rgh <- rgh[-1]

#duplicate for label box and spectro
lf <- rep(lf, length(btm)/(ncol + 1))
rgh <- rep(rgh, length(btm)/(ncol + 1))

#put them together
m <- cbind(lf, rgh,  btm, tp)
m <- m[order(m[,1], -m[,4]),]
m <- m[c(((nrow * 2) + 1):((ncol + 1) * nrow * 2), 1:(nrow * 2)), ]

#add left col for freq axis
#set parameters used to pick up spectros with freq axis
minlf <- sort(unique(m[,1]))[2]
minbtm <- min(m[,3])
m <- rbind(m, c(0, min(m[,1]), 0, 1))

#add bottom row for time axis
m <- rbind(m, c(0, 1, 0, minbtm))

#add legend col
if(legend)
{
  leg.wd <- 1.08 + leg.wd/100
  m <- rbind(m[1:(nrow(m)-2), ], c(1, leg.wd, 0, 1), m[(nrow(m)-1):nrow(m), ])
  m[,1] <- m[,1]/leg.wd
  m[,2] <- m[,2]/leg.wd

  #reset parameters used to pick up spectros with freq axis
  minlf <- minlf/leg.wd
    }

X3 <- X[rep(1:nrow(X), each = 2), ]

#convert factors to character
X3 <- rapply(X3, as.character, classes="factor", how="replace")

#start graphic device
imgfun(filename = paste0("Catalog_p", page, ".", it), units = "in", width = width, height = height, res = res)
invisible(close.screen(all.screens = TRUE))
split.screen(figs = m)

#testing layout screens
# for(i in 1:nrow(m))
# {screen(i)
#   par( mar = rep(0, 4))
#   plot(0.5, xlim = c(0,1), ylim = c(0,1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
#   box()
#   text(x = 0.5, y = 0.5, labels = i)
# }
# close.screen(all.screens = T)

#plot 
lapply(1:nrow(m), function(i)
                  {
    # i = 1
    screen(i)           
    if(i <= nrow(X3))
    {  
      if(i %% 2 == 0) #plot spectros
{     #Read sound files, initialize frequency and time limits for spectrogram
        r <- tuneR::readWave(as.character(X3$sound.files[i]), header = TRUE)
      f <- r$sample.rate
      t <- c(X3$start[i] - mar, X3$end[i] + mar)

  if (t[1] < 0) t[1] <- 0

  if(t[2] > r$samples/f) t[2] <- r$samples/f
  
  fl<- flim #in case flim its higher than can be due to sampling rate
  if(fl[2] > ceiling(f/2000) - 1) fl[2] <- ceiling(f/2000) - 1 
  
  rec <- tuneR::readWave(as.character(X3$sound.files[i]), from = t[1], to = t[2], units = "seconds")

    #add xaxis to bottom spectros
  if(i %in% which(m[,3] == minbtm) | !same.time.scale) {
    axisX = TRUE
    btm = 2.6
  } else {
    axisX = FALSE
    btm = 0
  } 
  
  #add y axis to first column
  # if(i %in% which(m[,1] == minlf)) {
  #   axisY = TRUE
  #   lft = 2
  # } else {
  #   axisY = FALSE
  #   lft = 0
  # }
  # 
  par(mar = c(btm, 0, 0, 0))
  
  spectro2(wave = rec, f = rec@samp.rate, flim = fl, wl = wl, ovlp = ovlp, axisX = axisX, axisY = FALSE, tlab = NULL, flab = NULL, palette = pal, fast = fast, main = NULL, grid = gr, page = page, rm.zero = TRUE, cexlab = cex, collevels = collev)

} else { #plot labels
  
  par( mar = rep(0, 4))

    plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  
  #color boxes
  if(!is.null(tags))
  {
    # if(i %in% which(m[,1] == minlf)) {
    #   cutbox1 <- predict(object = smooth.spline(c(0.343, 0.077, 0.205, 0.142, 0.26) - 0.01 ~ c(12, 2, 6, 4, 8), df = 5), x = ncol)$y
    # 
    #   cutbox2 <- 0.5 + (cutbox1 / 2)
    # } else  {
        cutbox1 <- 0
        cutbox2 <- 0.5
        # }
    
    lim <- par("usr")
    if(length(tags) == 1)
    rect(xleft = lim[1] + cutbox1, ybottom = lim[3]-1, xright = lim[2], ytop = 0.5, border = "black", col = X3$col1[i]) else {
      rect(xleft = lim[1] + cutbox1, ybottom = lim[3]-1, xright = cutbox2, ytop = 0.5, border = "black", col = X3$col1[i])
      rect(xleft = cutbox2, ybottom = lim[3]-1, xright = lim[2], ytop = 0.5, border = "black", col = X3$col2[i])
      
    }
    
    #plot labels
    text(x = 0.5, y = 0.8, labels = paste(X3[i, labels], collapse = " "), 
         cex = (ncol * nrow * 1.5 * cex)/((ncol * nrow)^1.2))
    } else
             text(x = 0.5, y = 0.5, labels = paste(X3[i, labels], collapse = " "), 
                  cex = (ncol * nrow * 2 * cex)/((ncol * nrow)^1.2))
           }
    }  
  
  #add freq ticks
  if(i %in% ((ncol * nrow * 2) + 1):((ncol * nrow * 2) + (nrow * 2)) & i %% 2 == 0 &  (i - (ncol * nrow *2)) <= nrow(X3))
  {
    par( mar = rep(0, 4))
    plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")  
   
    ylab <- pretty(flim, 5)
    ylab <- ylab[ylab < flim[2] & ylab > flim[1]]
    ys <- ylab/flim[2]
    
    usr <- par("usr")
    if(i %in% which(m[,3] == minbtm) | !same.time.scale) usr[1] <- 0.11

    ys <- usr[1] + (usr[2] - usr[1]) * ys
  
    out <- lapply(1:length(ys), 
                  function(w) 
                  {
                    lines(x = c(0.9, 1.04), y = c(ys[w], ys[w]))
                    text(x = 0.25, y = ys[w], labels = ylab[w], cex = cex)
                  }
    )
  }
    
  #add Freq axis label
  if(i == nrow(m) - 1)
  {
    par(mar = c(0, 0, 0, 0))
    plot(1, col = "white", frame.plot = FALSE)
    text(x = 1, y = 1.05, "Frequency (kHz)", srt = 90, cex = 1.2 * cex) 
  }
   
  #add time axis label
  if(i == nrow(m))
  {
    par(mar = c(0, 0, 0, 0))
    plot(1, col = "white", frame.plot = FALSE)
    text(x = 1, y = 1.05, "Time (s)", cex = 1.2 * cex) 
  }

  #add legend 
  if(legend & i == nrow(m) - 2)
  {
    par( mar = rep(0, 4))
    plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")  
    
    y <- seq(0.2, 0.8, length.out = nrow(tag.col.df) + length(unique(tag.col.df$tag.col)))
    y <- y[length(y):1]
    step <-  y[1] - y[2]
    
    text(x = 0.5, y = max(y) + step, labels = tag.col.df$tag.col[1], cex = cex, font = 2) 
  
    
    out <- lapply(which(tag.col.df$tag.col == unique(tag.col.df$tag.col)[1]), function(w)
    {
      # plot label
      text(x = 0.5, y = y[w], labels = tag.col.df$tag[w], cex = cex) 
      
      #plot color box
      rect(xleft = 0.3, ybottom = y[w] - (step/2) - (step/6), xright = 0.7, ytop = y[w] - (step/2) + (step/6), border = "black", col = tag.col.df$col[w])
      })
    
    if(length(unique(tag.col.df$tag.col)) == 2)
    {
      text(x = 0.5, y = y[max(which(tag.col.df$tag.col == unique(tag.col.df$tag.col)[1])) + 2], labels = tag.col.df$tag.col[nrow(tag.col.df)], cex = cex, font = 2) 
      
      y <- y - step * 2

      out <- lapply(which(tag.col.df$tag.col == unique(tag.col.df$tag.col)[2]), function(w)
      {
        # plot label
        text(x = 0.5, y = y[w], labels = tag.col.df$tag[w], cex = cex) 
        
        #plot color box
        rect(xleft = 0.3, ybottom = y[w] - (step/2) - (step/6), xright = 0.7, ytop = y[w] - (step/2) + (step/6), border = "black", col = tag.col.df$col[w])
      })
      
    }
    
    
  }
  i = i + 1
   })
  close.screen(all.screens = TRUE)
dev.off()
}  

#run function over X
cel <- ceiling((nrow(X)/(ncol * nrow)))
if(cel < 1)
  Xlist <- list(X) else
    Xlist <- lapply(1:cel, function(x) 
      {
      if(x < cel)
      X[((((ncol * nrow) * (x - 1)) + 1):((ncol * nrow) * (x))), ] else
        X[((((ncol * nrow) * (x - 1)) + 1):nrow(X)), ]
      })

 #Apply over each sound file
 # Run parallel in windows
 if(parallel > 1) {
   if(Sys.info()[1] == "Windows") {

     z <- NULL

     cl <- parallel::makeCluster(parallel)

     doParallel::registerDoParallel(cl)

     out <- foreach::foreach(z = 1:length(Xlist)) %dopar% {
       catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast, flim, wl, ovlp, pal, 
                width, height, tag.col.df, legend, cex)

     parallel::stopCluster(cl)

     }
   }
   if(Sys.info()[1] == "Linux") {    # Run parallel in Linux

     out <- parallel::mclapply(1:length(Xlist), function (z) {
       catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast, flim, wl, ovlp, pal, 
                width, height, tag.col.df, legend, cex)
            })
   }
   if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
   {
     cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))

     doParallel::registerDoParallel(cl)

     out <- foreach::foreach(z = 1:length(Xlist)) %dopar% {
       catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast, flim, wl, ovlp, pal, 
                width, height, tag.col.df, legend, cex)
     }

     parallel::stopCluster(cl)

   }
 }
 else {
   if(pb)
     out <- pbapply::pblapply(1:length(Xlist), function(z)
       catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast, flim, wl, ovlp, pal, 
                width, height, tag.col.df, legend, cex))  else
         out <- lapply(1:length(Xlist), function(z)
           catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast, flim, wl, ovlp, pal, 
                    width, height, tag.col.df, legend, cex))
 }

if(!is.null(path)) setwd(wd)
}
  
  