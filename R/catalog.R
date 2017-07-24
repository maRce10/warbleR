#' Create catalog of vocal signals
#' 
#' \code{catalog} produces spectrograms of selections (signals) split into multiple rows and columns.
#' @usage catalog(X, flim = c(0, 22), nrow = 4, ncol = 3, same.time.scale = TRUE, 
#' collev = seq(-40, 0, 1), ovlp = 50, parallel = 1, mar = 0.05, wl = 512, gr = FALSE, 
#' pal = reverse.gray.colors.2, it = "jpeg", path = NULL, pb = TRUE, fast.spec = FALSE, 
#' res = 100, orientation = "v", labels = c("sound.files", "selec"), height = NULL, 
#' width = NULL, tags = NULL, tag.pal = list(temp.colors, heat.colors, topo.colors), 
#' legend = 3, cex = 1, leg.wd = 1, img.suffix = NULL, tag.widths = c(1, 1), hatching = 0, 
#' breaks = c(5, 5), group.tag = NULL, spec.mar = 0, spec.bg = "white", 
#' max.group.cols = NULL)
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
#' \code{\link[seewave]{spectro}} for more palettes. Palettes as \code{\link[monitoR]{gray.2}} may work better when \code{fast.spec = TRUE}.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Note that progress bar is only used
#' when parallel = 1.
#' @param fast.spec Logical. If \code{TRUE} then image function is used internally to create spectrograms, which substantially 
#' increases performance (much faster), although some options become unavailable, as collevels, and sc (amplitude scale).
#' This option is indicated for signals with high background noise levels. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}}, \code{\link[monitoR]{topo.1}} and \code{\link[monitoR]{rainbow.1}} (which should be imported from the package monitoR) seem
#' to work better with 'fast.spec' spectograms. Palette colors \code{\link[monitoR]{gray.1}}, \code{\link[monitoR]{gray.2}}, 
#' \code{\link[monitoR]{gray.3}} offer 
#' decreasing darkness levels. THIS IS STILL BEING TESTED.
#' @param res Numeric argument of length 1. Controls image resolution. Default is 100 (faster)
#'  although 300 is recommended for publication/presentation quality. Note that high resolution
#'   produce significantly bigger image files. This could be problematic when creating pdf files
#'   using \code{\link{catalog}}. 
#' @param  orientation String. Indicates whether a letter page size image is produced in vertical ('v' option) or
#' horizontal orientation ('h' option). Note that width and height can also be specified.
#' @param labels String vector. Provides the column names that will be used as labels above the corresponding spectrograms. 
#' @param height Numeric. Single value (in inches) indicating the height of the output image files. Default is 11 
#' for vertical orientation.
#' @param width Numeric. Single value (in inches) indicating the width of the output image files.  Default is 8.5
#'  for vertical orientation.
#' @param tags String vector. Provides the column names that will be used for the color tagging legend above. Tags can also be numeric. Continuous variables would be break down in 10 color classes.
#'  spectrograms. 
#' @param tag.pal List of color palette function for tags. Should be of length 1, 2.  Default is \code{list(temp.colors, heat.colors, topo.colors)}.
#' @param legend A numeric vector of length 1 controling a legend for color tags is added.
#' Ignored if no tags are provided. Four values are allowed: 
#' \itemize{
#'    \item \code{0}: No label
#'    \item \code{1}: Label for the first color tag
#'    \item \code{2}: Label for the second color tag
#'    \item \code{3}: Labels both color tags
#'    }
#'  Default is 3.
#' @param cex A numeric vector of length 1 giving the amount by which text 
#'   (including labels and axis) should be magnified. Default is 1. 
#' @param leg.wd Numeric. Controls the width of the legend column. Default is 1.
#' @param img.suffix A character vector of length 1 with a suffix (label) to add at the end of the names of 
#' image files. Default is \code{NULL} (no suffix). Can be useful to label catalogs from different individuals, 
#' species or sites.
#' @param tag.widths A numeric vector of length 2 to control de relative width of the color tags (when 2 tags are provided).
#' @param hatching A numeric vector of length 1 controling cross-hatching is used for color tags. Several cross-hatching 
#' patterns are used to make tags with similar colors more distinguishable. Four values are allowed: 
#' \itemize{
#'    \item \code{0}: No cross-hatching
#'    \item \code{1}: Cross-hatching the first color tag
#'    \item \code{2}: Cross-hatching the second color tag
#'    \item \code{3}: Cross-hatching both color tags
#'    }
#' @param breaks Numeric vector of length 1 or 2 controling the number of intervals in which a 
#' numeric tag will be divided. The numbers control the first and second tags respectively. 
#' Ignored if tags are not numeric. Default is c(5, 5). 
#' @param group.tag Character vector of length 1 indicating the column name to be used to color
#' the empty plot areas around the spectrograms. If provided selections that belong to the same
#' tag level are clumped together in the catalog (the 'X' data frame is sorted by that column).
#' This tags cannot be included in the legend so it would be better to use the label field to identify the different levels.
#' @param spec.mar Numeric vector of length 1 to add space at the top, left and right sides of
#'  the spectrogram. Useful to better display the grouping of selections when 'group.tag' is 
#'  provided. Internally applied for setting 'mar' using \code{\link[graphics]{par}}.
#' @param spec.bg Character vector of length 1 to control the background color of the spectrogram. Default is 'white'. Ignored if \code{group.tag = NULL}. 
#' @param max.group.cols Numeric vector of length 1 indicating the number of different colors 
#' that will be used for group tags (see 'group.tag' argument). If provided (and the number is 
#' smaller than the number of levels in the 'group.tag' column) the colors would be recycled, 
#' athough ensuring that adjacent groups do not share the same color. Useful when the 
#' 'group.tag' has many levels and the colors assigned become very similar. Default is \code{NULL}.
#' @return Image files with spectrograms of whole sound files in the working directory. Multiple pages
#' can be returned, depending on the length of each sound file. 
#' @export
#' @name catalog
#' @details This functions aims to simplify the visual exploration of multiple vocalizations. The function plots a
#'  matrix of spectrograms from a selection table. Spectrograms can be labeled or color tagged to facilitate
#'   exploring variation related to a parameter of interest (e.g. location, song type). A legend will be added to 
#'   help match colors with tag levels (if legend is > 0). Different color palettes can
#'   be used for each tag. Numeric tags are split in intervals (the number of intervals can be
#'    controlled with break argument). The width and height can also be adjusted to fit more column and/or rows.
#'   This files can be put together in a single pdf file with \code{\link{catalog2pdf}}.
#'   We recommend using low resolution (~60-100) and smaller dimensions (width & height < 10) if
#'   aiming to generate pdfs (otherwise pdfs could be pretty big).
#' @seealso \url{https://marce10.github.io/2017-03-17-Creating_song_catalogs/}
#' \code{\link{catalog2pdf}}
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
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE,
#'  orientation = "v",  labels = c("sound.files", "selec"), legend = 0)
#'  
#'  #different time scales and tag palette
#' catalog(X = selec.table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = F,
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, 
#'  orientation = "v",  labels = c("sound.files", "selec"), legend = 0, 
#'  tag.pal = list(terrain.colors))
#'  
#'  #adding tags and changing spectro palette
#' catalog(X = selec.table, flim = c(1, 10), nrow = 4, ncol = 2, same.time.scale = F,
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, pal = reverse.heat.colors,
#'  orientation = "v",  labels = c("sound.files", "selec"), legend = 1, 
#'  tag.pal = list(terrain.colors), tags = "sound.files")
#' 
#'  #create a bigger selection table
#'  X <- rbind(selec.table, selec.table, selec.table, selec.table)
#'  X <- rbind(X, X)
#'  
#'  #create some simulated labels
#'  X$songtype <- sample(letters[13:15], nrow(X), replace = T)
#'  X$indiv <- sample(letters[1:12], nrow(X), replace = T)
#' 
#' # 12 columns in 5 rows, 2 tags
#' catalog(X = X, flim = c(1, 10), nrow = 5, ncol = 12, same.time.scale = F,
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, 
#'  orientation = "v",  labels = c("sound.files", "selec"), legend = 3, 
#'  collev = seq(-65, 0, 5), tag.pal = list(terrain.colors), tags = c("songtype", "indiv"))
#' 
#'
#' # with legend
#' catalog(X = X, flim = c(1, 10), nrow = 5, ncol = 12, same.time.scale = F,
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE,
#'  orientation = "v",  labels = c("sound.files", "selec"), legend = 3, 
#'  width = 20, collev = seq(-65, 0, 5), tag.pal = list(terrain.colors),
#'   tags = c("songtype", "indiv"))
#'   
#'   # horizontal orientation
#' catalog(X = X, flim = c(1, 10), nrow = 5, ncol = 12, same.time.scale = F,
#'  ovlp = 90, parallel = 1, mar = 0.01, wl = 200, gr = FALSE,
#'  orientation = "h",  labels = c("sound.files", "selec"), legend = 3, 
#'  width = 20, collev = seq(-65, 0, 5), tag.pal = list(terrain.colors),
#'   tags = c("songtype", "indiv"))
#' check this floder
#' getwd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on mar-12-2017 (MAS)

catalog <- function(X, flim = c(0, 22), nrow = 4, ncol = 3, same.time.scale = TRUE, collev = seq(-40, 0, 1), 
                    ovlp = 50, parallel = 1, mar = 0.05, wl = 512, gr = FALSE, pal = reverse.gray.colors.2, 
                    it = "jpeg", path = NULL, pb = TRUE, fast.spec = FALSE, res = 100, orientation = "v", 
                    labels = c("sound.files", "selec"), height = NULL, width = NULL, tags = NULL, 
                    tag.pal = list(temp.colors, heat.colors, topo.colors), legend = 3, cex = 1, leg.wd = 1, 
                    img.suffix = NULL, tag.widths = c(1, 1), hatching = 0, breaks = c(5, 5), 
                    group.tag = NULL, spec.mar = 0, spec.bg = "white", max.group.cols = NULL)
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
  if(!is.list(tag.pal) & !is.null(tag.pal)) stop("'tag.pal' must be a list of color palette functions of length 1 or 2")

  if(length(tag.pal) == 1) tag.pal[[2]] <- tag.pal[[1]]
  if(length(tag.pal) == 2 & !is.null(group.tag)) tag.pal[[3]] <- tag.pal[[1]]
  
  if(!is.null(max.group.cols) & length(tag.pal) == 3) {fc <- tag.pal[[3]]
  tag.pal[[3]] <- function(n) rep(fc(max.group.cols), ceiling(10/3))[1:n]}
  
  if(length(breaks) == 1) breaks[2] <- breaks[1]
    
  #pal must be a color function
  if(!is.function(pal)) stop("'pal' must be a color palette function")
  
    # orientation
  if(!orientation %in% c("v", "h")) stop("orientation should be either
                                                         'v' or 'h'")
  
  #missing label columns
  if(!all(labels %in% colnames(X)))
    stop(paste(paste(labels[!(labels %in% colnames(X))], collapse=", "), "label column(s) not found in data frame"))
  
  #if tags> 2
  if(length(tags) > 2) stop("No more than 2 tags can be used at a time")
  
    #missing tag columns
  if(!all(tags %in% colnames(X)))
    stop(paste(paste(tags[!(tags %in% colnames(X))], collapse=", "), "tag column(s) not found in data frame"))

  #missing tag columns
  if(!all(tags %in% colnames(X)))
    stop(paste(paste(tags[!(tags %in% colnames(X))], collapse=", "), "tag column(s) not found in data frame"))
  
  if(!is.null(group.tag))
    {if(!group.tag %in% colnames(X))
      stop("group.tag column not found in data frame") else
        X <- X[order(X[, group.tag]),]
      
      if(is.numeric(X[, group.tag]))
        stop("group tag should cannot be numeric")
        }
  
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
  
  #if parallel and pb in windows
  if(parallel > 1 &  pb & Sys.info()[1] == "Windows") {
    message("parallel with progress bar is currently not available for windows OS")
    message("running parallel without progress bar")
    pb <- FALSE
  } 

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

  # if levels are shared between tags
  if(length(tags) == 2) if(any(unique(X[ ,tags[1]]) %in% unique(X[ ,tags[2]]))) stop("Tags cannot contained levels with the same labels")
  
  #legend
  if(!is.numeric(legend) | legend < 0 | legend > 3)
    stop("legend should be be a value between 0 and 3")

  #hatching
  if(!is.numeric(hatching) | hatching < 0 | hatching > 3)
    stop("hatching should be be a value between 0 and 3")

  #legend
  if(!is.numeric(legend) | legend < 0 | legend > 3)
    stop("legend should be be a value between 0 and 3")
    
  #set dimensions
  if(is.null(width))
  {if(orientation == "v")   width <- 8.5 else width <- 11}
  
  if(is.null(height))
  {if(orientation == "h")   height <- 8.5 else height <- 11}
  
  #fix hatching based on tags
  if(length(tags) == 1 & hatching == 2) hatching <- 0 
  if(length(tags) == 1 & hatching == 3) hatching <- 1 
  if(is.null(tags)) hatching <- 0 
  
  #box colors
  if(!is.null(tags))
    {
   
    if(length(tags) == 1 & legend == 2) legend <- 0
    
     #convert to character
    Y <- as.data.frame(rapply(X, as.character, classes="factor", how="replace"))
    
    #if tag is numeric
    if(is.numeric(X[, tags[1]])) 
    {
      if(is.integer(X[, tags[1]]))  
{
        if( length(unique(X[, tags[1]])) > 1)
        boxcols <- tag.pal[[1]](length(unique(X[, tags[1]])))[as.numeric(cut(X[, tags[1]],breaks = length(unique(X[, tags[1]]))))] else boxcols <- tag.pal[[1]](1)
} else  
          boxcols <- tag.pal[[1]](breaks[1])
    }      else   boxcols <- tag.pal[[1]](length(unique(Y[, tags[1]]))) 

    if(length(tags) == 2)  
    { 
        boxcols <- c(boxcols, tag.pal[[2]](length(unique(Y[, tags[2]]))))
                     }
    
  #convert characters to factors
  X <- rapply(X, as.factor, classes="character", how="replace")
  X$col1 <- X[,tags[1]] 
  if(is.numeric(X[,tags[1]]) & !is.integer(X[,tags[1]]))
    {
    X$col1 <- tag.pal[[1]](breaks[1])[as.numeric(cut(X[, tags[1]],breaks = breaks[1]))]
    X$col.numeric1 <- cut(X[, tags[1]],breaks = breaks[1])
      }  else {
        X$col1 <- as.factor(X$col1)
        X$col1 <- droplevels(X$col1)
        levels(X$col1) <- boxcols[1:length(unique(X$col1))]
        }
 
  #add to df for legend
  if(is.numeric(X[,tags[1]]) & !is.integer(X[,tags[1]]))
    tag.col.df <- X[!duplicated(X[,"col.numeric1"]), c("col.numeric1", "col1")] else
      tag.col.df <- X[!duplicated(X[,tags[1]]), c(tags[1], "col1")]
      
    tag.col.df$tag.col <- tags[1]
  names(tag.col.df) <- c("tag", "col", "tag.col")

if(length(tags) == 2) 
    {
    X$col2 <- X[,tags[2]] 
    if(is.numeric(X[,tags[2]]) & !is.integer(X[,tags[2]]))
    {
      X$col2 <- tag.pal[[2]](breaks[2])[as.numeric(cut(X[, tags[2]],breaks = breaks[2]))]
      X$col.numeric2 <- cut(X[, tags[2]],breaks = breaks[2])
    }  else {  
    X$col2 <- as.factor(X$col2)
    X$col2 <- droplevels(X$col2)
    levels(X$col2) <- boxcols[(length(unique(X$col1))+1):length(boxcols)]
}
 
    if(is.numeric(X[,tags[2]]) & !is.integer(X[,tags[2]]))
      W <- X[!duplicated(X[ ,"col.numeric2"]), c("col.numeric2", "col2")] else    
    W <- X[!duplicated(X[,tags[2]]), c(tags[2], "col2")]
    
    W$tag.col <- tags[2]
    names(W) <- c("tag", "col", "tag.col")
    W$tag <- as.character(W$tag)
    tag.col.df <- rbind(tag.col.df, W)
    }  
   
      
    # add hatching lines for color tags
    if(hatching == 0 | is.null(tags)) 
    {   
      tag.col.df$pattern <- "no.pattern"
      X$pattern.1 <- "no.pattern"
      X$pattern.2 <- "no.pattern"
    }  else {
      
      tag.col.df$pattern <- rep(c("diamond", "grid", "forward", "backward", "horizontal", "vertical"), ceiling(nrow(tag.col.df)/6))[1:nrow(tag.col.df)] 

            if(hatching == 1 & length(tags) == 2)
            {if(is.numeric(X[,tags[2]]) & !is.integer(X[,tags[2]])) 
         tag.col.df$pattern[tag.col.df$tag %in% as.character(X$col.numeric2)] <- "no.pattern"
                   
else
  tag.col.df$pattern[tag.col.df$tag %in% X[,tags[2]]] <- "no.pattern"
            }
      
            if(hatching == 2 & length(tags) == 2)
              if(is.numeric(X[,tags[1]]) & !is.integer(X[,tags[1]]))
                tag.col.df$pattern[tag.col.df$tag %in% as.character(X$col.numeric1)] <- "no.pattern" else
                tag.col.df$pattern[tag.col.df$tag %in% X[,tags[1]]] <- "no.pattern"
            
    }


      X <- do.call(rbind, lapply(1:nrow(X), function(x) {
        W <- X[x, ]
        if(is.numeric(X[,tags[1]]) & !is.integer(X[,tags[1]]))
          W$pattern.1 <-tag.col.df$pattern[tag.col.df$tag == as.character(W$col.numeric1)]
          else  
        W$pattern.1 <-tag.col.df$pattern[tag.col.df$tag == as.character(W[,tags[1]])]
        
        if(length(tags) == 2)
        {   if(is.numeric(X[,tags[2]]) & !is.integer(X[,tags[2]])) 
          W$pattern.2 <-tag.col.df$pattern[tag.col.df$tag == as.character(W$col.numeric2)] else 
            W$pattern.2 <- tag.col.df$pattern[tag.col.df$tag == as.character(W[,tags[2]])]
        } else Y$pattern.2 <- "no.pattern"
          return(W)
      }))
    
    
  tag.col.df <- rapply(tag.col.df, as.character, classes="factor", how="replace")
  } else legend <- 0
  
  
  # grouping color
  if(!is.null(group.tag))
  {
    #convert to character
    Y <- as.data.frame(rapply(X, as.character, classes="factor", how="replace"))
    
    #if tag is numeric
   grcl <- tag.pal[[3]](length(unique(Y[, group.tag]))) 
    
    #convert characters to factors
    X <- rapply(X, as.factor, classes="character", how="replace")
    X$colgroup <- X[,group.tag] 
    X$colgroup <- droplevels(as.factor(X$colgroup))
    levels(X$colgroup) <- grcl[1:length(unique(X$colgroup))]
  
    #add to df for legend
  #   tag.col.df2 <- X[!duplicated(X[,group.tag]), c(group.tag, "colgroup")]
  #   names(tag.col.df2) <- c("tag", "col")
  #   tag.col.df2$tag.col <- paste(group.tag, "(group.tag)")
  #   tag.col.df2$pattern <- "no.pattern"
  #   
  # if(!exists("tag.col.df"))
  #     tag.col.df <- tag.col.df2 else tag.col.df <- rbind(tag.col.df, tag.col.df2)
   }
  
    #calculate time and freq ranges based on all recs
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

flim[2] <- min(rangs$fl2)

# adjust times if same.time.scale = T
if(same.time.scale)
{
  X2 <- lapply(1:nrow(X), function(x)
  {
  Y <- X[x, ]
  dur <- Y$end - Y$start
  if(dur < max(rangs$mardur)) {
    Y$end  <- Y$end + (max(rangs$mardur) - dur)/2
    Y$start  <- Y$start - (max(rangs$mardur) - dur)/2
    if(Y$start < 0) {
      Y$end <- Y$end - Y$start  
      Y$start <- 0
      }
    }
return(Y)
})
X <- do.call(rbind, X2)
}

 
# function to run on data frame subset   
catalFUN <- function(X, nrow, ncol, page, labels, grid, fast.spec, flim, wl, ovlp, pal, width, height, tag.col.df, legend, cex, img.suffix)
{
#set layout for screensplit
#rows
if(is.null(tags))
  rws <- rep(c(5, nrow / 8), nrow) else   rws <- rep(c(5, nrow / 4), nrow)
  
if(same.time.scale) rws <- c(nrow / 1.7, rws) else rws <- c(nrow / 8, rws)


#define row width
csrws <- cumsum(rws)
rws <- csrws/max(csrws)
minrws <- min(rws)
tp <- sort(rws[-1], decreasing = TRUE)
tp <- rep(tp, each = ncol + 1)
btm <- c(sort(rws[-length(rws)], decreasing = TRUE))
btm <- rep(btm, each = ncol + 1)

#columns
lfcol.width <- ncol / 27
faxis.width <- ncol / 37
if(faxis.width < 0.2) faxis.width <- 0.2
if(ncol > 1)
{
  spectroclms <- c(lfcol.width, faxis.width, rep(1, ncol))
csclms <- cumsum(spectroclms)
cls <- csclms/max(csclms)
lf <- c(0, cls[-length(cls)])
rgh <- cls
} else { 
  lf <- c(0, lfcol.width, 0.014 + lfcol.width)
  rgh <- c(lfcol.width, 0.014 + lfcol.width, 1)
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
m <- rbind(m, c(minlf, 1, 0, minbtm))

#add legend col
if(legend > 0)
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
if(!is.null(img.suffix)) img.suffix <- paste0("-", img.suffix)
imgfun(filename = paste0("Catalog_p", page, img.suffix, ".", it), units = "in", width = width, height = height, res = res)

# split graphic window
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
sqplots <- which(m[,2] != minlf)

lapply(sqplots, function(i)
                  {

    if(i <= nrow(X3) & !is.null(group.tag)) par(bg = X3$colgroup[i], new = TRUE) else par(bg = "white", new = TRUE)
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
  
  rec <- tuneR::readWave(as.character(X3$sound.files[i]), from = t[1], to = t[2], units = "seconds")

    #add xaxis to bottom spectros
  if(!same.time.scale) {
    axisX = TRUE
    btm = 2.6
  } else {
    axisX = FALSE
    btm = 0
  } 
  
  axisY <- ifelse(m[i,1] == minlf, TRUE, FALSE) 
  
  
  par(mar = c(btm, rep(spec.mar, 3)))
  
  if(!is.null(group.tag))
    {
    plot(x=-1, y=-1, axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         panel.first={points(0, 0, pch=16, cex=1e6, col = spec.bg)})
    
      }  
  
  # draw spectro
  spectro.INTFUN.2(wave = rec, f = rec@samp.rate, flim = flim, wl = wl, ovlp = ovlp, axisX = axisX, axisY = axisY, tlab = NULL, flab = NULL, palette = pal, fast.spec = fast.spec, main = NULL, grid = gr, page = page, rm.zero = TRUE, cexlab = cex * 1.2, collevels = collev, cexaxis = cex * 1.2, add = TRUE)

} else { #plot labels
  
  par(mar = rep(0, 4))

    plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  
  #color boxes
  if(!is.null(tags))
  {
    cutbox1 <- 0
    cutbox2 <- tag.widths[1]/(tag.widths[1] + tag.widths[2])
    
    lim <- par("usr")
    if(length(tags) == 1)
    rectw.INTFUN(xl = lim[1] + cutbox1, yb = lim[3]-1, xr = lim[2], yt = 0.5, bor = "black", cl = X3$col1[i], den = 10, ang = NULL, pattern = X3$pattern.1[i]) else {
      rectw.INTFUN(xl = lim[1] + cutbox1, yb = lim[3]-1, xr = cutbox2, yt = 0.5, bor = "black", cl = X3$col1[i], den = 10, ang = NULL, pattern = X3$pattern.1[i])
      rectw.INTFUN(xl = cutbox2, yb = lim[3]-1, xr = lim[2], yt = 0.5, bor = "black", cl = X3$col2[i], den = 10, ang = NULL, pattern = X3$pattern.2[i])
    }
    
    #plot labels
    text(x = 0.5, y = 0.8, labels = paste(X3[i, labels], collapse = " "), 
         cex = (ncol * nrow * 1.5 * cex)/((ncol * nrow)^1.2))
    } else
             text(x = 0.5, y = 0.5, labels = paste(X3[i, labels], collapse = " "), 
                  cex = (ncol * nrow * 2 * cex)/((ncol * nrow)^1.2))
           }
    }  
    
  #add Freq axis label
  if(i == nrow(m) - 1)
  {
    par(mar = c(0, 0, 0, 0), bg = "white", new = T)
    plot(1, frame.plot = FALSE)
    text(x = 1, y = 1.05, "Frequency (kHz)", srt = 90, cex = 1.2 * cex) 
  }
   
  #add time axis label
  if(i == nrow(m))
  {
    par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
    plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")  
    
    if(same.time.scale)
    {
      # add title
        text(x = 0.5, y = 0.25, "Time (s)", cex = 1.2 * cex) 
      
      # max duration
      maxdur <- max(X$end - X$start)
      xlab <- pretty(seq(0, maxdur, length.out = 3), min.n = 5)
      xlab <- xlab[xlab < maxdur & xlab > 0]
      xs <- xlab/mean(X$end - X$start)   
      xs <- xs/ncol
        
      finncol <- which(nrow(X) >= seq(0, nrow * ncol, nrow)[-1])
      
      if(length(finncol) > 0)
    {  usr <- par("usr")
      sq <- c(seq(min(usr), max(usr), length.out = ncol + 1))
      sq <- sq[-length(sq)]
      sq <- sq[finncol]
      out <- lapply(sq, function(p)
        {
        out <- lapply(1:length(xs), function(w)
                      {
                        lines(y = c(0.9, 1.04), x = c(xs[w], xs[w]) + p)
                        text(y = 0.75, x = xs[w] + p, labels = xlab[w], cex = cex)
                      })
        })
      }
      } else text(x = 0.5, y = 0.5, "Time (s)", cex = 1.2 * cex) 
  
  }

  #add legend 
  if(legend > 0 & i == nrow(m) - 2)
  {
     
     par( mar = rep(0, 4))
    plot(0.5, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n")  
    
    # define y limits for legend labels
    y1 <- 0.2
    y2 <- 0.8
    
    #remove rows if legend != 3
    if(legend == 1)
      tag.col.df <- droplevels(tag.col.df[tag.col.df$tag.col == tags[1], ])
    
    if(legend == 2)
      tag.col.df <- droplevels(tag.col.df[tag.col.df$tag.col == tags[2], ])
    
    
    
    if(nrow(tag.col.df) > 15) 
 {
      y1 <- 0.03
     y2 <- 0.97
 }     
      
      y <- seq(y1, y2, length.out = nrow(tag.col.df) + length(unique(tag.col.df$tag.col)))
    
    y <- y[length(y):1]
    step <-  y[1] - y[2]
    
    #add left right if 2 tags
    if(length(tags) == 2)
    {
      if(legend == 3)
      {
      labtag1 <- paste("left:", tags[1])
      labtag2 <- paste("right:", tags[2])
    } else {      
    labtag2 <- tags[2]
    labtag1 <- tags[1]  
    }    
      } else labtag1 <- tags[1] 
      
    #adjust if numeric
    if(is.numeric(X[,tags[1]]) & !is.integer(X[,tags[1]]))
    {
      aa <- as.character(sapply(strsplit(as.character(tag.col.df$tag[tag.col.df$tag.col == tags[1]]), ",", fixed = T), "[", 1))
      tag.col.df[tag.col.df$tag.col == tags[1],] <- tag.col.df[order(as.numeric(substr(aa, 2, nchar(aa)))),]
    }
      
if(legend %in% c(1, 3))  
{    text(x = 0.5, y = max(y) + step, labels = labtag1, cex = cex, font = 2) 

    out <- lapply(which(tag.col.df$tag.col == tags[1]), function(w)
    {
      # plot label
      text(x = 0.5, y = y[w], labels = tag.col.df$tag[w], cex = cex) 
      
      #plot color box
      rectw.INTFUN(xl = 0.3, yb = y[w] - (step/2) - (step/6), xr = 0.7, yt =  y[w] - (step/2) + (step/6), bor = "black", cl = tag.col.df$col[w],  den = 10, ang = NULL, pattern = tag.col.df$pattern[w])
      })
}
    
    nrowtag1 <- nrow(tag.col.df[tag.col.df$tag.col == tags[1], ])
    
    if(length(tags) == 2 & legend %in% c(2, 3))
    {

      #remove first tag
      tag.col.df <- tag.col.df[tag.col.df$tag.col == tags[2],]

            if(is.numeric(X[,tags[2]]) & !is.integer(X[,tags[2]]))
        {       
        aa <- as.character(sapply(strsplit(as.character(tag.col.df$tag), ",", fixed = T), "[", 1))
        tag.col.df <- tag.col.df[order(as.numeric(substr(aa, 2, nchar(aa)))),]
        }

      if(legend == 3)
        text(x = 0.5, y = y[nrowtag1 + 2], labels = labtag2, cex = cex, font = 2) else
          text(x = 0.5, y = ifelse(max(y) + step < 1, max(y) + step, 0.99), labels = labtag2, cex = cex, font = 2) 
      
      if(legend == 3)
      y <- y - step * 2

      out <- lapply(1:nrow(tag.col.df), function(w)
      {
        # plot label
        text(x = 0.5, y = y[w + nrowtag1], labels = tag.col.df$tag[w], cex = cex) 
        
        #plot color box
        rectw.INTFUN(xl = 0.3, yb = y[w + nrowtag1] - (step/2) - (step/6), xr = 0.7, yt = y[w + nrowtag1] - (step/2) + (step/6), bor = "black", cl = tag.col.df$col[w],  den = 10, ang = NULL, pattern = tag.col.df$pattern[w])
      })
      
    }
    
    
  }
   }
)
  close.screen(all.screens = TRUE)
dev.off()
}  

#run function over X to split it in subset data frames
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
       catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast.spec, flim, wl, ovlp, pal, 
                width, height, tag.col.df, legend, cex, img.suffix)

     parallel::stopCluster(cl)

     }
   }
   if(Sys.info()[1] == "Linux") {    # Run parallel in Linux

     if(pb)
     out <- pbmcapply::pbmclapply(1:length(Xlist), mc.cores = parallel, function (z) {
       catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast.spec, flim, wl, ovlp, pal, 
                width, height, tag.col.df, legend, cex, img.suffix)
     }) else
         out <- parallel::mclapply(1:length(Xlist),  mc.cores = parallel, function (z) {
       catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast.spec, flim, wl, ovlp, pal,
                width, height, tag.col.df, legend, cex, img.suffix)
            })
   }
   if(!any(Sys.info()[1] == c("Linux", "Windows"))) # parallel in OSX
   {
     cl <- parallel::makeForkCluster(getOption("cl.cores", parallel))

     doParallel::registerDoParallel(cl)

     out <- foreach::foreach(z = 1:length(Xlist)) %dopar% {
       catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast.spec, flim, wl, ovlp, pal, 
                width, height, tag.col.df, legend, cex, img.suffix)
     }

     parallel::stopCluster(cl)

   }
 } else {
   if(pb)
     out <- pbapply::pblapply(1:length(Xlist), function(z)
       catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast.spec, flim, wl, ovlp, pal, 
                width, height, tag.col.df, legend, cex, img.suffix))  else
         out <- lapply(1:length(Xlist), function(z)
           catalFUN(X = Xlist[[z]], nrow, ncol, page = z, labels, grid, fast.spec, flim, wl, ovlp, pal, 
                    width, height, tag.col.df, legend, cex, img.suffix))
 }

if(!is.null(path)) setwd(wd)
}
  
  
