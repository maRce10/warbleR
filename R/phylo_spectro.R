#' Add spectrograms onto phylogenetic trees
#' 
#' \code{phylo_spectro} Add spectrograms to the tips of an objects of class phylo.
#' @usage phylo_spectro(X, tree, type = "fan", par.mar = rep(1, 4), 
#' size = 1, offset = 2, path = NULL, ladder = NULL, horizontal = TRUE, ...) 
#' @param X 'selection_table', 'extended_selection_table' or data frame containing columns for sound file name 
#' (sound.files), selection number (selec), and start and end time of signals (start and end). 
#' 'top.freq' and 'bottom.freq' columns are optional.
#' The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can be used as the input data frame. If using an 
#' 'extended_selection_table' the sound files are not required (see \code{\link{selection_table}}). 
#' @param tree Object of class 'phylo' (i.e. a phylogenetic tree). Ultrametric trees may produce better results.
#' If \code{NULL} (default) then the current working directory is used.
#' @param type Character string of length 1 specifying the type of phylogeny to be drawn 
#' (as in \code{\link[ape]{plot.phylo}}). Only 'phylogram' (default) and 'fan' are allowed.
#' @param par.mar Numeric vector with 4 elements, default is \code{rep(1, 4)}. Specifies the number of lines 
#' in inner plot margins where axis labels fall, with form c(bottom, left, top, right). 
#' See \code{\link[graphics]{par}}. See 'inner.par' argument for controling spectrogram margins.
#' @param size Numeric vector of length 1 controlling the relative size of spectrograms. Default is 1. 
#' Numbers between range \code{c(>0, Inf)} are allowed. 
#' @param offset Numeric vector of length 1 controlling the space between tips and spectrograms. Default is 2.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param ladder Character string controling whether the phylogeny is ladderized (i.e. the internal structure of the 
#' tree is reorganized to get the ladderized effect when plotted). Only 'left' of 'right' values are accepted. Default is 
#' \code{NULL} (no ladderization). See \code{\link[ape]{ladderize}} for more details.
#' @param horizontal Logical. Controls whether spectrograms in a fan phylogeny are place in a horizontal position 
#' \code{FALSE} or in the same angle as tree tips. Currently only horizontal spectrograms are available. 
#' @param ... Additional arguments to be passed to the internal spectrogram 
#' creating function (\code{\link{specreator}}) or phylogeny plotting function (\code{\link[ape]{plot.phylo}}) for 
#' customizing graphical output.
#' @return A phylogenetic tree with spectrograms on tree tips is plotted in the current graphical device.
#' @family spectrogram creators
#' @seealso \code{\link{specreator}}, \code{\link[ape]{plot.phylo}}
#' @export
#' @name phylo_spectro
#' @details The function uses internally the \code{\link[ape]{plot.phylo}} function to plot the tree 
#' and the \code{\link{specreator}} function to create the spectrograms. Arguments for both of these functions
#' can be provided for further customization.  
#' @examples { 
#' \donttest{
#' # First set empty folder
#' # setwd(tempdir())
#' 
#' # save example sound files
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "selec.table"))
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' 
#' # set spectrogram options (can be done at the phylo_spectro() function too)
#' warbleR_options(wl = 200, ovlp = 90, flim = "frange")
#' 
#' # subset example selection table
#' X <- selec.table[1:8, ]
#' 
#' # create random tree (need ape to be installed)
#' set.seed(1)
#' tree <- ape::rtree(nrow(X))
#' 
#' # Force tree to be ultrametric
#' tree <- ape::chronoMPL(tree)
#' 
#' # add tip label column to example selection table (just for the sake of the example)
#' X$tip.label <- tree$tip.label
#' 
#' 
#' # print phylogram with spectros
#' phylo_spectro(X = X, tree = tree, offset = 0.3, par.mar = c(0, 0, 0, 10),
#' pal = reverse.gray.colors.2, title.labels = c("sound.files", "selec"), size = 2)
#' 
#' # print phylogram with spectros and no margin in spectrograms showing tip labels (higher offset)
#' phylo_spectro(X = X, tree = tree, offset = 0.4, par.mar = c(0, 0, 0, 10), inner.mar = rep(0, 4),
#' pal = reverse.gray.colors.2, title.labels = c("sound.files", "selec"), size = 2)
#' 
#' # print fan tree and no margin in spectrograms
#' phylo_spectro(X = X, tree = tree, offset = 0.7, par.mar = rep(5, 4), inner.mar = rep(0, 4),
#' pal = reverse.gray.colors.2, size = 2, type = "fan")
#'   }
#' }
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on oct-1-2018 (MAS)

phylo_spectro <- function(X, tree, type = "fan", par.mar = rep(1, 4), size = 1, offset = 2, 
                         path = NULL, ladder = NULL, horizontal = TRUE, ...) {

  # error message if ape is not installed
  if (!requireNamespace("ape",quietly = TRUE))
    stop("must install 'ape' to use phylo_spectro()")
  
  # currenlt only horizontal is allowed
  horizontal <- TRUE
  
  ## save curent working directory and use a temporary one for saving images
  wd <- getwd() # save
  on.exit(setwd(wd), add = TRUE) # restores directory at the end
  if (is.null(path)) path <- wd # set  directory
  
  # set temporary working directory to save images  
  setwd(path)
  
  ## save par current setting and restores it later
  opar <- par # save
  on.exit(opar, add = TRUE) # restore
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(phylo_spectro)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0

  # rename path for sound files
  names(opt.argms)[names(opt.argms) == "wav.path"] <- "path"
  
  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]
  
  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]
  
  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]
 
  # set options left
  if (length(opt.argms) > 0)
    for (q in 1:length(opt.argms))
      assign(names(opt.argms)[q], opt.argms[[q]])
 
  # ladderize
  if (!is.null(ladder)) tree <- ape::ladderize(phy = tree, right = ladder == "right")
  
 # check tip labels match
 if (!identical(sort(as.character(X$tip.label)), sort(tree$tip.label))) stop("tree tip labels (tree$tip.label) and 'tip.label' column in 'X' do not match")  
 
 # map arguments provided
 argus <- c(opt.argms, call.argms)
 
 ## SPECTROGRAMS (save in temporary directory)  
  # save specreator call
 cll.spec <- quote(specreator(line = FALSE, pb = FALSE))
 
 # keep arguments in ... found in specreator
 shr.args <- argus[names(argus) %in% names(formals(specreator))]
  
 # add new arguments to call (if present rewrite if no add)
  if (length(shr.args) > 0)
    for(i in 1:length(shr.args))
      if( any(names(cll.spec) == names(shr.args)[i])) cll.spec[names(cll.spec) == names(shr.args)[i]] <- shr.args[[i]] else
      {
        cll.spec[[length(cll.spec) + 1]] <- shr.args[[i]]
        names(cll.spec)[length(cll.spec)] <-   names(shr.args)[i]
      }
  
 # call specreator to save jpegs in temporary directory
  suppressWarnings(eval(cll.spec))
  
  ## PHYLOGENY
  # keep arguments in ... found in specreator
  cll.phylo <- quote(ape::plot.phylo(x = tree))

  # keep arguments in ... found in specreator
  shr.args2 <- argus[names(argus) %in% names(formals(ape::plot.phylo))]
  
  # add new arguments to call (if present rewrite if no add)
  if (length(shr.args2) > 0)
    for(i in 1:length(shr.args2))
      if( any(names(cll.phylo) == names(shr.args2)[i])) cll.phylo[names(cll.phylo) == names(shr.args2)[i]] <- shr.args2[[i]] else
      {
        cll.phylo[[length(cll.phylo) + 1]] <- shr.args2[[i]]
        names(cll.phylo)[length(cll.phylo)] <-   names(shr.args2)[i]
      }
  
  # set graphic parameter margins
  par(mar = par.mar)
  
  # plot tree
  eval(cll.phylo)
  
  # get coordinates for spectrograms
  lastPP <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
  xs <- lastPP$xx[1:nrow(X)]
  ys <- lastPP$yy[1:nrow(X)]
  
   # fix offset (taken from ape::tiplabels)
  if (offset != 0) {
    if (lastPP$type %in% c("phylogram", "cladogram")) {
      switch(lastPP$direction, rightwards = {
        xs <- xs + offset
      }, leftwards = {
        xs <- xs - offset
      }, upwards = {
        ys <- ys + offset
      }, downwards = {
        ys <- ys - offset
      })
    }

    if (lastPP$type %in% c("fan", "radial")) {
      tmp <- ape::rect2polar(xs, ys)
      tmp <- ape::polar2rect(tmp$r + offset, tmp$angle)
      xs <- tmp$x
      ys <- tmp$y
    }
    }

  # get degrees
  dgs <- rep(0, length(xs))
  
  # fix size according to number of tips in tree
  size <- size / sqrt(nrow(X))
  
  # if not horizontal
  if (!horizontal)
  {
    # cuadrant I
    dgs[xs > 0 &  ys > 0] <- asin(abs(ys[xs > 0 &  ys > 0])/ sqrt((xs[xs > 0 &  ys > 0])^2 + (ys[xs > 0 &  ys > 0])^2)) * 180 /pi
    
    # cuadrant II
    dgs[xs > 0 &  ys < 0] <- asin((ys[xs > 0 &  ys < 0])/ sqrt((xs[xs > 0 &  ys < 0])^2 + (ys[xs > 0 &  ys < 0])^2)) * 180 /pi
    
    # cuadrant III
    dgs[xs < 0 &  ys < 0] <- asin(abs(ys[xs < 0 &  ys < 0])/ sqrt((xs[xs < 0 &  ys < 0])^2 + (ys[xs < 0 &  ys < 0])^2)) * 180 /pi
    
    # cuadrant IV
    dgs[xs < 0 &  ys > 0] <- (acos((xs[xs < 0 &  ys > 0])/ sqrt((xs[xs < 0 &  ys > 0])^2 + (ys[xs < 0 &  ys > 0])^2)) * 180 /pi) + 180
  } 


  # add spectros to phylogeny  
  for(y in 1:nrow(X)) {
    
    # read images    
    img <- jpeg::readJPEG(source = file.path(path, paste0(X$sound.files[y], "-", X$selec[y], ".jpeg")))
    
    # get image aspect
    asp <- dim(img)[1]/dim(img)[2]
   
    # allow to plot outside main image area
    par(xpd = TRUE, mar = par()$mar + c(0, 0, 0, 7))

    # add spectrograms
    graphics::rasterImage(image = img, xleft = xs[y] - (size / 2), ybottom = ys[y] - (size / 2  * asp), xright = xs[y] + (size / 2), ytop = ys[y] + (size * asp / 2), angle = dgs[y])
    
  }
  
  # add points to coordinates for each label
  # points(x = xs, y = ys, pch = 20, col = "purple")
  
  # add labels to check degrees
  # for( i in 1:nrow(X))
  # text(x = xs[i], y = ys[i], cex =2 , labels = "text" , col = "green", srt = dgs[i])
  
# return(cbind(xs, ys, dgs))
  }
