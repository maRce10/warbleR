#' Add spectrograms onto phylogenetic trees
#' 
#' \code{phylo_spectro} Add spectrograms to the tips of an objects of class phylo.
#' @usage phylo_spectro(X, tree, type = "phylogram", par.mar = rep(1, 4), 
#' size = 1, offset = 0, path = NULL, ladder = NULL, horizontal = TRUE, ...) 
#' @param X 'selection_table', 'extended_selection_table' or data frame containing columns for sound file name 
#' (sound.files), selection number (selec), and start and end time of signals (start and end). 
#' 'top.freq' and 'bottom.freq' columns are optional. In addition, the data frame must include the column 'tip.label' that contains the names of the tip labels found in the tree (e.g. '\code{tree$tip.label}). This column is used to match rows and tip labels. If using an 
#' 'extended_selection_table' the sound files are not required (see \code{\link{selection_table}}). 
#' @param tree Object of class 'phylo' (i.e. a phylogenetic tree). Ultrametric trees may produce better results.
#' If \code{NULL} (default) then the current working directory is used. Tip labels must match the names provided in the 'tip.label' column in 'X' (see 'X' argument).
#' @param type Character string of length 1 specifying the type of phylogeny to be drawn 
#' (as in \code{\link[ape]{plot.phylo}}). Only 'phylogram' (default) and 'fan' are allowed.
#' @param par.mar Numeric vector with 4 elements, default is \code{rep(1, 4)}. Specifies the number of lines 
#' in inner plot margins where axis labels fall, with form c(bottom, left, top, right). 
#' See \code{\link[graphics]{par}}. See 'inner.par' argument for controlling spectrogram margins.
#' @param size Numeric vector of length 1 controlling the relative size of spectrograms. Higher numbers increase the height of spectrograms. Default is 1. 
#' Numbers between range \code{c(>0, Inf)} are allowed. 
#' @param offset Numeric vector of length 1 controlling the space between tips and spectrograms. Default is 0.
#' @param path Character string containing the directory path where the sound files are located. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param ladder Character string controlling whether the phylogeny is ladderized (i.e. the internal structure of the 
#' tree is reorganized to get the ladderized effect when plotted). Only 'left' of 'right' values are accepted. Default is 
#' \code{NULL} (no ladderization). See \code{\link[ape]{ladderize}} for more details.
#' @param horizontal Logical. Controls whether spectrograms in a fan phylogeny are place in a horizontal position 
#' \code{FALSE} or in the same angle as the tree tips. Currently only horizontal spectrograms are available. 
#' @param ... Additional arguments to be passed to the internal spectrogram 
#' creating function (\code{\link{spectrograms}}) or phylogeny plotting function (\code{\link[ape]{plot.phylo}}) for 
#' customizing graphical output. Only rightwards phylogenies can be plotted.
#' @return A phylogenetic tree with spectrograms on tree tips is plotted in the current graphical device.
#' @family spectrogram creators
#' @seealso \code{\link{spectrograms}}, \code{\link[ape]{plot.phylo}}
#' @export
#' @name phylo_spectro
#' @details The function add the spectrograms of sounds annotated in a selection table ('X' argument) onto the tips of a phylogenetic tree. 
#' The 'tip.label' column in 'X' is used to match spectrograms and tree tips. The function uses internally the \code{\link[ape]{plot.phylo}} function to plot the tree 
#' and the \code{\link{spectrograms}} function to create the spectrograms. Arguments for both of these functions
#' can be provided for further customization.
#' @examples { 
#' \donttest{
#' # First set empty folder
#' 
#' 
#' # save example sound files
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' 
#' # set spectrogram options (can be done at the phylo_spectro() function too)
#' warbleR_options(wl = 200, ovlp = 90, flim = "frange", wav.path = tempdir())
#' 
#' # subset example selection table
#' X <- lbh_selec_table[1:8, ]
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
#' # print phylogram with spectros
#' phylo_spectro(X = X, tree = tree, par.mar = c(0, 0, 0, 8), size = 2)
#'
#' # no margin in spectrograms and showing tip labels (higher offset)
#' phylo_spectro(X = X, tree = tree, offset = 0.1, par.mar = c(0, 0, 0, 6), 
#' inner.mar = rep(0, 4), size = 2)
#' 
#' # print fan tree and no margin in spectrograms
#' phylo_spectro(X = X, tree = tree, offset = 0.6, par.mar = rep(3, 4),
#' inner.mar = rep(0, 4), size = 2, type = "fan", show.tip.label = FALSE)
#' 
#' # changing edge color and witdh
#' phylo_spectro(X = X, tree = tree, offset = 0.2, par.mar = rep(3, 4), inner.mar = rep(0, 4), 
#' size = 2, type = "fan", show.tip.label = FALSE, edge.color = "red", edge.width = 2)
#' 
#' # plotting a tree representing cross-correlation distances 
#' xcorr_mat <- cross_correlation(X, bp = c(1, 10))
#' 
#' xc.tree <- ape::chronoMPL(ape::as.phylo(hclust(as.dist(1 - xcorr_mat))))
#' 
#' X$tip.label <- xc.tree$tip.label
#' 
#' phylo_spectro(X = X, tree = xc.tree, offset = 0.03, par.mar = rep(3, 4), 
#' inner.mar = rep(0, 4), size = 0.3, type = "fan", show.tip.label = FALSE, 
#' edge.color = "red", edge.width = 2)
#'   }
#' }
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on oct-1-2018 (MAS)

phylo_spectro <- function(X, tree, type = "phylogram", par.mar = rep(1, 4), size = 1, offset = 0, 
                         path = NULL, ladder = NULL, horizontal = TRUE, ...) {

  # error message if ape is not installed
  if (!requireNamespace("ape",quietly = TRUE))
    stop2("must install 'ape' to use phylo_spectro()")
  
  # error message if jpeg package is not installed
  if (!requireNamespace("jpeg",quietly = TRUE))
    stop2("must install 'jpeg' to use this function")
  
  # currenlt only horizontal is allowed
  if (!horizontal) cat("Currently only horizontal spectrograms are allowed")
  horizontal <- TRUE
  
  #check path if not provided set to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) stop2("'path' provided does not exist") else
      path <- normalizePath(path)
  
  ## save par current setting and restores it later
  opar <- par # save
  on.exit(opar) # restore
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(phylo_spectro)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
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
 if (!identical(sort(as.character(X$tip.label)), sort(tree$tip.label))) stop2("tree tip labels (tree$tip.label) and 'tip.label' column in 'X' do not match")
 
  #sort X as in tip labels
  X <- X[match(tree$tip.label, X$tip.label), ]

  # map arguments provided
 argus <- c(opt.argms, call.argms)
 
 ## SPECTROGRAMS (save in temporary directory)  
  # save specreator call
 cll.spec <- quote(spectrograms(line = FALSE, pb = FALSE, dest.path = tempdir()))
 
 # keep arguments in ... found in specreator
 shr.args <- argus[names(argus) %in% names(formals(spectrograms))]
  
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
  cll.phylo <- quote(ape::plot.phylo(x = tree, direction = "rightwards"))

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
  
  # get border coordinates
  usr <- par()$usr
  # plt <- par()$plt
  # 
  # # correct for margins
  # usr[2] <- usr[2] + ((usr[2] - usr[1]) * (1 - plt[2])) 
  # usr[1] <- usr[1] - ((usr[2] - usr[1]) * plt[1])

  # y limits
  plt.y.rng <- usr[4] - usr[3]
  
  # fix size according to number of tips in tree
  size <- plt.y.rng / nrow(X) * size 
  
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
  
  # if position of  not horizontal
  if (!horizontal & lastPP$type %in% c("fan", "radial"))
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

  # empty list for images
  imgs <- list()
  
  # image coordinate data frame
  coors <- data.frame(xs, ys, size, dgs, asp = NA, xleft = NA, xright = NA, 
                      ybottom = NA, ytop = NA)
  
  # read images and calculate margins for spectrograms
  for(y in 1:nrow(X)) {
    
    # read images    
    imgs[[1 + length(imgs)]] <- jpeg::readJPEG(source = file.path(tempdir(),  paste0(X$sound.files[y], "-", X$selec[y], ".jpeg")))
    
    # get image dimensions
    dims <- dim(imgs[[length(imgs)]])
    
    # get image aspect
    coors$asp[y] <- 1 / (dims[1] / dims[2])
  
    # calculate image limits
    # if (lastPP$type == "phylogram") 
   
    coors$xleft[y] <- coors$xs[y]  
    coors$xright[y] <- coors$xs[y] + size  * coors$asp[y]  
    coors$difx[y] <- size  * coors$asp[y]  
    coors$ybottom[y] <- coors$ys[y] - (size / 2)  
    coors$ytop[y] <- coors$ys[y] + (size / 2)  
      }

  # add spectros to phylogeny  
  for(y in 1:nrow(X)) {

    # allow to plot outside main image area
    par(xpd = TRUE)
    
    # add spectrograms
    if (lastPP$type == "phylogram")
    graphics::rasterImage(image = imgs[[y]], xleft = coors$xleft[y], ybottom = coors$ybottom[y], xright = coors$xleft[y] +  coors$difx[y], ytop = coors$ytop[y], angle = dgs[y])
    else
    graphics::rasterImage(image = imgs[[y]], xleft = xs[y] - (size / 2), ybottom = ys[y] - (size / 2  * coors$asp[y]), xright = xs[y] + (size / 2), ytop = ys[y] + (size * coors$asp[y] / 2), angle = dgs[y])
  }

  }
