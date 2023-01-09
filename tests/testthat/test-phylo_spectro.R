test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  

  # set spectrogram options (can be done at the phylo_spectro() function too)
  warbleR_options(wl = 200, ovlp = 90, flim = "frange", wav.path = tempdir())

  # subset example selection table
  X <- lbh_selec_table[1:8, ]

  # create random tree (need ape to be installed)
  set.seed(1)
  tree <- ape::rtree(nrow(X))

  # Force tree to be ultrametric
  tree <- ape::chronoMPL(tree)

  # add tip label column to example selection table (just for the sake of the example)
  X$tip.label <- tree$tip.label

  # print phylogram with spectros
  ps <- phylo_spectro(X = X, tree = tree, par.mar = c(0, 0, 0, 8), size = 2)
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  
    expect_null(ps)
})