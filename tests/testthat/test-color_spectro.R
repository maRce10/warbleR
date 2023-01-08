test_that("without selection table", {
  
  data(list = c("Phae.long1", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files

  # subset selection table
  st <- lbh_selec_table[lbh_selec_table$sound.files == "Phae.long1.wav", ]

  # read wave file as an R object
  sgnl <- tuneR::readWave(file.path(tempdir(), st$sound.files[1]))

  # create color column
  st$colors <- c("red2", "blue", "green")

  # highlight selections
 expect_equal(color_spectro(
    wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-40, 0, 5),
    dB = "B", X = st, col.clm = "colors", base.col = "skyblue", t.mar = 0.07, f.mar = 0.1,
    interactive = NULL
  ), c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0))
})