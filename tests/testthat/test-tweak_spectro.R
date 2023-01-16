test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

  # variable collevels
  ps <- tweak_spectro(X = lbh_selec_table, wl = 164, ovlp = c(90), wn = c("flattop"), flim = c(0, 10),
  length.out = 4, nrow = 2, ncol = 2, width = 20, height = 11.3, rm.axes = TRUE,
  cex = 1, box = F, collev.min = c(-20, -150), path = tempdir())
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  
    expect_null(ps)
})
