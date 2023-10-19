test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
    data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
    writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
    writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

    # not normalize
    wpdf <- wpd_features(lbh_selec_table[1:5, ], threshold2 = 0.3, nor = FALSE, path = tempdir())
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  
    expect_true(is.data.frame(wpdf))
})
