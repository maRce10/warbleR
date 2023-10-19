test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
    data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
    writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
    writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

    fc <- find_clipping(X = lbh_selec_table[1:5, ], path = tempdir(), pb = FALSE)

    fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
    
    unlink(fls)
    
  expect_true(sum(fc$prop.clipped) > 0.0005)
})
