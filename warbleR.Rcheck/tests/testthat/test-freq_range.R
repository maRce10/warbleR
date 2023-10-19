test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
    data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
    writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
    writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

      fr <- freq_range(
        X = lbh_selec_table[1:4, ], wl = 112, fsmooth = 1, threshold = 13, widths = c(4, 1),
        img = TRUE, pb = FALSE, it = "tiff", line = TRUE, mar = 0.1, bp = c(1, 10.5),
        flim = c(0, 11), path = tempdir()
      )

    fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
    
    unlink(fls)
    
  expect_equal(nrow(fr), 4)
})
