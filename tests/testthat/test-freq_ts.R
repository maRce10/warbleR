test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
    data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
    writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
    writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

    fts <- freq_ts(X = lbh_selec_table, length.out = 30, flim = c(1, 12), bp = c(2, 9),
            wl = 300, pb = FALSE, path = tempdir())

    fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
    
    unlink(fls)
    
  expect_true(nrow(fts) == 5 & ncol(fts) == 32)
})