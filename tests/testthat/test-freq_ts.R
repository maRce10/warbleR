test_that("dominant", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
    data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
    writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
    writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

    fts <- freq_ts(X = lbh_selec_table[1:5, ], length.out = 30, flim = c(1, 12), bp = c(2, 9),
            wl = 300, pb = FALSE, path = tempdir(), img = TRUE)

    fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
    
    unlink(fls)
    
  expect_true(nrow(fts) == 5 & ncol(fts) == 32)
})


test_that("fundamental", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
  fts <- freq_ts(X = lbh_selec_table[1:5, ], length.out = 30, flim = c(1, 12), bp = c(2, 9),
                 wl = 300, pb = FALSE, path = tempdir(), type = "fundamental", img = FALSE)
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(nrow(fts) == 5 & ncol(fts) == 32)
})




test_that("fundamental seewave", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
  fts <- freq_ts(X = lbh_selec_table[1:5, ], length.out = 30, flim = c(1, 12), bp = c(2, 9),
                 wl = 300, pb = FALSE, path = tempdir(), type = "fundamental", ff.method = "tuneR")
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(nrow(fts) == 5 & ncol(fts) == 32)
})

test_that("entropy", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
suppressWarnings(fts <- freq_ts(X = lbh_selec_table[1:5, ], length.out = 30, flim = c(1, 12), bp = c(2, 9),
                 wl = 300, pb = FALSE, path = tempdir(), type = "entropy", raw.contour = TRUE, img = FALSE, clip.edges = TRUE, adjust.wl = TRUE))
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(nrow(fts) == 5 & ncol(fts) == 32)
})
