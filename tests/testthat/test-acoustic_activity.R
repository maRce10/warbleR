test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
  #'
  # get vocal activity by second
  va <- acoustic_activity(X = lbh_selec_table, path = tempdir(), time.window = 1, 
                     hop.size = 1)
                     
  # get the row with the highest rate per sound file
  max_min <- do.call(rbind, lapply(split(va, va$sound.files), function(x) 
  x[which.max(x$rate), ]))
  
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(nrow(va), 12)
  
  expect_equal(nrow(max_min), 4)
})

test_that("including a file with no annotations", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
  writeWave(Phae.long1, file.path(tempdir(), "no_anns.wav"))
  
  va2 <- acoustic_activity(X = lbh_selec_table, path = tempdir(), time.window = 1, 
                 hop.size = 1, files = list.files(tempdir(), pattern = ".wav$"))
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(nrow(va2), 15)
})