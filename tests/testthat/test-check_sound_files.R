test_that("without selection table", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # save wav file examples
  data(list = c("Phae.long1", "Phae.long2"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

  # without selection data frame
  csf <- capture_messages(check_sound_files(path = tempdir()))

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(grepl("All files can be read", csf[[1]]))
})

test_that("with selection table", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # save wav file examples
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  
  # with selection data frame
  # without selection data frame
  csf <- capture_messages(check_sound_files(X = lbh_selec_table[1:5, ], path = tempdir()))
  
  fls <- list.files(path = tempdir(), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(grepl("All files can be read", csf[[1]]))
})

