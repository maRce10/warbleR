test_that("basic catalog", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)

  data("Phae.long1")
  Phae.long1.2 <- stereo(Phae.long1, Phae.long1)
  
  writeWave(Phae.long1.2, file.path(tempdir(), "Phae.long1.2.wav"))
  
  rc <- remove_channels(channels = 1, path = tempdir(), pb = FALSE)
    catalog_file <- list.files(tempdir(), pattern = "tiff")
  
  fls <- list.files(path = file.path(tempdir(), "converted_sound_files"), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  unlink(dirname(fls), recursive = TRUE)
  
  expect_true(any(grepl("Phae.long1.2.wav", fls)))
})
