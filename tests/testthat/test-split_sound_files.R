test_that("basic", {

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # get warbleR sound file examples
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
 
    # split files in 1 s files
    ssf <- split_sound_files(sgmt.dur = 1, path = tempdir(), pb = FALSE)
   
  fls <- list.files(path = tempdir(), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(length(fls), 4)
  
})
