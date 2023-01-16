test_that("basic", {

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # get warbleR sound file examples
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
 
  spectrograms(
        X = lbh_selec_table, flim = c(0, 11), res = 300, mar = 0.05,
        wl = 300, path = tempdir(), pb = FALSE
      )
  
  imgs <- list.files(path = tempdir(), pattern = "jpeg$|tiff$", full.names = TRUE)
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(length(imgs), 3)
  
})
