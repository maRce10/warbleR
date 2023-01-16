test_that("basic", {

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # get warbleR sound file examples
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
 
    spl <- sound_pressure_level(
      X = lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ],
      parallel = 1, pb = FALSE, path = tempdir()
    )
  

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(nrow(spl), 3)

})
