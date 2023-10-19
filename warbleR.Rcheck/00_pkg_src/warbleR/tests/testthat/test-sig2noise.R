test_that("basic", {

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # get warbleR sound file examples
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
  # measure
  s2n <- sig2noise(lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ],
      mar = 0.2,
      path = tempdir(),
      pb = FALSE
    )
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(nrow(s2n) == 3 & ncol(s2n) == 8 & any(names(s2n) %in% "SNR"))

})
