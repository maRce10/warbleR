test_that("basic", {

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # get warbleR sound file examples
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
  snr_spectrograms(lbh_selec_table[1:3, ],
    flim = c(0, 10), inner.mar = c(4, 4.5, 2, 1),
    outer.mar = c(4, 2, 2, 1), picsize = 2, res = 300, cexlab = 2, mar = 0.2,
    snrmar = 0.1, it = "jpeg", wl = 300, path = tempdir(), pb = FALSE
  )
  
  imgs <- list.files(path = tempdir(), pattern = "jpeg$", full.names = TRUE)
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(length(imgs), 3)

})
