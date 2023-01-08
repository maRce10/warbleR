test_that("basic", {

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # get warbleR sound file examples
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
  sm_sng <- simulate_songs(
    n = 6, harms = 1, seed = 1, diff.fun = "pure.tone",
    freqs = seq(4, 6, length.out = 6), selec.table = TRUE,
    path = tempdir()
  )
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(names(sm_sng), c("selec.table", "wave"))

})