test_that("basic", {

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # get warbleR sound file examples
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  

  # measure frequency contours
  dom.freq.ts <- freq_ts(X = lbh_selec_table[1:2, ], path = tempdir(), img = FALSE, pb = FALSE)

  # get number of inflections
  infl <- inflections(X = dom.freq.ts, pb = FALSE)
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(nrow(infl) == 2 & sum(infl$inflections) == 13)

})
