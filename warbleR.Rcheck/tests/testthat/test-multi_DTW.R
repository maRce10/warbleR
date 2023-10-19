test_that("basic", {

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # get warbleR sound file examples
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
  # measure
  df <- freq_ts(X = lbh_selec_table[1:3, ], threshold = 10, img = FALSE, path = tempdir(), pb = FALSE)
  se <- freq_ts(X = lbh_selec_table[1:3, ], threshold = 10, img = FALSE, path = tempdir(), pb = FALSE, type = "entropy")

  # run function
  mdtw <- multi_DTW(df, se, pb = FALSE)
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(nrow(mdtw) == 3 & ncol(mdtw) == 3)
})
