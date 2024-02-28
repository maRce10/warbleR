test_that("basic", {
  
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)

  data(list = c("Phae.long1", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

  # create extended selection table
  X <- selection_table(
    X = lbh_selec_table[1:3, ], extended = TRUE, pb = FALSE,
    path = tempdir()
  )
  
  # resample
  if (Sys.info()[1] != "Windows") Y <- resample_est(X, pb = FALSE)
    
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  
  expect_true(TRUE) # to avoid errors during unit testing on windows
})
