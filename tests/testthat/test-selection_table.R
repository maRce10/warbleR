test_that("selection table", {
  
    data(list = c(
      "Phae.long1",
      "lbh_selec_table"
    ))
  
  tuneR::writeWave(NatureSounds::Phae.long1, file.path(tempdir(), "Phae.long1.wav"), extensible = FALSE)

  # make selection table    
  st <- selection_table(X = lbh_selec_table[1:3, ], path = tempdir(), pb = FALSE)
  
    
  fls <- list.files(path = tempdir(), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(is_selection_table(st))
})


test_that("basic", {
  
  data(list = c(
    "Phae.long1",
    "lbh_selec_table"
  ))
  
  tuneR::writeWave(NatureSounds::Phae.long1, file.path(tempdir(), "Phae.long1.wav"), extensible = FALSE)
  
  #' # make extended selection table
  est <- selection_table(
    X = lbh_selec_table[1:3, ], extended = TRUE,
    path = tempdir(), pb = FALSE
  )
  
  
  
  fls <- list.files(path = tempdir(), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(is_extended_selection_table(est))
})
