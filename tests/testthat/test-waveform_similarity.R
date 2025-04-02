test_that("standard", {
  
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  
  wvc1 <- waveform_similarity(X = lbh_selec_table[1:6, ], path = tempdir(), sim.method = "DTW", pb = FALSE)
  wvc2 <- waveform_similarity(X = lbh_selec_table[1:6, ], path = tempdir(), sim.method = "correlation", pb = FALSE)
  

  cr <- cor(c(wvc1), c(wvc2))
  fls <- list.files(path = tempdir(), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  
  # highlight selections
  expect_equal(cr, -0.962159937)
})


test_that("sliding", {
  
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  
  wvc1 <- waveform_similarity(X = lbh_selec_table[1:4, ], path = tempdir(), sim.method = "DTW", type = "sliding", pb = FALSE)
  wvc2 <- waveform_similarity(X = lbh_selec_table[1:4, ], path = tempdir(), sim.method = "correlation", type = "sliding", pb = FALSE)
  
  
  cr <- cor(c(wvc1), c(wvc2))
  fls <- list.files(path = tempdir(), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  
  # highlight selections
  expect_equal(cr, -0.9859650)
})