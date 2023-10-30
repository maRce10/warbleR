test_that("basic detection", {
  tuneR::writeWave(NatureSounds::Phae.long1, file.path(tempdir(), "Phae.long1.wav"), extensible = FALSE)

  # perfect detection
  suppressWarnings(ad <- auto_detec(pb = FALSE,
    threshold = 10, ssmooth = 300, power = 1, parallel = 1,
    bp = c(2, 9), wl = 120, mindur = 0.1, maxdur = 1, path = tempdir(), flist = "Phae.long1.wav", 
  ))

  fls <- list.files(path = tempdir(), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(nrow(ad), 3)
})

test_that("supplying X, printing output, no detection", {
  tuneR::writeWave(NatureSounds::Phae.long1, file.path(tempdir(), "Phae.long1.wav"), extensible = FALSE)
  tuneR::writeWave(NatureSounds::Phae.long2, file.path(tempdir(), "Phae.long2.wav"), extensible = FALSE)
  splX <- split_sound_files(sgmts = 2, only.sels = TRUE, path = tempdir())
  
  splX$sound.files <- splX$org.sound.files
  splX$selec <- 1:nrow(splX)
  
  suppressWarnings(ad <- autodetec(threshold = 99, ssmooth = 300, power = 1,parallel = 1, X = splX,
                  bp=c(2,9), wl = 120, mindur=0.1, maxdur=1,  path = tempdir(), pb = FALSE))
  
  
  fls <- list.files(path = tempdir(), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(nrow(ad), 4)
})
