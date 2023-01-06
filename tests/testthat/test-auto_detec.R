test_that("basic detection", {
  tuneR::writeWave(NatureSounds::Phae.long1, file.path(tempdir(), "Phae.long1.wav"), extensible = FALSE)

  # perfect detection
  suppressWarnings(ad <- autodetec(
    threshold = 10, ssmooth = 300, power = 1, parallel = 1,
    bp = c(2, 9), wl = 120, mindur = 0.1, maxdur = 1, path = tempdir(), flist = "Phae.long1.wav"
  ))

  expect_equal(nrow(ad), 3)
})
