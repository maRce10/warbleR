test_that("without selection table", {
  
  # save wav file examples
  data(list = c("Phae.long1", "Phae.long2"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

  # without selection data frame
  csf <- capture_output(check_sound_files(path = tempdir()))

  expect_equal(csf, "All files can be read")
})

test_that("with selection table", {
  
  # save wav file examples
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
  
  # with selection data frame
  # without selection data frame
  csf <- capture_output(check_sound_files(X = lbh_selec_table, path = tempdir()))
  
  expect_equal(csf, "All files can be read\nsmallest number of samples: 2838 (sound file:Phae.long2.wav; selection label: 2)")
})
