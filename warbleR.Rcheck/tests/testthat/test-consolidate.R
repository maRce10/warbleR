test_that("basic", {
  
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  cns <- consolidate(path = tempdir())
  
  fls <- list.files(path = file.path(tempdir(), "consolidated_files"), full.names = TRUE, pattern = ".wav$")

  unlink(fls)

  expect_equal(length(fls), 4)
})
