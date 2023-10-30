test_that("basic", {
  
  fls <- list.files(path = tempdir(), full.names = TRUE, pattern = ".wav$", recursive = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

  cns <- consolidate(path = tempdir(), pb = FALSE, file.ext = ".wav$", )
  
  fls <- list.files(path = file.path(tempdir(), "consolidated_files"), full.names = TRUE, pattern = ".wav$")

  unlink(fls)

  expect_equal(length(fls), 2)
})
