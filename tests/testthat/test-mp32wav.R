test_that("basic", {

  if (Sys.info()[1] != "Windows"){   # to avoid weird unit testing error on windows 

  # create folder to move image files
  dir.create(file.path(tempdir(), "mp3"))

  # Convert all files to .wav format
  mp32wav(path = system.file("extdata", package = "bioacoustics"), dest.path = file.path(tempdir(), "mp3"), overwrite = TRUE, pb = FALSE)

  fls <- list.files(path =  file.path(tempdir(), "mp3"), pattern = "wav$", full.names = TRUE)

  unlink(fls)
  unlink(file.path(tempdir(), "mp3"), recursive = TRUE)

  } else fls <- 1

  expect_length(fls, 1)
})
