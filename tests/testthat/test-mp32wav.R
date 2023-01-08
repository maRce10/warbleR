test_that("basic", {

  qxc <- query_xc(qword = "nr:149950", download = TRUE, path = tempdir(), pb = FALSE)

  # create folder to move image files
  dir.create(file.path(tempdir(), "mp3"))
  
  # Convert all files to .wav format
  mp32wav(path = tempdir(), dest.path = file.path(tempdir(), "mp3"), overwrite = TRUE, pb = FALSE)

  fls <- list.files(path =  file.path(tempdir(), "mp3"), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(length(fls), 1)

})
