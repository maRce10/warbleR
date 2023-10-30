test_that("basic", {

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)

  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

   # create spectrograms
   spectrograms(lbh_selec_table[1:5, ], path = tempdir(), pb = FALSE)

    # create folder to move image files
    suppressWarnings(dir.create(file.path(tempdir(), "imgs")))

    # copy files
    move_images(cut = FALSE, from = tempdir(), to = file.path(tempdir(), "imgs"), pb = FALSE, overwrite = TRUE)

  fls <- list.files(path =  file.path(tempdir(), "imgs"), pattern = "jpeg$", full.names = TRUE)
  
  unlink(fls)
  
  expect_equal(length(fls), 5)

})
