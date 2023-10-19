test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  
  spectrograms(lbh_selec_table, flim = c(0, 11), inner.mar = c(4,4.5,2,1), outer.mar = c(4,2,2,1),
               picsize = 2, res = 300, cexlab = 2, mar = 0.05, wl = 300, path = tempdir(), pb = FALSE)
  
  #go to the working directory (tempdir()) and delete some images
  imgs <- list.files(path = tempdir(), pattern = "jpeg", full.names = TRUE)
  
  # delete 5
  unlink(imgs[1:5])
  
  #filter selection data frame
  fmloc <- filter_sels(X = lbh_selec_table, path = tempdir())
  
  expect_true(nrow(fmloc) == 3)
})
