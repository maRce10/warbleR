test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  lst <- lbh_selec_table
  lst$tailored <- "y"  
  write.csv(file = file.path(tempdir(), "seltailor_output.csv"), x = lst)
  
  fls <- list.files(path = tempdir(), full.names = TRUE)
  
  unlink(fls)
  
    expect_error(tailor_sels(X = lbh_selec_table, flim = c(1, 12), wl = 300, auto.next = TRUE, path = tempdir()))
})