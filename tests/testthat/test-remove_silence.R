test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
  sil <- silence(samp.rate = 22500, duration = 3, xunit = "time")


  wv1 <- pastew(pastew(Phae.long1, sil, f = 22500, output = "Wave"),
  Phae.long2, f = 22500, output = "Wave")

  #check silence in between amplitude peaks
  env(wv1)

   #save wave file
   writeWave(object = wv1, filename = file.path(tempdir(), "wv1.wav"),
    extensible = FALSE)

  #remove silence
   if (Sys.info()[1] != "Windows") rs <- remove_silence(files = "wv1.wav", pb = FALSE, path = tempdir())
  
  fls <- list.files(path = tempdir(), full.names = TRUE)
  
  unlink(fls)
  
    expect_true(any(grepl("silence-removed_files", fls)))
})
