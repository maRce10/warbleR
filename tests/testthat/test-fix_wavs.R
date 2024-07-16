test_that("basic", {
  
  if (Sys.info()[1] != "Windows"){
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
    # load data
    data(list = c("Phae.long4", "Phae.long2", "lbh_selec_table", "comp_matrix"))

    # save sound files
    writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
    writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

    fw <- fix_wavs(files = c("Phae.long4.wav", "Phae.long2.wav"), path = tempdir())
    
    fls <- list.files(path = file.path(tempdir(), "converted_sound_files"), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
    
    unlink(fls)
  } else fls <- 1:2
    
  expect_equal(length(fls), 2)
})
