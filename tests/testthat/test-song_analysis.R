test_that("basic", {

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  # get warbleR sound file examples
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  
  # add a 'song' column
  lbh_selec_table$song <- c("song1", "song1", "song1", "song2",
    "song2", "song3", "song3", "song3", "song4", "song4", "song4")

  # measure acoustic parameters
  sp <- spectro_analysis(lbh_selec_table[1:5, ], bp = c(1, 11), 300, fast = TRUE, path = tempdir(), pb = FALSE)

  # add song data
  sp <- merge(sp, lbh_selec_table[1:5, ], by = c("sound.files", "selec"))

  # caculate song-level parameters for all numeric parameters
  sa <- song_analysis(X = sp, song_colm = "song", parallel = 1, pb = FALSE)
  

  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(nrow(sa) == 2 & ncol(sa) == 40)
  
})
