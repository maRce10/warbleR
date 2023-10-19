test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
    data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
    writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
    writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

    full_spectrograms(
      sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300,
      it = "jpeg", path = tempdir(), dest.path = tempdir(), pb = FALSE
    )

    fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|pdf$|jpeg$", full.names = TRUE)
    
    jpegs <- list.files(path = tempdir(), pattern = "jpeg$", full.names = TRUE)
    
    unlink(fls)
    
  
  expect_true(length(jpegs) == 2)
})


test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
  full_spectrograms(
    sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300,
    it = "jpeg", path = tempdir(), dest.path = tempdir(), pb = FALSE
  )
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|pdf$|jpeg$", full.names = TRUE)
  
  jpegs <- list.files(path = tempdir(), pattern = "jpeg$", full.names = TRUE)
  
  unlink(fls)
  
  
  expect_true(length(jpegs) == 2)
})

test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
  full_spectrograms(
    sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300,
    it = "jpeg", path = tempdir(), dest.path = tempdir(), pb = FALSE
  )
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|pdf$|jpeg$", full.names = TRUE)
  
  jpegs <- list.files(path = tempdir(), pattern = "jpeg$", full.names = TRUE)
  
  unlink(fls)
  
  
  expect_true(length(jpegs) == 2)
})


test_that("adding X", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
  full_spectrograms(
    sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300,
    it = "jpeg", path = tempdir(), dest.path = tempdir(), pb = FALSE, X = lbh_selec_table
  )
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|pdf$|jpeg$", full.names = TRUE)
  
  jpegs <- list.files(path = tempdir(), pattern = "jpeg$", full.names = TRUE)
  
  unlink(fls)
  
  
  expect_true(length(jpegs) == 2)
})
