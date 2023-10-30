test_that("basic catalog", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)

  data("Phae.long1", "lbh_selec_table")
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

    tfc <- track_freq_contour(
      X = lbh_selec_table[1:3,], flim = c(0, 5), ovlp = 90,
      it = "tiff", bp = c(1, 3), contour = "df", wl = 300, frange = TRUE,
      path = tempdir(), pb = FALSE
    )

  
  imgs <- list.files(path = tempdir(), pattern = "tiff$", full.names = TRUE)
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$|jpeg$|tiff$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(any(grepl("Phae.long1.wav-1-trackfreqs.tiff", imgs)))
})
