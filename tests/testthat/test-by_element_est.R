test_that("basic est convertion", {
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  # create extended selection table
  by_song_est <- selection_table(lbh_selec_table,
    path = tempdir(),
    extended = TRUE, by.song = "sound.files", pb = FALSE
  )

  bs_est <- by_element_est(X = by_song_est, mar = 0.05, pb = FALSE)

  fls <- list.files(path = tempdir(), pattern = "wav$", full.names = TRUE)
  
  unlink(fls)
  
  expect_true(!attr(bs_est, "by.song"))
})
