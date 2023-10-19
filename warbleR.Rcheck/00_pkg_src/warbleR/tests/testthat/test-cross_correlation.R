test_that("basic", {
  
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  
    # run cross correlation on spectrograms (SPCC)
    xcor <- cross_correlation(X = lbh_selec_table[1:4, ], wl = 300, ovlp = 90, path = tempdir(), pb = FALSE)

    fls <- list.files(path = file.path(tempdir(), "consolidated_files"), full.names = TRUE, pattern = ".wav$")
    
    unlink(fls)
    
  expect_true(is.matrix(xcor) & nrow(xcor) == 4)
})
