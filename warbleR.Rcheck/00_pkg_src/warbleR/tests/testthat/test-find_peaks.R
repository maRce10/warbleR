test_that("basic", {
  
  fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
  
  unlink(fls)
  
    # load data
    data(list = c("Phae.long4", "Phae.long2", "lbh_selec_table2", "comp_matrix"))

    # save sound files
    writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
    writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

    # run cross-correlation
    suppressWarnings(xc.output <- cross_correlation(
      X = lbh_selec_table2, output = "list",
      compare.matrix = comp_matrix, path = tempdir(), pb = FALSE
    ))

    # find peaks
    suppressWarnings(pks <- find_peaks(xc.output = xc.output, path = tempdir(), pb = FALSE))
  
    fls <- list.files(path = tempdir(), pattern = "wav$|wac$|mp3$|flac$", full.names = TRUE)
    
    unlink(fls)
    
  expect_equal(nrow(pks), 8)
})
