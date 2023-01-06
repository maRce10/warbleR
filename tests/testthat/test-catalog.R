test_that("basic catalog", {
  
  data(list = c("Phae.long1", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  
  # create extended selection table
  catalog(X = lbh_selec_table[1:3, ], flim = c(1, 10), nrow = 3, ncol = 4, same.time.scale = T, ovlp = 90, parallel = 1, 
          mar = 0.01, wl = 200, gr = FALSE, pal = gray.3, it = "tiff", pb = F, fast.spec = T, res = 260,
          orientation = "h", labels = c("sound.files", "selec"), tags = "selec", legend = 0, cex = 1, 
          tag.pal = list(terrain.colors), leg.wd = 4, collev = seq(-65, 0, 5), by.row = T, spec.mar =1, path = tempdir())
  
  catalog_file <- list.files(tempdir(), pattern = "tiff")
  
  expect_equal("Catalog_p1.tiff", catalog_file)
})
