test_that("SP vs dfDTW", {
  
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  unlink(list.files(path = tempdir(), pattern = "comp.meth-dfDTW-SP", full.names = TRUE))
  
  
  cm  <- compare_methods(
    X = lbh_selec_table, flim = c(0, 10), bp = c(0, 10), mar = 0.1, wl = 300,
    ovlp = 90, res = 200, n = 10, length.out = 30,
    methods = c("SP", "dfDTW"), parallel = 1, it = "jpeg", path = tempdir()
  )
  
  imgs <- list.files(path = tempdir(), pattern = "comp.meth-dfDTW-SP")

  # highlight selections
  expect_equal(length(imgs), 10)
})