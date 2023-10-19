test_that("basic", {

  # get warbleR sound file examples
  data(list = "lbh_selec_table")

  # get gaps
  gps <- gaps(X = lbh_selec_table, pb = FALSE)
  
  expect_equal(nrow(gps), 11)
  
  expect_true(any(names(gps) %in% "gaps"))
  
})
