test_that("basic", {

  X <- query_xc("Phaethornis anthophilus", download = FALSE)

  # create image in R graphic device
  mp <- map_xc(X, img = FALSE)

  expect_null(mp)
  
})
