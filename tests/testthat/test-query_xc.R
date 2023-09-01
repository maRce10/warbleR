test_that("basic", {

  X <- query_xc("Phaethornis anthophilus", download = FALSE)
  
  expect_true(is.data.frame(X))

  expect_true(nrow(X) > 0)
  
})
