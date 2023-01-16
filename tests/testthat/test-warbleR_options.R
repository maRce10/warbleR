test_that("basic", {
  
  warbleR_options(pb = FALSE, path = tempdir())
 
  wo <- warbleR_options(reset = TRUE)
  
  expect_true(is.list(wo))
})
