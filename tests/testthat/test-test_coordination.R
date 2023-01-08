test_that("basic catalog", {
  
  data(sim_coor_sing)

  # set global options (this can also be set within the function call)
  warbleR_options(iterations = 30, pb = FALSE)

  # testing if coordination happens less than expected by chance
  tc <- test_coordination(sim_coor_sing)
  
  expect_true(nrow(tc) == 3 & ncol(tc) == 5)
})
