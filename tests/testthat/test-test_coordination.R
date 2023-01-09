test_that("basic", {
  
  data(sim_coor_sing)

  # testing if coordination happens less than expected by chance
  tc <- test_coordination(sim_coor_sing, iterations = 30, pb = FALSE)
  
  expect_true(nrow(tc) == 3 & ncol(tc) == 5)
  
})


test_that("duration method", {
  
  data(sim_coor_sing)
  
  # testing if coordination happens less than expected by chance
  tc <- test_coordination(sim_coor_sing, iterations = 30, pb = FALSE, ovlp.method = "duration")
  
  expect_true(nrow(tc) == 3 & ncol(tc) == 5)
})
