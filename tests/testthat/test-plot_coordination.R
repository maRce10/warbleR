test_that("basic", {

  # load simulate singing events (see data documentation)
  data(sim_coor_sing)

  #' # make plot_coordination in graphic device format
  cgs <- plot_coordination(X = sim_coor_sing, ovlp = TRUE, only.coor = FALSE, img = FALSE)
  
  expect_true(is.list(cgs) & length(cgs) == 3)

})