test_that("basic", {

    # no overlap
    os <- overlapping_sels(X = lbh_selec_table, pb = FALSE, verbose = FALSE)

    # modified lbh_selec_table to make the first and second selection overlap
    Y <- lbh_selec_table
    Y$end[4] <- 1.5

    os2 <- overlapping_sels(X = Y, verbose = FALSE, pb = FALSE)
  
  expect_true(all(is.na(os$ovlp.sels)) & !all(is.na(os2$ovlp.sels)))
  
})