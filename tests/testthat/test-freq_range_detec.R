test_that("basic", {
  
dev.off()
    data(tico)
    frd <- freq_range_detec(
      wave = tico, wl = 512, fsmooth = 0.01, threshold = 1, bp = c(2, 8),
      widths = c(4, 2)
    )

  expect_equal(nrow(frd), 1)
})
