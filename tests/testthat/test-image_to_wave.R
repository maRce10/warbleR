test_that("basic", {

    # empty plot
  plot(0, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))

  # text to include
  text <- " warbleR "

  # add text
  text(x = 0.5, y = 0.5, labels = text, cex = 11, font = 1)

  # save image in temporary directory
  dev2bitmap(file.path(tempdir(), "temp-img.png"), type = "pngmono", res = 30)

  # read it
  wv <- image_to_wave(file = file.path(tempdir(), "temp-img.png"), plot = TRUE, flim = c(1, 12))

  dev.off()
  
  expect_true(is(wv, "Wave"))

  
      
})
