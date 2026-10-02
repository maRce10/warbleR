test_that("query_xc() is deprecated", {
  expect_warning(out <- query_xc("Phaethornis anthophilus", download = FALSE), "suwo")
  expect_null(out)
})

test_that("map_xc() is deprecated", {
  expect_warning(out <- map_xc(data.frame(), img = FALSE), "suwo")
  expect_null(out)
})
