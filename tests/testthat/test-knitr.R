test_that("knit_engine_quone is registered when knitr is available", {
  skip_if_not_installed("knitr")
  engines <- knitr::knit_engines$get()
  expect_true("quone" %in% names(engines))
})
