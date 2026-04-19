test_that("fmt is a no-op on canonical input", {
  skip_if_no_compiler()
  tmp <- tempfile(fileext = ".Q")
  on.exit(unlink(tmp))
  writeLines("main <- 1\n", tmp)
  expect_true(fmt(tmp))
})


test_that("fmt_string returns formatted output", {
  skip_if_no_compiler()
  out <- fmt_string("main <- 1")
  expect_type(out, "character")
})
