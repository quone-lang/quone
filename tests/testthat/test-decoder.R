test_that("infer_decoder turns a CSV into a Quone pipeline", {
  skip_if_not_installed("readr")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("name,age", "Alice,30", "Bob,25"), tmp)
  body <- infer_decoder(tmp)
  expect_match(body, "Csv\\.dataframe")
  expect_match(body, "\\bcharacter\\b")
})


test_that("write_decoder writes a Quone module file", {
  skip_if_not_installed("readr")
  csv <- tempfile(fileext = ".csv")
  qf <- tempfile(fileext = ".Q")
  on.exit(unlink(c(csv, qf)))
  writeLines(c("x,y", "1,2"), csv)
  out <- write_decoder(csv, qf, module_path = "Decoders.Demo",
                       binding_name = "demo")
  expect_true(file.exists(out))
  body <- paste(readLines(out), collapse = "\n")
  expect_match(body, "module Decoders\\.Demo")
})
