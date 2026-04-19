test_that("install errors when neither pak nor devtools is installed", {
  if (!requireNamespace("pak", quietly = TRUE) &&
      !requireNamespace("devtools", quietly = TRUE)) {
    expect_error(install("."), "pak")
  } else {
    succeed("install requires a real project; covered in integration tests")
  }
})


test_that("test_project warns when no tests directory exists", {
  skip_if_no_compiler()
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  create_project(tmp, name = "demo")
  expect_message(
    test_project(tmp),
    regexp = "tests"
  )
})
