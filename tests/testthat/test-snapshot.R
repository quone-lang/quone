test_that("snapshot_test fails gracefully without testthat installed", {
  if (requireNamespace("testthat", quietly = TRUE)) {
    succeed("testthat is installed; covered by other tests")
  } else {
    expect_error(snapshot_test("foo.Q"), "testthat")
  }
})
