test_that("pkgdown_site errors when pkgdown is missing", {
  if (requireNamespace("pkgdown", quietly = TRUE)) {
    succeed("pkgdown is installed; nothing to assert in unit tests")
  } else {
    expect_error(pkgdown_site("."), "pkgdown")
  }
})
