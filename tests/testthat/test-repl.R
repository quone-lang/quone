test_that("detect_project picks up a quone.toml in the working dir", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  create_project(tmp, name = "demo")
  withr::with_dir(tmp, {
    expect_equal(quone:::detect_project(), ".")
  })
})


test_that("detect_project returns NULL elsewhere", {
  withr::with_tempfile("td", {
    dir.create(td)
    withr::with_dir(td, {
      expect_null(quone:::detect_project())
    })
  })
})
