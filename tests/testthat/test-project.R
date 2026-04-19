test_that("create_project writes a quone.toml and src/Main.Q", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  create_project(tmp, name = "demo")
  expect_true(file.exists(file.path(tmp, "quone.toml")))
  expect_true(file.exists(file.path(tmp, "src", "Main.Q")))

  toml <- readLines(file.path(tmp, "quone.toml"))
  expect_true(any(grepl("name = \"demo\"", toml)))
})


test_that("create_module rejects lowercase module path segments", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  create_project(tmp, name = "demo")
  expect_error(create_module("stats.summary", project = tmp))
})


test_that("create_module mirrors dotted paths to nested directories", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  create_project(tmp, name = "demo")
  path <- create_module("Stats.Summary", project = tmp)
  expect_true(file.exists(path))
  expect_equal(
    normalizePath(path),
    normalizePath(file.path(tmp, "src", "Stats", "Summary.Q"))
  )
})


test_that("add_dependency appends to [dependencies]", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  create_project(tmp, name = "demo")
  add_dependency("dplyr", ">= 1.1", project = tmp)
  toml <- readLines(file.path(tmp, "quone.toml"))
  expect_true(any(grepl("dplyr = \">= 1.1\"", toml)))
})


test_that("remove_dependency drops the matching line", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  create_project(tmp, name = "demo")
  add_dependency("dplyr", ">= 1.1", project = tmp)
  add_dependency("purrr", project = tmp)
  remove_dependency("dplyr", project = tmp)
  toml <- readLines(file.path(tmp, "quone.toml"))
  expect_false(any(grepl("dplyr =", toml)))
  expect_true(any(grepl("purrr =", toml)))
})
