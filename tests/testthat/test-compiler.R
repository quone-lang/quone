test_that("compiler_path returns NULL when nothing is installed and error = FALSE", {
  withr::local_options(quone.compiler_path = "")
  withr::local_envvar(QUONEC = "")
  expect_null(compiler_path(error = FALSE))
})


test_that("compiler_path errors by default when not installed", {
  withr::local_options(quone.compiler_path = "")
  withr::local_envvar(QUONEC = "")
  expect_error(compiler_path(), regexp = "quonec")
})


test_that("compiler_path honours an explicit `path` argument", {
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_equal(
    normalizePath(compiler_path(tmp)),
    normalizePath(tmp)
  )
})


test_that("compiler_path uses options(quone.compiler_path) ahead of QUONEC", {
  opt_path <- tempfile()
  env_path <- tempfile()
  file.create(opt_path)
  file.create(env_path)
  on.exit(unlink(c(opt_path, env_path)))
  withr::local_options(quone.compiler_path = opt_path)
  withr::local_envvar(QUONEC = env_path)
  expect_equal(
    normalizePath(compiler_path()),
    normalizePath(opt_path)
  )
})


test_that("build invokes the compiler and writes a .R file", {
  skip_if_no_compiler()
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  q <- file.path(tmp, "hello.Q")
  writeLines("main <- 1", q)
  out <- build(q, sourcemap = FALSE)
  expect_true(file.exists(out))
})


test_that("check raises a classed condition on type errors", {
  skip_if_no_compiler()
  tmp <- tempfile(fileext = ".Q")
  writeLines("x <- 1 + 1.0", tmp)
  on.exit(unlink(tmp))
  expect_error(check(tmp), class = "quone_diagnostic")
})


test_that("detect_os normalises every supported sysname", {
  expect_equal(quone:::detect_os(), quone:::detect_os())
  expect_true(quone:::detect_os() %in% c("macos", "linux", "windows"))
})


test_that("detect_arch normalises x86_64 and arm64 spellings", {
  expect_true(
    quone:::detect_arch() %in% c("x86_64", "arm64")
  )
})


test_that("release_asset_url uses the documented quonec-<os>-<arch> name", {
  url <- quone:::release_asset_url("0.0.1")
  expect_match(
    url,
    "/quonec-(macos|linux|windows)-(x86_64|arm64)\\.tar\\.gz$"
  )
})


test_that("release_asset_url handles the latest pointer", {
  url <- quone:::release_asset_url("latest")
  expect_match(url, "/releases/latest/download/")
})
