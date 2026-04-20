test_that("setup() is non-interactive when prompt = FALSE", {
  withr::local_envvar(QUONEC = "")
  withr::local_options(quone.compiler_path = "")

  fake_compiler <- tempfile()
  file.create(fake_compiler)
  on.exit(unlink(fake_compiler), add = TRUE)

  missing_path <- tempfile()
  testthat::local_mocked_bindings(
    install_compiler = function(...) {
      options(quone.compiler_path = fake_compiler)
      Sys.setenv(QUONEC = fake_compiler)
      invisible(fake_compiler)
    },
    user_compiler_path = function() missing_path,
    detect_editors = function() character(),
    .package = "quone"
  )

  msgs <- testthat::capture_messages(
    res <- setup(prompt = FALSE)
  )
  expect_match(msgs, "Welcome to", all = FALSE)
  expect_match(msgs, "Setup summary", all = FALSE)
  expect_type(res, "list")
  expect_named(res, c("compiler", "lsp"))
  expect_true(file.exists(res$compiler))
  expect_s3_class(res$lsp, "tbl_df")
  expect_equal(nrow(res$lsp), 0L)
})


test_that("setup() reports an existing compiler without reinstalling", {
  fake_compiler <- tempfile()
  file.create(fake_compiler)
  on.exit(unlink(fake_compiler), add = TRUE)

  withr::local_options(quone.compiler_path = fake_compiler)
  withr::local_envvar(QUONEC = fake_compiler)

  reinstalled <- FALSE
  testthat::local_mocked_bindings(
    install_compiler = function(...) {
      reinstalled <<- TRUE
      invisible(fake_compiler)
    },
    detect_editors = function() character(),
    .package = "quone"
  )

  msgs <- testthat::capture_messages(
    res <- setup(prompt = FALSE)
  )
  expect_match(msgs, "Found .*quonec.* nothing to do", all = FALSE)
  expect_false(reinstalled)
  expect_equal(res$compiler, normalizePath(fake_compiler))
})


test_that("compiler_path() finds an install left by a previous session", {
  withr::local_options(quone.compiler_path = "")
  withr::local_envvar(QUONEC = "")

  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
  fake_bin <- file.path(data_dir, "quonec")
  file.create(fake_bin)

  testthat::local_mocked_bindings(
    user_compiler_path = function() fake_bin,
    .package = "quone"
  )

  expect_equal(
    normalizePath(compiler_path()),
    normalizePath(fake_bin)
  )
})
