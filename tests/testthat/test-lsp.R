test_that("lsp_status returns one row per known editor", {
  res <- lsp_status()
  expect_s3_class(res, "tbl_df")
  expect_setequal(
    res$editor,
    c("vscode", "cursor", "positron", "rstudio", "neovim", "helix", "zed")
  )
})


test_that("install_lsp writes a config snippet and reports nicely", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile(),
    R_USER_CONFIG_DIR = tempfile()
  )
  fake_compiler <- tempfile()
  file.create(fake_compiler)
  withr::local_options(quone.compiler_path = fake_compiler)
  withr::local_envvar(QUONEC = fake_compiler)

  expect_message(
    res <- install_lsp("neovim"),
    regexp = "Setting up Quone editor support|Editor support summary"
  )
  expect_s3_class(res, "tbl_df")
  expect_true(all(file.exists(res$config_path)))
  expect_true(all(is.na(res$extension_installed)))
})


test_that("install_lsp(quiet = TRUE) suppresses per-step chatter", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile(),
    R_USER_CONFIG_DIR = tempfile()
  )
  fake_compiler <- tempfile()
  file.create(fake_compiler)
  withr::local_options(quone.compiler_path = fake_compiler)
  withr::local_envvar(QUONEC = fake_compiler)

  msgs <- testthat::capture_messages(
    res <- install_lsp("neovim", quiet = TRUE)
  )
  expect_s3_class(res, "tbl_df")
  expect_false(any(grepl("Setting up Quone editor support", msgs)))
  expect_true(any(grepl("Editor support summary", msgs)))
})


test_that("install_lsp shows actionable advice when no editors are found", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile(),
    R_USER_CONFIG_DIR = tempfile()
  )
  fake_compiler <- tempfile()
  file.create(fake_compiler)
  withr::local_options(quone.compiler_path = fake_compiler)
  withr::local_envvar(QUONEC = fake_compiler)

  testthat::local_mocked_bindings(
    detect_editors = function() character(),
    .package = "quone"
  )

  expect_message(
    res <- install_lsp("auto"),
    regexp = "No supported editors detected"
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0L)
})
