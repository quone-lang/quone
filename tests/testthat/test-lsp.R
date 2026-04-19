test_that("lsp_status returns one row per known editor", {
  res <- lsp_status()
  expect_s3_class(res, "tbl_df")
  expect_setequal(
    res$editor,
    c("vscode", "positron", "rstudio", "neovim", "helix", "zed")
  )
})


test_that("install_lsp writes a config snippet", {
  skip_if_no_compiler()
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile(),
    R_USER_CONFIG_DIR = tempfile()
  )
  res <- install_lsp("neovim")
  expect_s3_class(res, "tbl_df")
  expect_true(all(file.exists(res$config_path)))
})
