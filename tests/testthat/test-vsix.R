test_that("install_lsp() builds and installs the bundled extension when missing", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile(),
    R_USER_CONFIG_DIR = tempfile()
  )

  fake_compiler <- tempfile()
  file.create(fake_compiler)
  withr::local_options(quone.compiler_path = fake_compiler)
  withr::local_envvar(QUONEC = fake_compiler)

  fake_editor_cli <- tempfile()
  file.create(fake_editor_cli)
  fake_src <- tempfile()
  dir.create(file.path(fake_src, "dist"), recursive = TRUE)
  fake_vsix <- file.path(fake_src, "dist", "quone-0.0.1.vsix")
  file.create(fake_vsix)

  build_called <- FALSE
  install_called <- character()
  uninstall_called <- character()

  testthat::local_mocked_bindings(
    detect_editors = function() "cursor",
    editor_binary = function(name) fake_editor_cli,
    find_bundled_vsix = function() NULL,
    find_vsix_source = function() fake_src,
    build_vsix = function(src = NULL, quiet = FALSE) {
      build_called <<- TRUE
      fake_vsix
    },
    .package = "quone"
  )

  testthat::local_mocked_bindings(
    run = function(cmd, args = character(), ...) {
      flag <- args[args %in% c("--install-extension",
                               "--uninstall-extension",
                               "--list-extensions")]
      if (length(flag) == 1L && flag == "--install-extension") {
        install_called <<- c(install_called, args[[2L]])
      } else if (length(flag) == 1L && flag == "--uninstall-extension") {
        uninstall_called <<- c(uninstall_called, args[[2L]])
      }
      list(status = 0L, stdout = "", stderr = "")
    },
    .package = "processx"
  )

  msgs <- testthat::capture_messages(
    res <- install_lsp("cursor", quiet = TRUE)
  )

  expect_true(build_called)
  expect_equal(install_called, fake_vsix)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$editor, "cursor")
  expect_true(isTRUE(res$installed))
  expect_true(isTRUE(res$extension_installed))
  expect_match(msgs, "Editor support summary", all = FALSE)
  expect_match(msgs, "ready", all = FALSE)
})


test_that("install_lsp() removes a stale quone.quone-lang extension first", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile(),
    R_USER_CONFIG_DIR = tempfile()
  )
  fake_compiler <- tempfile()
  file.create(fake_compiler)
  withr::local_options(quone.compiler_path = fake_compiler)
  withr::local_envvar(QUONEC = fake_compiler)

  fake_editor_cli <- tempfile()
  file.create(fake_editor_cli)
  fake_vsix <- tempfile(fileext = ".vsix")
  file.create(fake_vsix)

  uninstall_called <- character()

  testthat::local_mocked_bindings(
    detect_editors = function() "cursor",
    editor_binary = function(name) fake_editor_cli,
    find_bundled_vsix = function() fake_vsix,
    .package = "quone"
  )

  testthat::local_mocked_bindings(
    run = function(cmd, args = character(), ...) {
      flag <- args[args %in% c("--install-extension",
                               "--uninstall-extension",
                               "--list-extensions")]
      if (length(flag) == 1L && flag == "--list-extensions") {
        return(list(
          status = 0L,
          stdout = "ms-python.python\nquone.quone-lang\n",
          stderr = ""
        ))
      }
      if (length(flag) == 1L && flag == "--uninstall-extension") {
        uninstall_called <<- c(uninstall_called, args[[2L]])
      }
      list(status = 0L, stdout = "", stderr = "")
    },
    .package = "processx"
  )

  install_lsp("cursor", quiet = TRUE)

  expect_equal(uninstall_called, "quone.quone-lang")
})


test_that("install_lsp() reports honestly when extension install fails", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile(),
    R_USER_CONFIG_DIR = tempfile()
  )
  fake_compiler <- tempfile()
  file.create(fake_compiler)
  withr::local_options(quone.compiler_path = fake_compiler)
  withr::local_envvar(QUONEC = fake_compiler)

  fake_editor_cli <- tempfile()
  file.create(fake_editor_cli)
  fake_vsix <- tempfile(fileext = ".vsix")
  file.create(fake_vsix)

  testthat::local_mocked_bindings(
    detect_editors = function() "cursor",
    editor_binary = function(name) fake_editor_cli,
    find_bundled_vsix = function() fake_vsix,
    .package = "quone"
  )

  testthat::local_mocked_bindings(
    run = function(cmd, args = character(), ...) {
      flag <- args[args %in% c("--install-extension",
                               "--uninstall-extension",
                               "--list-extensions")]
      if (length(flag) == 1L && flag == "--install-extension") {
        return(list(
          status = 1L,
          stdout = "",
          stderr = "Could not install extension."
        ))
      }
      list(status = 0L, stdout = "", stderr = "")
    },
    .package = "processx"
  )

  msgs <- testthat::capture_messages(
    res <- install_lsp("cursor", quiet = TRUE)
  )

  expect_false(isTRUE(res$installed))
  expect_match(msgs, "extension wasn't installed", all = FALSE)
  expect_match(msgs, "Re-run", all = FALSE)
})


test_that("build_vsix() errors when neither npm nor source are available", {
  testthat::local_mocked_bindings(
    find_vsix_source = function() NULL,
    .package = "quone"
  )
  expect_error(build_vsix(), regexp = "Couldn't find the extension source")
})


test_that("a real build_vsix() produces a self-contained bundle", {
  testthat::skip_if_offline()
  testthat::skip_if(
    !nzchar(Sys.which("npm")),
    "npm not on PATH"
  )
  src <- find_vsix_source()
  if (is.null(src)) {
    candidate <- normalizePath(
      file.path(testthat::test_path("..", "..", "..", "compiler",
                                    "editors", "vscode")),
      mustWork = FALSE
    )
    if (dir.exists(candidate)) src <- candidate
  }
  testthat::skip_if(is.null(src), "no extension source checkout")

  vsix <- build_vsix(src = src, quiet = TRUE)
  expect_true(file.exists(vsix))

  scratch <- tempfile()
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  utils::unzip(vsix, exdir = scratch)
  bundled_js <- file.path(
    scratch, "extension", "dist", "extension.js"
  )
  expect_true(file.exists(bundled_js))
  expect_gt(
    file.size(bundled_js),
    100 * 1024L
  )
  bundled_text <- paste(
    readLines(bundled_js, warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    bundled_text,
    "vscode-languageclient",
    fixed = TRUE
  )
  expect_false(
    file.exists(file.path(scratch, "extension", "node_modules"))
  )
})


test_that("install_lsp() writes quone.compilerPath into editor user settings", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile(),
    R_USER_CONFIG_DIR = tempfile()
  )
  fake_compiler <- tempfile()
  file.create(fake_compiler)
  withr::local_options(quone.compiler_path = fake_compiler)
  withr::local_envvar(QUONEC = fake_compiler)

  fake_editor_cli <- tempfile()
  file.create(fake_editor_cli)
  fake_vsix <- tempfile(fileext = ".vsix")
  file.create(fake_vsix)

  fake_settings <- tempfile(fileext = ".json")

  testthat::local_mocked_bindings(
    detect_editors = function() "cursor",
    editor_binary = function(name) fake_editor_cli,
    find_bundled_vsix = function() fake_vsix,
    editor_user_settings_path = function(editor) fake_settings,
    .package = "quone"
  )

  testthat::local_mocked_bindings(
    run = function(cmd, args = character(), ...) {
      list(status = 0L, stdout = "", stderr = "")
    },
    .package = "processx"
  )

  install_lsp("cursor", quiet = TRUE)

  expect_true(file.exists(fake_settings))
  written <- jsonlite::fromJSON(fake_settings)
  expect_equal(
    normalizePath(written[["quone.compilerPath"]]),
    normalizePath(fake_compiler)
  )
})


test_that("settings.json writer preserves existing keys", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile(),
    R_USER_CONFIG_DIR = tempfile()
  )
  fake_compiler <- tempfile()
  file.create(fake_compiler)
  withr::local_options(quone.compiler_path = fake_compiler)
  withr::local_envvar(QUONEC = fake_compiler)

  fake_editor_cli <- tempfile()
  file.create(fake_editor_cli)
  fake_vsix <- tempfile(fileext = ".vsix")
  file.create(fake_vsix)
  fake_settings <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      "editor.fontSize" = 14,
      "workbench.colorTheme" = "Light Modern"
    ),
    fake_settings,
    auto_unbox = TRUE
  )

  testthat::local_mocked_bindings(
    detect_editors = function() "cursor",
    editor_binary = function(name) fake_editor_cli,
    find_bundled_vsix = function() fake_vsix,
    editor_user_settings_path = function(editor) fake_settings,
    .package = "quone"
  )
  testthat::local_mocked_bindings(
    run = function(cmd, args = character(), ...) {
      list(status = 0L, stdout = "", stderr = "")
    },
    .package = "processx"
  )

  install_lsp("cursor", quiet = TRUE)

  written <- jsonlite::fromJSON(fake_settings)
  expect_equal(written[["editor.fontSize"]], 14)
  expect_equal(written[["workbench.colorTheme"]], "Light Modern")
  expect_equal(
    normalizePath(written[["quone.compilerPath"]]),
    normalizePath(fake_compiler)
  )
})
