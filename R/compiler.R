#' Locate the `quonec` compiler binary
#' @param path Optional explicit path to a `quonec` binary.
#' @param error If `TRUE`, raise an error when no compiler is found.
#' @return A path to `quonec`, or `NULL` when `error = FALSE`.
#' @export
compiler_path <- function(path = NULL, error = TRUE) {
  if (!is.null(path)) {
    if (file.exists(path)) return(normalizePath(path, mustWork = TRUE))
    if (error) stop("compiler path does not exist: ", path, call. = FALSE)
    return(NULL)
  }

  candidate <- getOption("quone.compiler_path")
  if (is.null(candidate) || !nzchar(candidate)) {
    candidate <- Sys.getenv("QUONEC", unset = "")
  }
  if (!nzchar(candidate)) {
    candidate <- user_compiler_path()
  }
  if (!file.exists(candidate)) {
    candidate <- unname(Sys.which("quonec"))
  }
  if (length(candidate) == 0 || !nzchar(candidate) || !file.exists(candidate)) {
    if (error) {
      stop(
        "Could not find `quonec`. Run quone::install_compiler() or set QUONEC.",
        call. = FALSE
      )
    }
    return(NULL)
  }

  normalizePath(candidate, mustWork = TRUE)
}

user_compiler_path <- function() {
  file.path(tools::R_user_dir("quone", "data"), "quonec")
}

#' Install the `quonec` compiler
#' @param version Release version to install. `"latest"` downloads the latest
#'   GitHub release asset.
#' @param source Install source. Use `"github-release"` for release binaries or
#'   `"build-from-source"` for a sibling compiler checkout.
#' @param compiler_dir Optional compiler checkout used when
#'   `source = "build-from-source"`.
#' @return Invisibly, the installed compiler path.
#' @export
install_compiler <- function(
  version = "latest",
  source = c("github-release", "build-from-source"),
  compiler_dir = NULL
) {
  source <- match.arg(source)
  dest_dir <- tools::R_user_dir("quone", "data")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- user_compiler_path()

  if (source == "github-release") {
    asset <- release_asset_url(version)
    archive <- tempfile(fileext = ".tar.gz")
    on.exit(unlink(archive), add = TRUE)
    ok <- tryCatch({
      utils::download.file(asset, archive, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (ok && file.size(archive) > 0) {
      tmpdir <- tempfile()
      dir.create(tmpdir)
      on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
      utils::untar(archive, exdir = tmpdir)
      candidate <- list.files(
        tmpdir,
        pattern = "^quonec(\\.exe)?$",
        recursive = TRUE,
        full.names = TRUE
      )
      if (length(candidate) > 0) {
        file.copy(candidate[[1]], dest, overwrite = TRUE)
        Sys.chmod(dest, mode = "0755")
        options(quone.compiler_path = dest)
        Sys.setenv(QUONEC = dest)
        return(invisible(dest))
      }
    }
    stop(
      paste0(
        "Could not download a prebuilt Quone compiler for this platform.\n",
        "Tried: ", asset, "\n\n",
        "The GitHub release may still be building, or this platform may not ",
        "have a published binary for version `", version, "`.\n",
        "Wait for the release assets to finish publishing and try again, or ",
        "run `quone::install_compiler(source = \"build-from-source\")` from a ",
        "checkout with a sibling `compiler/` directory."
      ),
      call. = FALSE
    )
  }

  cabal <- unname(Sys.which("cabal"))
  if (length(cabal) == 0 || !nzchar(cabal)) {
    stop("Could not find `cabal` on PATH.", call. = FALSE)
  }
  sibling <- find_compiler_checkout(compiler_dir)
  if (is.null(sibling)) {
    stop(
      paste0(
        "Could not find a compiler checkout to build from.\n",
        "Pass `compiler_dir = \"/path/to/compiler\"`, set the ",
        "`QUONE_COMPILER_DIR` environment variable, or run from a checkout ",
        "with a sibling `compiler/` directory."
      ),
      call. = FALSE
    )
  }
  old_wd <- getwd()
  setwd(sibling)
  on.exit(setwd(old_wd), add = TRUE)

  stage_dir <- file.path(tempdir(), paste0("quone-cabal-install-", Sys.getpid()))
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)

  res <- system2(
    cabal,
    c(
      "install",
      "exe:quonec",
      paste0("--installdir=", stage_dir),
      "--overwrite-policy=always",
      "--install-method=copy"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    stop("`cabal install` failed:\n", paste(res, collapse = "\n"), call. = FALSE)
  }
  staged <- file.path(stage_dir, if (.Platform$OS.type == "windows") "quonec.exe" else "quonec")
  if (!file.exists(staged)) {
    stop("`cabal install` did not produce a quonec binary.", call. = FALSE)
  }
  copied <- file.copy(staged, dest, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stop("Could not copy quonec into ", dest, call. = FALSE)
  }
  Sys.chmod(dest, mode = "0755")
  options(quone.compiler_path = dest)
  Sys.setenv(QUONEC = dest)
  invisible(dest)
}

#' Install the Quone VS Code extension
#' @param editor Editor command to install into: `"code"`, `"cursor"`, or
#'   `"positron"`.
#' @param version Extension release version. `"latest"` downloads the latest
#'   GitHub release asset.
#' @param source Install source. Use `"github-release"` for a published VSIX or
#'   `"build-from-source"` for a sibling compiler checkout.
#' @param extension_dir Optional path to the VS Code extension source directory.
#' @param compiler Optional explicit path to a `quonec` binary.
#' @return Invisibly, a list with the editor, extension, compiler, and settings
#'   paths used.
#' @export
install_lsp <- function(
  editor = c("auto", "code", "cursor", "positron"),
  version = "latest",
  source = c("github-release", "build-from-source"),
  extension_dir = NULL,
  compiler = NULL
) {
  editor <- match.arg(editor)
  source <- match.arg(source)
  if (identical(editor, "auto")) {
    editor <- detect_editor()
  }

  compiler_bin <- compiler_path(compiler, error = !is.null(compiler))
  if (is.null(compiler_bin)) {
    install_compiler()
    compiler_bin <- compiler_path()
  }

  vsix <- NULL
  if (source == "github-release") {
    vsix <- download_extension_vsix(version)
    if (!is.null(vsix)) on.exit(unlink(vsix), add = TRUE)
    if (is.null(vsix)) {
      message(
        "Could not download quone-vscode.vsix from the GitHub release; ",
        "building the extension from a local checkout instead."
      )
      source <- "build-from-source"
    }
  }
  if (source == "build-from-source") {
    vsix <- build_extension_vsix(extension_dir)
  }

  editor_bin <- editor_command(editor)
  run_checked(
    editor_bin,
    c("--install-extension", vsix, "--force"),
    sprintf("%s --install-extension", editor)
  )

  settings <- write_editor_compiler_setting(editor, compiler_bin)
  message(
    "Installed Quone extension for ", editor,
    " and set quone.compilerPath to ", compiler_bin
  )
  invisible(list(
    editor = editor_bin,
    extension = vsix,
    compiler = compiler_bin,
    settings = settings
  ))
}

#' Start a guided first Quone run
#' @param path Output path for the bundled demo file.
#' @param editor Editor command to use for setup and opening files. `"auto"`
#'   detects `cursor`, `code`, or `positron`; `"r"` uses R's file editor.
#' @param install_lsp Whether to offer installing editor/LSP support.
#' @param overwrite Whether to replace an existing demo file.
#' @param ask Whether to ask before each step. Defaults to interactive sessions.
#' @return Invisibly, a list with the compiler, demo, generated R, and editor
#'   setup results.
#' @export
start <- function(
  path = "mtcars_summary.Q",
  editor = c("auto", "cursor", "code", "positron", "r"),
  install_lsp = TRUE,
  overwrite = FALSE,
  ask = interactive()
) {
  editor <- match.arg(editor)
  if (isTRUE(ask) && !interactive()) {
    cli::cli_abort(c(
      "Cannot run the guided setup prompts in a non-interactive session.",
      "i" = "Use {.code quone::start(ask = FALSE)} to run the default setup without prompts."
    ))
  }

  cli::cli_h1("Welcome to Quone")
  cli::cli_alert_info(
    "This guided setup will install Quone tooling, write a demo {.file .Q} file, compile it, and open the generated R."
  )

  compiler_bin <- start_setup_compiler(ask)
  lsp_result <- start_setup_lsp(editor, compiler_bin, install_lsp, ask)
  demo_path <- start_write_demo(path, overwrite, ask)

  start_open_step(
    demo_path,
    editor,
    ask,
    "Open the Quone demo so you can see the source before it is compiled?"
  )

  cli::cli_alert_info(
    "Quone will check {.path {demo_path}} and compile it to readable R."
  )
  confirm_step("Check and compile the demo now?", ask = ask)
  check(demo_path)
  generated <- compile(demo_path)
  cli::cli_alert_success("Compiled {.path {demo_path}} to {.path {generated}}.")

  start_open_step(
    generated,
    editor,
    ask,
    "Open the generated R file now?"
  )

  compile_call <- sprintf("quone::compile(%s)", deparse1(demo_path))
  cli::cli_h1("Quone is ready")
  cli::cli_alert_success("You now have a checked Quone file and the readable R it generated.")
  cli::cli_alert_info(paste0(
    "Next, try editing ", demo_path, " and running ", compile_call, " again."
  ))

  invisible(list(
    compiler = compiler_bin,
    lsp = lsp_result,
    source = demo_path,
    output = generated
  ))
}

#' Write a bundled demo `.Q` file
#' @param path Output path for the demo file.
#' @param name Bundled demo name.
#' @param overwrite If `FALSE`, stop when `path` already exists.
#' @return Invisibly, the written path.
#' @export
write_demo <- function(
  path = "mtcars_summary.Q",
  name = c("mtcars_summary"),
  overwrite = FALSE
) {
  name <- match.arg(name)
  src <- system.file("examples", paste0(name, ".Q"), package = "quone")
  if (!nzchar(src) || !file.exists(src)) {
    stop(
      "Bundled demo `", name, "` was not found in the installed quone package.",
      call. = FALSE
    )
  }
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("demo output already exists: ", path, call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  file.copy(src, path, overwrite = TRUE)
  invisible(normalizePath(path, mustWork = TRUE))
}

#' Compile a `.Q` file to `.R`
#' @param input Path to a `.Q` file.
#' @param output Optional output file or output directory.
#' @param style If `TRUE`, run `styler::style_file()` on generated R when
#'   the optional `styler` package is installed.
#' @return Invisibly, the expected generated `.R` path.
#' @export
compile <- function(input, output = NULL, style = TRUE) {
  args <- c("compile", input)
  out_dir <- output
  if (!is.null(output) && !dir.exists(output)) {
    out_dir <- dirname(output)
  }
  if (!is.null(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    args <- c(args, paste0("--out=", out_dir))
  }
  res <- invoke_compiler(args)
  stop_for_compiler_error(res)
  generated <- if (is.null(out_dir)) {
    sub("\\.Q$", ".R", input)
  } else {
    file.path(out_dir, sub("\\.Q$", ".R", basename(input)))
  }
  if (!is.null(output) && !dir.exists(output) && generated != output) {
    file.rename(generated, output)
    generated <- output
  }
  style_generated_r(generated, style)
  invisible(generated)
}

#' Compile all `.Q` files under a directory
#' @param input_dir Directory to search recursively.
#' @param output_dir Directory for generated `.R` files.
#' @param style If `TRUE`, run `styler::style_file()` on generated R when
#'   the optional `styler` package is installed.
#' @return Invisibly, `output_dir`.
#' @export
compile_dir <- function(input_dir, output_dir, style = TRUE) {
  q_files <- list.files(input_dir, pattern = "\\.Q$", recursive = TRUE, full.names = TRUE)
  generated <- file.path(output_dir, sub("\\.Q$", ".R", basename(q_files)))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  res <- invoke_compiler(c("compile-dir", input_dir, paste0("--out=", output_dir)))
  stop_for_compiler_error(res)
  style_generated_r(generated[file.exists(generated)], style)
  invisible(output_dir)
}

#' Check a `.Q` file without writing generated R
#' @param input Path to a `.Q` file.
#' @return Invisibly, `TRUE`.
#' @export
check <- function(input) {
  res <- invoke_compiler(c("check", input))
  stop_for_compiler_error(res)
  invisible(TRUE)
}

#' Format a `.Q` file or directory in place
#' @param input Path to a `.Q` file or directory.
#' @return Invisibly, `TRUE`.
#' @export
fmt <- function(input) {
  res <- invoke_compiler(c("fmt", input), json = FALSE)
  stop_for_compiler_error(res)
  invisible(TRUE)
}

#' Invoke the compiler
#' @param args Character vector of arguments for `quonec`.
#' @param json Whether to request JSON diagnostics.
#' @param compiler Optional compiler path.
#' @return A list with `status`, `stdout`, and `stderr`.
#' @export
invoke_compiler <- function(args, json = TRUE, compiler = NULL) {
  bin <- compiler_path(compiler)
  if (isTRUE(json) && !any(grepl("^--diagnostics-format=", args)) &&
      !any(args %in% c("version", "--version", "--help", "fmt", "lsp"))) {
    args <- c("--diagnostics-format=json", args)
  }
  out <- tempfile()
  err <- tempfile()
  on.exit(unlink(c(out, err)), add = TRUE)
  status <- system2(bin, args, stdout = out, stderr = err)
  list(
    status = status,
    stdout = paste(readLines(out, warn = FALSE), collapse = "\n"),
    stderr = paste(readLines(err, warn = FALSE), collapse = "\n")
  )
}

stop_for_compiler_error <- function(res) {
  if (!identical(res$status, 0L)) {
    msg <- res$stderr
    if (!nzchar(msg)) msg <- res$stdout
    stop(msg, call. = FALSE)
  }
}

style_generated_r <- function(paths, style = TRUE) {
  paths <- paths[file.exists(paths)]
  if (!isTRUE(style) || length(paths) == 0 || !requireNamespace("styler", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  invisible(utils::capture.output(styler::style_file(paths)))
  invisible(TRUE)
}

start_setup_compiler <- function(ask) {
  compiler_bin <- compiler_path(error = FALSE)
  if (!is.null(compiler_bin)) {
    cli::cli_alert_success("Found an existing Quone compiler at {.path {compiler_bin}}.")
    if (confirm_step("Use this compiler for the demo?", ask = ask, required = FALSE)) {
      return(compiler_bin)
    }
    if (!confirm_step(
      "Install a fresh Quone compiler now?",
      ask = ask,
      required = FALSE
    )) {
      cli::cli_alert_info("Keeping the existing compiler. You can reinstall later with {.code quone::install_compiler()}.")
      return(compiler_bin)
    }
  } else {
    cli::cli_alert_info(
      "Quone needs the {.code quonec} compiler before it can check or compile {.file .Q} files."
    )
    confirm_step("Install the Quone compiler now?", ask = ask)
  }

  compiler_bin <- install_compiler()
  cli::cli_alert_success("Installed the Quone compiler at {.path {compiler_bin}}.")
  compiler_bin
}

start_setup_lsp <- function(editor, compiler_bin, install_lsp, ask) {
  if (!isTRUE(install_lsp)) {
    cli::cli_alert_info("Skipping editor/LSP setup because {.code install_lsp = FALSE}.")
    return(NULL)
  }
  if (identical(editor, "r")) {
    cli::cli_alert_warning(
      "Skipping editor/LSP setup because {.code editor = \"r\"} uses R's file editor."
    )
    return(NULL)
  }

  cli::cli_alert_info(
    "Quone can install its VS Code-compatible extension and point it at {.path {compiler_bin}}."
  )
  if (!confirm_step("Install editor support now?", ask = ask, required = FALSE)) {
    cli::cli_alert_info("Skipped editor support. You can run {.code quone::install_lsp()} later.")
    return(NULL)
  }

  install_lsp_fn <- get("install_lsp", envir = environment(start_setup_lsp))
  result <- tryCatch(
    install_lsp_fn(editor = editor, compiler = compiler_bin),
    error = function(err) {
      cli::cli_alert_warning("Could not install editor support: {conditionMessage(err)}")
      cli::cli_alert_info(
        "Install the {.code cursor}, {.code code}, or {.code positron} CLI command, then run {.code quone::install_lsp()}."
      )
      NULL
    }
  )
  if (!is.null(result)) {
    cli::cli_alert_success("Installed Quone editor support.")
  }
  result
}

start_write_demo <- function(path, overwrite, ask) {
  write_overwrite <- isTRUE(overwrite)
  if (file.exists(path) && !write_overwrite) {
    cli::cli_alert_warning("The demo file already exists at {.path {path}}.")
    if (!isTRUE(ask)) {
      cli::cli_abort(c(
        "Cannot write the demo because {.path {path}} already exists.",
        "i" = "Choose another file with {.code quone::start(path = \"my_demo.Q\")} or set {.code overwrite = TRUE}."
      ))
    } else if (confirm_step("Overwrite this file with the bundled Quone demo?", ask = ask, required = FALSE)) {
      write_overwrite <- TRUE
    } else {
      cli::cli_abort(c(
        "Cannot write the demo without replacing {.path {path}}.",
        "i" = "Choose another file with {.code quone::start(path = \"my_demo.Q\")} or set {.code overwrite = TRUE}."
      ))
    }
  }

  cli::cli_alert_info("Quone will write a small bundled demo to {.path {path}}.")
  confirm_step("Write the demo file now?", ask = ask)
  demo_path <- write_demo(path, overwrite = write_overwrite)
  cli::cli_alert_success("Wrote the Quone demo to {.path {demo_path}}.")
  demo_path
}

start_open_step <- function(path, editor, ask, prompt) {
  cli::cli_alert_info("Quone can open {.path {path}} for you.")
  if (!confirm_step(prompt, ask = ask, required = FALSE)) {
    cli::cli_alert_info("Skipped opening {.path {path}}.")
    return(invisible(FALSE))
  }
  if (open_quone_file(path, editor = editor, ask = ask)) {
    cli::cli_alert_success("Opened {.path {path}}.")
    return(invisible(TRUE))
  }
  cli::cli_alert_warning("Could not open {.path {path}} automatically.")
  invisible(FALSE)
}

confirm_step <- function(prompt, ask = interactive(), required = TRUE, decline = NULL) {
  if (!isTRUE(ask)) {
    return(TRUE)
  }
  ok <- isTRUE(cli::cli_confirm(prompt, default = TRUE))
  if (ok) {
    return(TRUE)
  }
  if (isTRUE(required)) {
    if (is.null(decline)) {
      decline <- "No changes were made for this step. Run {.code quone::start()} again when you are ready."
    }
    cli::cli_abort(decline)
  }
  FALSE
}

open_quone_file <- function(path, editor = c("auto", "cursor", "code", "positron", "r"), ask = interactive()) {
  editor <- match.arg(editor)
  if (!interactive()) {
    cli::cli_alert_info("Skipping file opening because this R session is not interactive.")
    return(FALSE)
  }

  if (identical(editor, "r")) {
    utils::file.edit(path)
    return(TRUE)
  }

  selected <- editor
  if (identical(editor, "auto")) {
    selected <- tryCatch(detect_editor(), error = function(err) NULL)
  }

  if (!is.null(selected)) {
    opened <- tryCatch({
      editor_bin <- editor_command(selected)
      res <- system2(editor_bin, normalizePath(path, mustWork = TRUE), stdout = TRUE, stderr = TRUE)
      status <- attr(res, "status")
      is.null(status) || identical(status, 0L)
    }, error = function(err) FALSE)
    if (isTRUE(opened)) {
      return(TRUE)
    }
  }

  cli::cli_alert_warning(
    "Could not find a working {.code cursor}, {.code code}, or {.code positron} command on PATH."
  )
  cli::cli_alert_info(
    "Install your editor's shell command if you want Quone to open files there."
  )
  if (confirm_step("Open the file with R's built-in file editor instead?", ask = ask, required = FALSE)) {
    utils::file.edit(path)
    return(TRUE)
  }
  FALSE
}

release_asset_url <- function(version = "latest") {
  os <- detect_os()
  arch <- detect_arch()
  base <- "https://github.com/quone-lang/compiler/releases"
  segment <- release_download_segment(version)
  sprintf("%s/%s/quonec-%s-%s.tar.gz", base, segment, os, arch)
}

detect_os <- function() {
  sys <- Sys.info()[["sysname"]]
  switch(sys, Darwin = "macos", Linux = "linux", Windows = "windows", tolower(sys))
}

detect_arch <- function() {
  m <- Sys.info()[["machine"]]
  if (m %in% c("x86_64", "amd64")) "x86_64"
  else if (m %in% c("aarch64", "arm64")) "arm64"
  else m
}

extension_asset_url <- function(version = "latest") {
  base <- "https://github.com/quone-lang/compiler/releases"
  segment <- release_download_segment(version)
  sprintf("%s/%s/quone-vscode.vsix", base, segment)
}

release_download_segment <- function(version = "latest") {
  if (identical(version, "latest")) {
    return("latest/download")
  }
  tag <- if (startsWith(version, "v")) version else paste0("v", version)
  paste0("download/", tag)
}

download_extension_vsix <- function(version = "latest") {
  asset <- extension_asset_url(version)
  vsix <- tempfile(fileext = ".vsix")
  ok <- tryCatch({
    utils::download.file(asset, vsix, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (ok && file.exists(vsix) && file.size(vsix) > 0) {
    return(vsix)
  }
  unlink(vsix)
  NULL
}

build_extension_vsix <- function(extension_dir = NULL) {
  if (is.null(extension_dir)) {
    extension_dir <- find_sibling_vscode_extension()
  }
  if (is.null(extension_dir)) {
    stop("Could not find a sibling VS Code extension checkout.", call. = FALSE)
  }

  npm <- unname(Sys.which("npm"))
  if (length(npm) == 0 || !nzchar(npm)) {
    stop("Could not find `npm` on PATH.", call. = FALSE)
  }

  run_checked(npm, "install", "npm install", cwd = extension_dir)
  run_checked(npm, c("run", "package"), "npm run package", cwd = extension_dir)
  candidates <- list.files(
    file.path(extension_dir, "dist"),
    pattern = "\\.vsix$",
    full.names = TRUE
  )
  if (length(candidates) == 0) {
    stop("Extension build did not produce a .vsix file.", call. = FALSE)
  }
  normalizePath(candidates[[1]], mustWork = TRUE)
}

editor_command <- function(editor) {
  bin <- unname(Sys.which(editor))
  if (length(bin) == 0 || !nzchar(bin)) {
    stop("Could not find `", editor, "` on PATH.", call. = FALSE)
  }
  bin
}

detect_editor <- function() {
  for (candidate in c("cursor", "code", "positron")) {
    bin <- unname(Sys.which(candidate))
    if (length(bin) > 0 && nzchar(bin)) {
      return(candidate)
    }
  }
  stop(
    "Could not find `cursor`, `code`, or `positron` on PATH. ",
    "Pass editor = \"cursor\"/\"code\"/\"positron\" after installing the CLI command.",
    call. = FALSE
  )
}

write_editor_compiler_setting <- function(editor, compiler_bin) {
  settings_path <- editor_settings_path(editor)
  dir.create(dirname(settings_path), recursive = TRUE, showWarnings = FALSE)

  settings <- list()
  if (file.exists(settings_path) && file.size(settings_path) > 0) {
    settings <- tryCatch(
      jsonlite::read_json(settings_path, simplifyVector = FALSE),
      error = function(e) {
        stop(
          "Could not parse editor settings at ", settings_path,
          ". Set `quone.compilerPath` manually to: ", compiler_bin,
          call. = FALSE
        )
      }
    )
  }
  settings[["quone.compilerPath"]] <- compiler_bin
  jsonlite::write_json(settings, settings_path, auto_unbox = TRUE, pretty = TRUE)
  normalizePath(settings_path, mustWork = TRUE)
}

editor_settings_path <- function(editor) {
  app <- switch(
    editor,
    code = "Code",
    cursor = "Cursor",
    positron = "Positron"
  )
  sys <- Sys.info()[["sysname"]]
  if (identical(sys, "Windows")) {
    appdata <- Sys.getenv("APPDATA", unset = "")
    if (!nzchar(appdata)) stop("APPDATA is not set.", call. = FALSE)
    return(file.path(appdata, app, "User", "settings.json"))
  }
  if (identical(sys, "Darwin")) {
    return(file.path(path.expand("~/Library/Application Support"), app, "User", "settings.json"))
  }
  config_home <- Sys.getenv("XDG_CONFIG_HOME", unset = path.expand("~/.config"))
  file.path(config_home, app, "User", "settings.json")
}

run_checked <- function(command, args, label, cwd = NULL) {
  old <- NULL
  if (!is.null(cwd)) {
    old <- getwd()
    setwd(cwd)
    on.exit(setwd(old), add = TRUE)
  }
  res <- system2(command, args, stdout = TRUE, stderr = TRUE)
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    stop("`", label, "` failed:\n", paste(res, collapse = "\n"), call. = FALSE)
  }
  res
}

find_compiler_checkout <- function(compiler_dir = NULL) {
  if (!is.null(compiler_dir)) {
    if (is_compiler_checkout(compiler_dir)) {
      return(normalizePath(compiler_dir, mustWork = TRUE))
    }
    stop("`compiler_dir` is not a Quone compiler checkout: ", compiler_dir, call. = FALSE)
  }

  env_dir <- Sys.getenv("QUONE_COMPILER_DIR", unset = "")
  if (nzchar(env_dir) && is_compiler_checkout(env_dir)) {
    return(normalizePath(env_dir, mustWork = TRUE))
  }

  candidates <- c(
    file.path(getwd(), "..", "compiler"),
    file.path(getwd(), "compiler"),
    file.path(path.expand("~"), "dev", "quone-lang", "compiler"),
    file.path(path.expand("~"), "quone-lang", "compiler")
  )
  for (candidate in candidates) {
    if (is_compiler_checkout(candidate)) {
      return(normalizePath(candidate))
    }
  }
  NULL
}

is_compiler_checkout <- function(path) {
  dir.exists(path) && file.exists(file.path(path, "compiler.cabal"))
}

find_sibling_vscode_extension <- function() {
  candidates <- c(
    file.path(getwd(), "..", "compiler", "editors", "vscode"),
    file.path(getwd(), "compiler", "editors", "vscode")
  )
  for (candidate in candidates) {
    if (dir.exists(candidate) && file.exists(file.path(candidate, "package.json"))) {
      return(normalizePath(candidate))
    }
  }
  NULL
}
