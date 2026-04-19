#' Format a Quone file with `quonec fmt`
#'
#' Wraps the elm-format-style canonical formatter built into the
#' compiler (compiler track A4). When `check = TRUE`, the file is
#' read but not modified; the function returns `TRUE` if the input
#' is already canonical, `FALSE` otherwise (and prints a unified
#' diff to stderr).
#'
#' @param path Path to a `.Q` file (or a project root, in which case
#'   every `src/**/*.Q` is formatted recursively).
#' @param check Run in check mode (no rewrite).
#' @return Invisibly, `TRUE` on success / no changes needed.
#' @export
fmt <- function(path, check = FALSE) {
  args <- list("fmt")
  if (isTRUE(check)) args <- c(args, "--check")
  args <- c(args, path)
  res <- do.call(invoke_compiler, args)
  if (res$status != 0) {
    if (nzchar(res$stderr)) {
      cli::cli_alert_danger(res$stderr)
    } else {
      cli::cli_alert_danger("`quonec fmt` failed (exit {res$status})")
    }
    return(invisible(FALSE))
  }
  invisible(TRUE)
}


#' Recursively format every `.Q` file under a project's `src/`
#'
#' @param project Project root.
#' @param check Run in check mode.
#' @return Invisibly, `TRUE` if every file was already (or became)
#'   canonical.
#' @export
fmt_project <- function(project = ".", check = FALSE) {
  fmt(project, check = check)
}


#' Format a string of Quone source and return the result
#'
#' Used by the knitr engine to canonicalise `{quone}` chunks before
#' rendering. The full formatter is invoked via a temp file to avoid
#' assuming a stdin mode of the compiler.
#'
#' @param src A character string of Quone source.
#' @return The canonically-formatted source.
#' @export
fmt_string <- function(src) {
  tmp <- tempfile(fileext = ".Q")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(src, tmp)
  out <- fmt(tmp, check = FALSE)
  if (!isTRUE(out)) return(src)
  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}


#' Install a git pre-commit hook that runs `quonec fmt --check`
#'
#' @param project Project root.
#' @return Invisibly, the path to the new hook.
#' @export
use_fmt_pre_commit <- function(project = ".") {
  hook_dir <- file.path(project, ".git", "hooks")
  if (!dir.exists(hook_dir)) {
    cli::cli_abort("No {.path .git/hooks} directory; is this a git repo?")
  }
  hook_path <- file.path(hook_dir, "pre-commit")
  body <- c(
    "#!/usr/bin/env bash",
    "set -e",
    "files=$(git diff --cached --name-only --diff-filter=ACM | grep '\\.Q$' || true)",
    "if [ -n \"$files\" ]; then",
    "  for f in $files; do",
    "    quonec fmt --check \"$f\"",
    "  done",
    "fi"
  )
  writeLines(body, hook_path)
  Sys.chmod(hook_path, mode = "0755")
  cli::cli_alert_success("Installed pre-commit hook at {.path {hook_path}}")
  invisible(hook_path)
}


# ---- RStudio addins --------------------------------------------------


#' RStudio addin: format the active editor file in place
#' @keywords internal
#' @export
addin_fmt_active <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) return(invisible())
  ctx <- rstudioapi::getSourceEditorContext()
  if (is.null(ctx) || !nzchar(ctx$path)) {
    cli::cli_alert_warning("Save the file first.")
    return(invisible())
  }
  fmt(ctx$path)
}


#' RStudio addin: run `fmt --check` on the active editor file
#' @keywords internal
#' @export
addin_fmt_check_active <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) return(invisible())
  ctx <- rstudioapi::getSourceEditorContext()
  if (is.null(ctx) || !nzchar(ctx$path)) {
    cli::cli_alert_warning("Save the file first.")
    return(invisible())
  }
  fmt(ctx$path, check = TRUE)
}


#' RStudio addin: build the current Quone project
#' @keywords internal
#' @export
addin_build_project <- function() {
  build_package(".")
}
