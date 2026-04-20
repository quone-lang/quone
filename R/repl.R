#' Start an interactive Quone REPL
#'
#' Spawns `quonec repl` and gives it the calling terminal. When
#' `project` is non-`NULL` (the default when a `quone.toml` is
#' present in the current directory), the REPL auto-loads every
#' `.Q` file under `src/` on startup.
#'
#' Returns when the user exits the REPL with `:quit` (or end-of-input).
#'
#' @section Where this works:
#'
#'   `quone::repl()` takes over standard input and output, so it works
#'   best from a real terminal session (R or Rscript started from your
#'   shell). When you call it from an IDE console (RStudio, the R
#'   extension in Cursor / VS Code, Positron, etc.), the IDE may not
#'   forward keystrokes to the spawned process and the REPL can appear
#'   to hang at the prompt. In that case, either:
#'
#'   * run R from your terminal and call `quone::repl()` again, or
#'   * use `quone::repl_eval()` to evaluate single expressions
#'     non-interactively from inside the IDE.
#'
#' @param project Project root, or `NULL` to start without a
#'   project context.
#' @param rscript_path Optional path to a specific `Rscript` to use as
#'   the REPL backend; defaults to whichever `Rscript` is on PATH.
#' @return Invisibly, the REPL's exit status.
#' @seealso [repl_eval()] for one-shot, non-interactive evaluation.
#' @export
repl <- function(project = detect_project(), rscript_path = NULL) {
  warn_if_not_terminal()

  args <- "repl"
  if (!is.null(project)) {
    args <- c(args, paste0("--project=", project))
  }
  if (!is.null(rscript_path)) {
    args <- c(args, paste0("--rscript=", rscript_path))
  }
  bin <- compiler_path()
  status <- system2(bin, args)
  invisible(status)
}


#' Send a single Quone expression to the REPL and capture the printed
#' value
#'
#' Useful in vignettes, tests, and any non-interactive context (e.g.
#' calling from an IDE console where `repl()` can't reach a TTY).
#' Each call is independent: state from a previous `repl_eval` does
#' not persist.
#'
#' Internally this spawns `quonec repl`, pipes `expr` plus a
#' trailing `:quit`, and strips the welcome banner and `quone> `
#' prompts so the returned string is just the printed value (or the
#' compile / runtime error message).
#'
#' @param expr A character string of Quone source.
#' @param raw If `TRUE`, return the raw transcript including the
#'   banner and prompts. Defaults to `FALSE`.
#' @return The printed output of evaluating `expr`, with a trailing
#'   newline. When `expr` triggers a compile or runtime error the
#'   error message is returned instead. When `raw = TRUE`, returns
#'   the full transcript.
#' @seealso [repl()] for an interactive session.
#' @export
repl_eval <- function(expr, raw = FALSE) {
  bin <- compiler_path()
  out <- system2(
    bin,
    args = "repl",
    stdout = TRUE,
    stderr = TRUE,
    input = c(expr, ":quit")
  )
  text <- paste0(paste(out, collapse = "\n"), "\n")
  if (isTRUE(raw)) {
    return(text)
  }
  clean_repl_transcript(text)
}


#' Strip the REPL welcome banner and `quone> ` prompts from a captured
#' transcript so what remains is just the user-visible output.
#'
#' Robust to a missing trailing newline and to the fact that R's
#' auto-print can land on the same line as the prompt (e.g.
#' `quone> [1] 2`).
#' @keywords internal
clean_repl_transcript <- function(text) {
  if (!nzchar(text)) return(text)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]

  banner <- c(
    "^quonec repl [0-9]",
    "^Type :help for meta-commands"
  )
  banner_re <- paste(banner, collapse = "|")
  lines <- lines[!grepl(banner_re, lines)]

  lines <- sub("^(quone>|\\.\\.\\.\\.\\.) ?", "", lines)

  lines <- lines[nzchar(lines)]

  if (length(lines) == 0L) return("")
  paste0(paste(lines, collapse = "\n"), "\n")
}


#' Heuristic: is the current R process attached to a real terminal?
#'
#' Used to decide whether to print a "this might not work in your IDE"
#' hint before calling `quone::repl()`. False positives are mild
#' (the user sees an unnecessary tip); false negatives are worse
#' (silently hanging when stdin isn't forwarded), so we err on the
#' side of warning.
#' @keywords internal
running_in_terminal <- function() {
  isTRUE(tryCatch(isatty(stdin()), error = function(e) FALSE))
}


#' Print a friendly heads-up if `quone::repl()` is being launched from
#' a context where stdin is unlikely to reach the spawned compiler
#' (most IDE R consoles).
#' @keywords internal
warn_if_not_terminal <- function() {
  if (!interactive()) {
    return(invisible(NULL))
  }
  if (running_in_terminal()) {
    return(invisible(NULL))
  }
  cli::cli_alert_warning(c(
    "It looks like you're calling {.code quone::repl()} from an IDE \\
     console rather than a terminal."
  ))
  cli::cli_bullets(c(
    "i" = "The Quone REPL takes over stdin/stdout. Many IDE consoles \\
           don't forward keystrokes to spawned processes, so the prompt \\
           may sit there with no response.",
    "i" = "If that happens, run R from your terminal and try again, or \\
           use {.code quone::repl_eval(\"<expr>\")} to evaluate a \\
           single expression from here."
  ))
  invisible(NULL)
}


detect_project <- function() {
  if (file.exists("quone.toml")) "." else NULL
}
