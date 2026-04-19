#' Start an interactive Quone REPL
#'
#' Spawns `quonec repl` and gives it the calling terminal. When
#' `project` is non-`NULL` (the default when a `quone.toml` is
#' present in the current directory), the REPL auto-loads every
#' `.Q` file under `src/` on startup.
#'
#' Returns when the user exits the REPL with `:quit` (or end-of-input).
#'
#' @param project Project root, or `NULL` to start without a
#'   project context.
#' @param rscript_path Optional path to a specific `Rscript` to use as
#'   the REPL backend; defaults to whichever `Rscript` is on PATH.
#' @return Invisibly, the REPL's exit status.
#' @export
repl <- function(project = detect_project(), rscript_path = NULL) {
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
#' Useful in vignettes, tests, or any non-interactive context that
#' needs a one-shot evaluation. Each call is independent: state from
#' a previous `repl_eval` does not persist.
#'
#' @param expr A character string of Quone source.
#' @return The captured stdout from the REPL evaluation.
#' @export
repl_eval <- function(expr) {
  bin <- compiler_path()
  res <- processx::run(
    bin,
    c("repl"),
    input = paste0(expr, "\n:quit\n"),
    error_on_status = FALSE
  )
  res$stdout
}


detect_project <- function() {
  if (file.exists("quone.toml")) "." else NULL
}
