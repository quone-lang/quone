#' Watch a Quone project and rebuild on every change
#'
#' Polls `src/` (every `interval_ms` milliseconds; default 500 ms) on
#' R's [later::later()] event loop. The polling task re-schedules
#' itself, so the user's REPL stays responsive: type, run other
#' commands, or call [watch_stop()] to terminate the watcher.
#'
#' Use [watch_stop()] (or `Ctrl-C` in the rare case the loop is
#' actively executing a build) to stop.
#'
#' @param project Project root.
#' @param interval_ms Polling interval in milliseconds.
#' @param on_success Optional callback invoked after a successful
#'   build, receiving the build directory.
#' @param on_error Optional callback invoked on a build failure,
#'   receiving the diagnostics tibble. Defaults to
#'   [show_diagnostics()].
#' @param block If `TRUE`, blocks the caller and pumps the event
#'   loop in-line (equivalent to `while (TRUE)
#'   later::run_now(0.1)`); if `FALSE` (the default in interactive
#'   sessions), schedules the poll on R's main event loop and
#'   returns immediately.
#' @return Invisibly, an opaque watcher handle (currently a
#'   one-element environment) that you can pass to [watch_stop()].
#' @export
watch <- function(
  project = ".",
  interval_ms = 500,
  on_success = NULL,
  on_error = NULL,
  block = !interactive()
) {
  if (is.null(on_error)) on_error <- show_diagnostics
  src_dir <- file.path(project, "src")
  if (!dir.exists(src_dir)) {
    cli::cli_abort("No {.path src/} directory in {.path {project}}")
  }
  cli::cli_alert_info(
    paste0(
      "Watching ", src_dir,
      " (every ", interval_ms, " ms; ",
      "call quone::watch_stop() to stop)"
    )
  )

  handle <- new.env(parent = emptyenv())
  handle$active <- TRUE
  handle$state <- snapshot_mtimes(src_dir)
  handle$src_dir <- src_dir
  handle$project <- project
  handle$on_success <- on_success
  handle$on_error <- on_error
  handle$interval_s <- interval_ms / 1000

  # Initial build so the user sees feedback immediately.
  do_build(project, on_success, on_error)

  schedule_poll(handle)

  # Let any active watchers register on a session-global list so
  # watch_stop() can find them without an explicit handle.
  active_watchers(add = handle)

  if (isTRUE(block)) {
    repeat {
      if (!isTRUE(handle$active)) break
      later::run_now(timeoutSecs = handle$interval_s)
    }
  }

  invisible(handle)
}


#' Stop a watcher
#'
#' Stops the most recently started watcher, or the specific `handle`
#' if one is provided. Subsequent calls are no-ops.
#'
#' @param handle An optional handle returned from [watch()].
#' @return Invisibly, `TRUE` if a watcher was stopped.
#' @export
watch_stop <- function(handle = NULL) {
  if (is.null(handle)) {
    handles <- active_watchers()
    if (length(handles) == 0L) {
      cli::cli_alert_info("No active watchers.")
      return(invisible(FALSE))
    }
    handle <- handles[[length(handles)]]
  }
  handle$active <- FALSE
  active_watchers(remove = handle)
  cli::cli_alert_info("Watcher stopped.")
  invisible(TRUE)
}


schedule_poll <- function(handle) {
  later::later(
    function() {
      if (!isTRUE(handle$active)) return(invisible())
      new <- snapshot_mtimes(handle$src_dir)
      if (!identical(new, handle$state)) {
        cli::cli_alert_info("Change detected, rebuilding ...")
        do_build(handle$project, handle$on_success, handle$on_error)
        handle$state <- new
      }
      schedule_poll(handle)
    },
    delay = handle$interval_s
  )
}


active_watchers <- local({
  handles <- list()
  function(add = NULL, remove = NULL) {
    if (!is.null(add)) handles[[length(handles) + 1L]] <<- add
    if (!is.null(remove)) {
      handles <<- Filter(function(h) !identical(h, remove), handles)
    }
    handles
  }
})


snapshot_mtimes <- function(dir) {
  files <- fs::dir_ls(dir, recurse = TRUE, glob = "*.Q")
  if (length(files) == 0) return(character())
  stats::setNames(format(file.info(files)$mtime), files)
}


do_build <- function(project, on_success, on_error) {
  tryCatch({
    out <- build_package(project, document = TRUE)
    cli::cli_alert_success("Built {.path {out}}")
    if (!is.null(on_success)) on_success(out)
  }, quone_diagnostic = function(cnd) {
    diags <- attr(cnd, "diagnostics")
    if (is.null(diags)) diags <- empty_diagnostics()
    on_error(diags)
  }, error = function(cnd) {
    cli::cli_alert_danger(conditionMessage(cnd))
  })
}


#' Build, then `pkgload::load_all` the generated R package
#'
#' Like `devtools::load_all()` but for a Quone project: every change
#' triggers a rebuild plus a fresh `load_all` so the freshly-defined
#' R functions are reachable in the calling session immediately.
#'
#' Implemented on top of [watch()] so the polling runs on R's event
#' loop; the calling session's REPL stays interactive.
#'
#' @param project Project root.
#' @param interval_ms Polling interval in milliseconds.
#' @param block If `TRUE`, blocks the caller and pumps the event
#'   loop in-line; if `FALSE` (the default in interactive sessions),
#'   schedules the watcher on R's main event loop and returns
#'   immediately so the user can keep typing.
#' @return Invisibly, the watcher handle.
#' @export
dev_session <- function(
  project = ".",
  interval_ms = 500,
  block = !interactive()
) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    cli::cli_abort(c(
      "Install {.pkg pkgload} to use {.fn dev_session}.",
      i = "Run {.code install.packages(\"pkgload\")}."
    ))
  }
  watch(
    project = project,
    interval_ms = interval_ms,
    block = block,
    on_success = function(out) {
      with_sourcemap(pkgload::load_all(out, quiet = TRUE))
      cli::cli_alert_success("Reloaded package from {.path {out}}")
    }
  )
}
