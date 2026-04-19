#' Assert that a Quone file lowers to the same R as a snapshot
#'
#' Compiles `quone_path`, compares the resulting R against the
#' snapshot file in `snapshot_dir`. Updates the snapshot when run
#' under `testthat::snapshot_accept()`. Use this in `testthat`
#' suites for performance-sensitive lowerings (e.g. the base-R vs
#' `purrr` exceptions in LANGUAGE.md 13.3.2) so an upstream compiler
#' change does not silently regress the output.
#'
#' @param quone_path Path to the `.Q` file under test.
#' @param snapshot_dir Directory in which to store / look up
#'   snapshots.
#' @return Invisibly, the path to the snapshot file.
#' @export
snapshot_test <- function(
  quone_path,
  snapshot_dir = "tests/snapshots"
) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    cli::cli_abort("Install {.pkg testthat} to use snapshot_test()")
  }
  dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  r_path <- build(quone_path, out = out_dir, sourcemap = FALSE)
  snapshot_name <- paste0(
    fs::path_ext_remove(basename(quone_path)),
    ".R"
  )
  target <- file.path(snapshot_dir, snapshot_name)
  invisible(
    testthat::expect_snapshot_file(
      r_path,
      name = snapshot_name,
      transform = NULL
    )
  )
}


#' Open every pending snapshot diff in the IDE
#'
#' A thin wrapper around `testthat::snapshot_review()` that filters
#' to Quone snapshots only.
#'
#' @return Invisibly, `NULL`.
#' @export
snapshot_review <- function() {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    cli::cli_abort("Install {.pkg testthat} to use snapshot_review()")
  }
  testthat::snapshot_review("snapshots/")
  invisible(NULL)
}
