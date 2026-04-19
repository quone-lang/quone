#' Build, document, and finalise an R package
#'
#' Equivalent to: `quone::build_package(project)` followed by
#' `roxygen2::roxygenise(file.path(project, "build"))`. This is the
#' single command that the `examples/README.md` workflow currently
#' asks the user to run as two separate steps.
#'
#' @param project Project root.
#' @return Invisibly, the path to the generated R package directory.
#' @export
document <- function(project = ".") {
  build_package(project, document = TRUE)
}


#' Install a Quone project as an R package
#'
#' Builds, documents, then installs the resulting package via
#' `pak::pkg_install` (or `devtools::install` if `pak` is missing).
#'
#' @param project Project root.
#' @param upgrade Passed through to `pak::pkg_install`.
#' @return Invisibly, the project root path.
#' @export
install <- function(project = ".", upgrade = "default") {
  build_dir <- document(project)
  if (requireNamespace("pak", quietly = TRUE)) {
    pak::pkg_install(paste0("local::", build_dir), upgrade = upgrade)
  } else if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::install(build_dir)
  } else {
    cli::cli_abort(c(
      "Either {.pkg pak} or {.pkg devtools} must be installed.",
      i = "Run {.code install.packages(\"pak\")}."
    ))
  }
  invisible(project)
}


#' Run the project's R-side test suite
#'
#' Builds the project, optionally installs it, then runs every test
#' file under `tests/`. Tests use plain `testthat`; Quone fixtures
#' can be `quone::source_quone`'d into the test environment to make
#' assertions against the generated R.
#'
#' @param project Project root.
#' @param dir Directory holding the tests, relative to `project`.
#' @return Invisibly, the testthat results.
#' @export
test_project <- function(project = ".", dir = "tests") {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    cli::cli_abort("Install {.pkg testthat} to run Quone tests.")
  }
  build_dir <- document(project)
  test_dir <- file.path(project, dir)
  if (!dir.exists(test_dir)) {
    cli::cli_alert_warning("No tests directory at {.path {test_dir}}")
    return(invisible(NULL))
  }
  invisible(testthat::test_dir(test_dir))
}
