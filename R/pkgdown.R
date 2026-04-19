#' Build a `pkgdown` site for the *generated* R package
#'
#' Runs [document()] (so the package's `man/` is up to date) and then
#' invokes `pkgdown::build_site_external()` on the build directory.
#' Useful for publishing a Quone project's R-side documentation
#' without writing any roxygen comments by hand -- the comments come
#' from the `#'` blocks on the Quone source.
#'
#' @param project Project root.
#' @param preview If `TRUE` (default), open the rendered site after
#'   building.
#' @return Invisibly, the path to the built site.
#' @export
pkgdown_site <- function(project = ".", preview = TRUE) {
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    cli::cli_abort(c(
      "Install {.pkg pkgdown} to use {.fn pkgdown_site}.",
      i = "Run {.code install.packages(\"pkgdown\")}."
    ))
  }
  build_dir <- document(project)
  pkgdown::build_site_github_pages(
    pkg = build_dir,
    new_process = FALSE,
    install = FALSE
  )
  site <- file.path(build_dir, "docs")
  if (isTRUE(preview)) {
    if (interactive() && requireNamespace("utils", quietly = TRUE)) {
      utils::browseURL(file.path(site, "index.html"))
    }
  }
  invisible(site)
}
