.onLoad <- function(libname, pkgname) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(quone = knit_engine_quone)
  }
  invisible(NULL)
}


.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    path <- tryCatch(compiler_path(error = FALSE), error = function(e) NULL)
    if (is.null(path)) {
      packageStartupMessage(
        "quone: the `quonec` compiler was not found on PATH. ",
        "Run `quone::install_compiler()` to install it."
      )
    }
  }
}
