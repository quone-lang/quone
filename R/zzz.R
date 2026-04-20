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
        "quone: the Quone compiler isn't installed yet. ",
        "Run `quone::setup()` to install the compiler and configure ",
        "your editor in one step."
      )
    }
  }
}
