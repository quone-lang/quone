#' quone: R companion for the Quone language
#'
#' The `quone` package is a developer-experience layer for working
#' with the Quone language from R. It discovers and invokes the
#' `quonec` compiler, scaffolds projects, runs a watch-and-rebuild
#' loop, surfaces compiler diagnostics in the IDE, rewrites runtime
#' tracebacks back to `.Q` source, registers a knitr / Quarto engine,
#' wraps the real `quonec repl` / `quonec fmt` / `quonec lsp`, and
#' includes audit and snapshot tooling.
#'
#' The package is for *authors* of Quone. Generated R remains a
#' standalone artifact whose only runtime dependencies are base R
#' plus the libraries the compiler infers from foreign imports
#' (typically `dplyr`, `purrr`, `readr`).
#'
#' @keywords internal
"_PACKAGE"
