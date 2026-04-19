#' knitr / Quarto engine for ` ```{quone} ` chunks
#'
#' Compiles each Quone chunk via [build()], optionally `source()`s the
#' generated R, and renders one of three views:
#'
#' * `quone.show = "quone"` -- show only the Quone source.
#' * `quone.show = "r"` -- show only the generated R.
#' * `quone.show = "both"` (the default) -- side-by-side.
#'
#' Registered automatically in `.onLoad` whenever `knitr` is loaded.
#' Callers who want to register manually can do
#' `knitr::knit_engines$set(quone = quone:::knit_engine_quone)`.
#'
#' @param options A `knitr` chunk options list.
#' @return The chunk output, suitable for splicing into the rendered
#'   document.
#' @keywords internal
#' @export
knit_engine_quone <- function(options) {
  src <- paste(options$code, collapse = "\n")
  quone.show <- options$`quone.show` %||% "both"
  quone.eval <- isTRUE(options$eval %||% TRUE)

  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  q_path <- file.path(tmpdir, paste0(options$label %||% "chunk", ".Q"))
  writeLines(src, q_path)
  r_path <- tryCatch(
    build(q_path, out = tmpdir, sourcemap = FALSE),
    quone_diagnostic = function(cnd) {
      msg <- conditionMessage(cnd)
      knitr::engine_output(
        options,
        code = src,
        out = paste("Error in Quone chunk:", msg)
      )
    }
  )
  if (is.null(r_path) || !file.exists(r_path)) {
    return(knitr::engine_output(options, code = src,
                                out = "compilation failed"))
  }
  r_src <- paste(readLines(r_path, warn = FALSE), collapse = "\n")

  result <- if (quone.eval) {
    captured <- utils::capture.output({
      e <- new.env(parent = globalenv())
      source(r_path, local = e, echo = FALSE)
    })
    paste(captured, collapse = "\n")
  } else ""

  shown <- switch(
    quone.show,
    quone = src,
    r = r_src,
    both = paste(
      "# Quone\n\n", src, "\n\n# Generated R\n\n", r_src,
      sep = ""
    ),
    src
  )
  knitr::engine_output(options, code = shown, out = result)
}
