#' Infer a typed Quone CSV decoder from a real CSV file
#'
#' Reads the first `n` rows of `path` with `readr::guess_parser()`,
#' then emits a `Csv.dataframe |> Csv.column ...` pipeline matching
#' the surface in LANGUAGE.md section 11.1. Use this to scaffold a
#' decoder before hand-tuning column types.
#'
#' @param path Path to the CSV file.
#' @param n Number of rows to inspect. Default 1000.
#' @return A character string of Quone source.
#' @export
infer_decoder <- function(path, n = 1000) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Install {.pkg readr} to use {.fn infer_decoder}.",
      i = "Run {.code install.packages(\"readr\")}."
    ))
  }
  preview <- readr::read_csv(
    path, n_max = n,
    show_col_types = FALSE
  )
  cols <- vapply(preview, infer_quone_type, character(1))
  lines <- c("Csv.dataframe")
  for (i in seq_along(cols)) {
    lines <- c(lines, sprintf(
      "    |> Csv.column %s %s",
      shQuoteText(names(cols)[[i]]),
      cols[[i]]
    ))
  }
  paste(lines, collapse = "\n")
}


#' Write the inferred decoder to a `.Q` file
#'
#' @param path Path to the CSV file.
#' @param to Path to the output `.Q` file.
#' @param module_path Quone module path (e.g. `"Decoders.Adsl"`).
#' @param binding_name Name of the exported decoder binding.
#' @param n Number of CSV rows to inspect for type inference.
#' @return Invisibly, the path to the new file.
#' @export
write_decoder <- function(
  path,
  to,
  module_path = "Decoders.Inferred",
  binding_name = "decoder",
  n = 1000
) {
  body <- infer_decoder(path, n = n)
  fs::dir_create(dirname(to))
  out <- c(
    sprintf("module %s exporting (%s)", module_path, binding_name),
    "",
    "#' Inferred CSV decoder.",
    "#' @export",
    sprintf("%s <-", binding_name),
    paste0("    ", strsplit(body, "\n", fixed = TRUE)[[1]])
  )
  writeLines(out, to)
  cli::cli_alert_success("Wrote decoder to {.path {to}}")
  invisible(to)
}


infer_quone_type <- function(col) {
  if (inherits(col, "character")) "character"
  else if (inherits(col, "integer")) "integer"
  else if (inherits(col, "numeric")) "double"
  else if (inherits(col, "logical")) "logical"
  else if (inherits(col, "Date") || inherits(col, "POSIXct")) "character"
  else "character"
}


shQuoteText <- function(s) sprintf('"%s"', s)
