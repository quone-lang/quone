#' The closed set of diagnostic categories the compiler can emit.
#'
#' Mirrors LANGUAGE.md section 12.1, plus the implementation-only
#' `internal` sentinel for compiler bugs.
#' @keywords internal
DIAGNOSTIC_CATEGORIES <- c(
  "lexical",
  "parse",
  "unbound-variable",
  "type-mismatch",
  "unknown-constructor",
  "record-field",
  "unknown-dataframe-column",
  "file-loading",
  "decode",
  "non-exhaustive-pattern",
  "internal"
)


#' Get diagnostics for a `.Q` file or project
#'
#' Runs the compiler in JSON mode and returns the parsed result as a
#' tibble with one row per diagnostic.
#'
#' @param path Path to a `.Q` file, or a project directory containing
#'   `quone.toml`.
#' @return A tibble with columns `severity`, `category`, `file`,
#'   `line`, `col`, `end_line`, `end_col`, `message`, `hint`.
#' @export
diagnostics <- function(path) {
  is_dir <- dir.exists(path)
  args <- if (is_dir) list("build", "--package", path) else list("check", path)
  res <- do.call(invoke_compiler, args)
  parse_ndjson(res$stderr)
}


#' Render diagnostics into the IDE Problems pane
#'
#' Calls `rstudioapi::sourceMarkers()` when running inside RStudio /
#' Positron; otherwise prints a `cli` block per diagnostic.
#' @param diags A diagnostics tibble (see [diagnostics()]).
#' @return Invisibly, `diags`.
#' @export
show_diagnostics <- function(diags) {
  if (nrow(diags) == 0) {
    cli::cli_alert_success("No diagnostics.")
    return(invisible(diags))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    markers <- lapply(seq_len(nrow(diags)), function(i) {
      list(
        type = if (diags$severity[[i]] == "error") "error"
               else if (diags$severity[[i]] == "warning") "warning"
               else "info",
        file = diags$file[[i]],
        line = diags$line[[i]],
        column = diags$col[[i]],
        message = diags$message[[i]]
      )
    })
    rstudioapi::sourceMarkers(
      name = "quone",
      markers = markers,
      basePath = NULL,
      autoSelect = "first"
    )
  } else {
    for (i in seq_len(nrow(diags))) {
      cli::cli_alert_danger(format_diagnostic(diags[i, ]))
    }
  }
  invisible(diags)
}


#' Format a single diagnostic as a one-line `cli`-styled string
#'
#' @param d A single-row diagnostics tibble or a list with the same
#'   fields.
#' @return A character string.
#' @export
format_diagnostic <- function(d) {
  if (is.data.frame(d)) d <- as.list(d[1, ])
  hint <- if (!is.null(d$hint) && !is.na(d$hint) && nzchar(d$hint))
    paste0("\n  hint: ", d$hint)
  else ""
  sprintf(
    "%s[%s]: %s\n  --> %s:%d:%d%s",
    d$severity, d$category, d$message,
    d$file, d$line, d$col, hint
  )
}


#' Raise an error from a diagnostics tibble
#'
#' Used by [build()], [check()], and the rest of the compiler
#' wrappers: when the compiler exits with a non-zero status, this
#' function turns the parsed JSON diagnostic stream into a classed
#' [rlang::abort()] condition.
#'
#' The condition class hierarchy is
#' `quone_diagnostic / quone_<category> / error / condition`, so
#' callers can `tryCatch(quone_type_mismatch = ...)`.
#' @param diags A diagnostics tibble (see [diagnostics()]).
#' @keywords internal
#' @export
abort_on_diagnostics <- function(diags) {
  if (nrow(diags) == 0) {
    rlang::abort("compiler exited non-zero with no diagnostics")
  }
  primary <- diags[1, ]
  msg <- format_diagnostic(primary)
  klass <- c(
    paste0("quone_", gsub("-", "_", primary$category)),
    "quone_diagnostic"
  )
  rlang::abort(
    msg,
    class = klass,
    diagnostics = diags
  )
}


parse_ndjson <- function(stream) {
  if (is.null(stream) || !nzchar(stream)) {
    return(empty_diagnostics())
  }
  lines <- strsplit(stream, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) return(empty_diagnostics())
  rows <- lapply(lines, function(ln) {
    obj <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = TRUE),
                    error = function(e) NULL)
    if (is.null(obj)) return(NULL)
    tibble::tibble(
      severity = obj$severity %||% NA_character_,
      category = factor(
        obj$category %||% NA_character_,
        levels = DIAGNOSTIC_CATEGORIES
      ),
      file = obj$file %||% NA_character_,
      line = obj$start$line %||% NA_integer_,
      col = obj$start$col %||% NA_integer_,
      end_line = obj$end$line %||% NA_integer_,
      end_col = obj$end$col %||% NA_integer_,
      message = obj$message %||% NA_character_,
      hint = obj$hint %||% NA_character_
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty_diagnostics())
  do.call(rbind, rows)
}


empty_diagnostics <- function() {
  tibble::tibble(
    severity = character(),
    category = factor(character(), levels = DIAGNOSTIC_CATEGORIES),
    file = character(),
    line = integer(),
    col = integer(),
    end_line = integer(),
    end_col = integer(),
    message = character(),
    hint = character()
  )
}


`%||%` <- function(a, b) if (is.null(a)) b else a
