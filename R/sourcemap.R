#' Read a `.R.map` sidecar emitted by the compiler
#'
#' Returns a list with `source`, `generated`, and `entries`, where
#' `entries` is a tibble of `r_line`, `r_col`, `q_line`, `q_col`.
#' @param path Path to the `.R.map` sidecar.
#' @return A `quone_sourcemap` object.
#' @keywords internal
read_sourcemap <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("No source map at {.path {path}}")
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    return(structure(
      list(source = NA_character_, generated = NA_character_,
           entries = empty_entries()),
      class = "quone_sourcemap"
    ))
  }
  hdr <- jsonlite::fromJSON(lines[[1]], simplifyVector = TRUE)
  rest <- lines[-1]
  entries <- if (length(rest) == 0) empty_entries() else {
    rows <- lapply(rest, function(ln) {
      obj <- jsonlite::fromJSON(ln, simplifyVector = TRUE)
      tibble::tibble(
        r_line = obj$r$line,
        r_col = obj$r$col,
        q_line = obj$q$line,
        q_col = obj$q$col
      )
    })
    do.call(rbind, rows)
  }
  structure(
    list(
      source = hdr$source,
      generated = hdr$generated,
      entries = entries
    ),
    class = "quone_sourcemap"
  )
}


empty_entries <- function() {
  tibble::tibble(
    r_line = integer(),
    r_col = integer(),
    q_line = integer(),
    q_col = integer()
  )
}


#' Look up a generated-R position in a source map
#'
#' Finds the `.Q` position whose `r_line` is the largest one less than
#' or equal to the requested line. Returns `NULL` when no map exists
#' next to `generated_path`.
#' @param generated_path Path to the generated `.R` file.
#' @param line Generated R line (1-based).
#' @param col Generated R column (1-based).
#' @return A list with `file`, `line`, `col`, or `NULL`.
#' @export
source_position <- function(generated_path, line, col = 1L) {
  map_path <- paste0(generated_path, ".map")
  if (!file.exists(map_path)) return(NULL)
  sm <- read_sourcemap(map_path)
  if (nrow(sm$entries) == 0) return(NULL)
  candidates <- sm$entries[sm$entries$r_line <= line, ]
  if (nrow(candidates) == 0) return(NULL)
  hit <- candidates[which.max(candidates$r_line), ]
  list(
    file = sm$source,
    line = as.integer(hit$q_line),
    col = as.integer(hit$q_col)
  )
}


#' Rewrite a captured R traceback to point at `.Q` source
#'
#' Walks each frame of `tb` and, for every frame whose source ref
#' points into a generated R file with a known `.R.map`, replaces it
#' with the corresponding `.Q` position.
#' @param tb A traceback as returned by `rlang::trace_back()` or
#'   `base::sys.calls()`.
#' @return A character vector of rewritten frames.
#' @export
trace_to_source <- function(tb) {
  if (inherits(tb, "rlang_trace")) {
    if (is.null(tb$call)) return(character())
    refs <- attr(tb$call, "srcref")
    refs <- if (is.null(refs)) vector("list", length(tb$call)) else refs
  } else if (is.list(tb)) {
    refs <- lapply(tb, attr, which = "srcref")
  } else {
    return(character())
  }
  vapply(seq_along(refs), function(i) {
    sr <- refs[[i]]
    if (is.null(sr)) return(deparse_call(if (is.list(tb)) tb[[i]] else tb$call[[i]]))
    file <- attr(sr, "srcfile")$filename %||% ""
    line <- as.integer(sr)[[1]]
    col <- as.integer(sr)[[2]]
    pos <- source_position(file, line, col)
    if (is.null(pos)) {
      sprintf("%s:%d:%d", file, line, col)
    } else {
      sprintf("%s:%d:%d (%s:%d)", pos$file, pos$line, pos$col, file, line)
    }
  }, character(1))
}


deparse_call <- function(x) paste(deparse(x), collapse = " ")


#' Run R code with traceback rewriting installed
#'
#' Installs a calling handler that captures any condition's traceback
#' and rewrites it to point at `.Q` source when a `.R.map` sidecar is
#' present. Used by [dev_session()] automatically.
#'
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#' @export
with_sourcemap <- function(expr) {
  withCallingHandlers(
    expr,
    error = function(cnd) {
      tb <- if (requireNamespace("rlang", quietly = TRUE))
        rlang::trace_back() else sys.calls()
      rewritten <- trace_to_source(tb)
      if (length(rewritten) > 0) {
        cli::cli_alert_warning(
          "Quone source positions for the most recent error:"
        )
        for (line in rewritten) cli::cli_li(line)
      }
    }
  )
}
