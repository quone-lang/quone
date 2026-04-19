test_that("parse_ndjson handles an empty stream", {
  d <- quone:::parse_ndjson("")
  expect_equal(nrow(d), 0)
  expect_setequal(
    colnames(d),
    c("severity", "category", "file", "line", "col",
      "end_line", "end_col", "message", "hint")
  )
})


test_that("parse_ndjson decodes a single diagnostic", {
  raw <- jsonlite::toJSON(
    list(
      severity = "error",
      category = "type-mismatch",
      file = "foo.Q",
      start = list(line = 1, col = 1),
      end = list(line = 1, col = 5),
      message = "boom"
    ),
    auto_unbox = TRUE
  )
  d <- quone:::parse_ndjson(as.character(raw))
  expect_equal(nrow(d), 1)
  expect_equal(d$severity, "error")
  expect_equal(as.character(d$category), "type-mismatch")
  expect_equal(d$line, 1L)
})


test_that("category factor uses the closed set", {
  expect_setequal(
    quone:::DIAGNOSTIC_CATEGORIES,
    c("lexical", "parse", "unbound-variable", "type-mismatch",
      "unknown-constructor", "record-field",
      "unknown-dataframe-column", "file-loading", "decode",
      "non-exhaustive-pattern", "internal")
  )
})


test_that("format_diagnostic produces a human-readable string", {
  d <- list(
    severity = "error",
    category = "parse",
    file = "foo.Q",
    line = 2L,
    col = 3L,
    message = "expected ;",
    hint = "remove the trailing comma"
  )
  out <- format_diagnostic(d)
  expect_match(out, "error\\[parse\\]")
  expect_match(out, "foo\\.Q:2:3")
  expect_match(out, "remove the trailing comma")
})


test_that("abort_on_diagnostics raises a quone_<category> condition", {
  d <- tibble::tibble(
    severity = "error",
    category = factor(
      "type-mismatch",
      levels = quone:::DIAGNOSTIC_CATEGORIES
    ),
    file = "foo.Q",
    line = 1L, col = 1L,
    end_line = 1L, end_col = 2L,
    message = "boom",
    hint = NA_character_
  )
  expect_error(
    abort_on_diagnostics(d),
    class = "quone_type_mismatch"
  )
})
