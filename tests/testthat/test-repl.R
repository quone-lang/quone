test_that("detect_project picks up a quone.toml in the working dir", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  create_project(tmp, name = "demo")
  withr::with_dir(tmp, {
    expect_equal(quone:::detect_project(), ".")
  })
})


test_that("detect_project returns NULL elsewhere", {
  withr::with_tempfile("td", {
    dir.create(td)
    withr::with_dir(td, {
      expect_null(quone:::detect_project())
    })
  })
})


test_that("clean_repl_transcript drops banner and prompts", {
  raw <- paste(
    "quonec repl 0.0.1",
    "Type :help for meta-commands, :quit to exit.",
    "quone> [1] 2",
    "quone> ",
    "",
    sep = "\n"
  )
  expect_equal(quone:::clean_repl_transcript(raw), "[1] 2\n")
})


test_that("clean_repl_transcript handles multi-line output", {
  raw <- paste(
    "quonec repl 0.0.1",
    "Type :help for meta-commands, :quit to exit.",
    "quone> [1] 1 2 3",
    "quone> [1] Inf",
    "quone> ",
    sep = "\n"
  )
  expect_equal(
    quone:::clean_repl_transcript(raw),
    "[1] 1 2 3\n[1] Inf\n"
  )
})


test_that("clean_repl_transcript returns '' for an empty transcript", {
  expect_equal(quone:::clean_repl_transcript(""), "")
})


test_that("running_in_terminal returns a single logical without erroring", {
  res <- quone:::running_in_terminal()
  expect_type(res, "logical")
  expect_length(res, 1L)
  expect_false(is.na(res))
})


test_that("warn_if_not_terminal is a quiet no-op when non-interactive", {
  expect_silent(quone:::warn_if_not_terminal())
})


test_that("repl_eval prints scalar results", {
  skip_if_no_compiler()
  out <- repl_eval("1.0 + 1.0")
  expect_match(out, "[1] 2", fixed = TRUE)
})


test_that("repl_eval prints vector literals", {
  skip_if_no_compiler()
  out <- repl_eval("[1.0, 2.0, 3.0]")
  expect_match(out, "[1] 1 2 3", fixed = TRUE)
})


test_that("repl_eval surfaces a Quone compile error without hanging", {
  skip_if_no_compiler()
  out <- repl_eval("1.0 + \"oops\"")
  expect_match(out, "type-mismatch")
})


test_that("repl_eval(raw = TRUE) keeps the banner", {
  skip_if_no_compiler()
  out <- repl_eval("1.0 + 1.0", raw = TRUE)
  expect_match(out, "quonec repl")
  expect_match(out, "quone>")
  expect_match(out, "[1] 2", fixed = TRUE)
})
