test_that("snapshot_mtimes returns a named vector keyed by file path", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  writeLines("x", file.path(tmp, "a.Q"))
  writeLines("y", file.path(tmp, "b.Q"))
  ms <- quone:::snapshot_mtimes(tmp)
  expect_length(ms, 2)
  expect_true(all(grepl("\\.Q$", names(ms))))
})


test_that("snapshot_mtimes is empty for a directory with no .Q files", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  expect_length(quone:::snapshot_mtimes(tmp), 0)
})


test_that("watch_stop is a no-op when no watcher is active", {
  res <- watch_stop()
  expect_false(res)
})


test_that("watch is non-blocking when block = FALSE and stoppable", {
  skip_if_no_compiler()
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  create_project(tmp, name = "demo")

  # Spawn a watcher; should return immediately because block = FALSE.
  handle <- watch(tmp, interval_ms = 50, block = FALSE)
  expect_true(is.environment(handle))
  expect_true(handle$active)

  # The poll task is registered with later; running the loop a few
  # times must not throw.
  later::run_now(timeoutSecs = 0.05)
  later::run_now(timeoutSecs = 0.05)

  expect_true(watch_stop(handle))
  expect_false(handle$active)
})
