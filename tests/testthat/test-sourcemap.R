test_that("source_position returns NULL when no map exists", {
  tmp <- tempfile(fileext = ".R")
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_null(source_position(tmp, 1, 1))
})


test_that("read_sourcemap parses NDJSON sidecars", {
  tmp <- tempfile(fileext = ".R")
  map <- paste0(tmp, ".map")
  on.exit(unlink(c(tmp, map)))
  writeLines("# placeholder", tmp)
  body <- c(
    '{"source":"in.Q","generated":"out.R"}',
    '{"r":{"line":1,"col":1},"q":{"line":4,"col":1}}',
    '{"r":{"line":2,"col":1},"q":{"line":7,"col":1}}'
  )
  writeLines(body, map)
  pos <- source_position(tmp, 2)
  expect_equal(pos$file, "in.Q")
  expect_equal(pos$line, 7L)
})


test_that("source_position picks the largest r_line not exceeding the request", {
  tmp <- tempfile(fileext = ".R")
  map <- paste0(tmp, ".map")
  on.exit(unlink(c(tmp, map)))
  writeLines("# placeholder", tmp)
  body <- c(
    '{"source":"in.Q","generated":"out.R"}',
    '{"r":{"line":1,"col":1},"q":{"line":3,"col":1}}',
    '{"r":{"line":3,"col":1},"q":{"line":9,"col":1}}',
    '{"r":{"line":7,"col":1},"q":{"line":12,"col":1}}'
  )
  writeLines(body, map)
  expect_equal(source_position(tmp, 5)$line, 9L)
  expect_equal(source_position(tmp, 7)$line, 12L)
  expect_equal(source_position(tmp, 100)$line, 12L)
})
