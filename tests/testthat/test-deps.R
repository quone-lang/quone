test_that("deps returns an empty tibble for projects with no deps", {
  skip_if_no_compiler()
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  create_project(tmp, name = "demo")
  d <- deps(tmp)
  expect_s3_class(d, "tbl_df")
})


test_that("deps_graph returns a Mermaid string", {
  skip_if_no_compiler()
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  create_project(tmp, name = "demo")
  out <- deps_graph(tmp)
  expect_match(out, "^graph LR")
})
