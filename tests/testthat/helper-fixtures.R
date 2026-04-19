skip_if_no_compiler <- function() {
  env_path <- Sys.getenv("QUONEC", unset = "")
  on_path <- Sys.which("quonec")
  has_compiler <-
    (nzchar(env_path) && file.exists(env_path)) ||
    (nzchar(on_path) && file.exists(on_path)) ||
    !is.null(tryCatch(compiler_path(error = FALSE), error = function(e) NULL))
  testthat::skip_if_not(has_compiler, "quonec not installed")
}


fixture <- function(name) {
  test_path("fixtures", name)
}
