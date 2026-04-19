#' Audit the auto-derived runtime dependency set
#'
#' Wraps `quonec deps --diagnostics-format=json`; returns a tibble
#' with one row per R package the compiled output will pull in, per
#' LANGUAGE.md section 13.9. The `introduced_by` column is currently
#' a constant `"compiler"` -- attribution to specific Quone constructs
#' lands once the compiler exposes a richer `deps` schema.
#'
#' @param project Project root.
#' @return A tibble with `package`, `introduced_by`.
#' @export
deps <- function(project = ".") {
  res <- invoke_compiler("deps", project)
  if (res$status != 0) {
    abort_on_diagnostics(parse_ndjson(res$stderr))
  }
  lines <- strsplit(res$stdout, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    return(tibble::tibble(
      package = character(),
      introduced_by = character()
    ))
  }
  pkgs <- vapply(lines, function(ln) {
    obj <- jsonlite::fromJSON(ln, simplifyVector = TRUE)
    obj$package
  }, character(1))
  tibble::tibble(
    package = unname(pkgs),
    introduced_by = "compiler"
  )
}


#' License audit for the auto-derived runtime dependency set
#'
#' Joins [deps()] against `available.packages()` to surface licenses
#' and CRAN URLs. Useful for supply-chain review.
#'
#' @param project Project root.
#' @return A tibble with `package`, `license`, `url`.
#' @export
licenses <- function(project = ".") {
  d <- deps(project)
  if (nrow(d) == 0) {
    return(tibble::tibble(package = character(), license = character(), url = character()))
  }
  ap <- as.data.frame(utils::available.packages())
  hits <- ap[ap$Package %in% d$package, c("Package", "License", "URL")]
  tibble::tibble(
    package = hits$Package,
    license = hits$License,
    url = hits$URL
  )
}


#' Render the dependency set as a Mermaid graph
#'
#' Returns a string suitable for a Quarto / R Markdown document,
#' e.g. inside ` ```{mermaid} ` fences.
#'
#' @param project Project root.
#' @return A character string.
#' @export
deps_graph <- function(project = ".") {
  d <- deps(project)
  if (nrow(d) == 0) return("graph LR\n  empty[no dependencies]\n")
  edges <- vapply(d$package, function(p) {
    sprintf("  project --> %s", gsub("[^A-Za-z0-9]", "_", p))
  }, character(1))
  paste(c("graph LR", edges), collapse = "\n")
}
