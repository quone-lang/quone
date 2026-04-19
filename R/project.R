#' Scaffold a brand-new Quone project
#'
#' Writes a `quone.toml` and a starter `src/Main.Q` under `path`.
#' The project layout follows LANGUAGE.md section 14.6.
#'
#' @param path Directory to create.
#' @param name Package name to write into `quone.toml` (defaults to
#'   the directory name).
#' @param author Free-text author string.
#' @return Invisibly, the absolute path to the new project.
#' @export
create_project <- function(
  path,
  name = fs::path_file(path),
  author = "An R user"
) {
  fs::dir_create(path)
  fs::dir_create(file.path(path, "src"))

  toml <- render_template(
    "quone.toml.tmpl",
    list(name = name, author = author)
  )
  writeLines(toml, file.path(path, "quone.toml"))

  main <- render_template("main.Q.tmpl", list())
  writeLines(main, file.path(path, "src", "Main.Q"))

  cli::cli_alert_success("Created Quone project at {.path {path}}")
  invisible(normalizePath(path))
}


#' Add a new module to a Quone project
#'
#' Translates a dotted Quone module path (`Stats.Summary`) to the
#' on-disk file (`src/Stats/Summary.Q`) per LANGUAGE.md section 14.6
#' and writes a stub binding.
#'
#' @param module_path Dotted module path, e.g. `"Stats.Summary"`.
#' @param project Project root.
#' @param stub_name Name of the stub binding to scaffold.
#' @return Invisibly, the path to the new file.
#' @export
create_module <- function(
  module_path,
  project = ".",
  stub_name = "todo"
) {
  segments <- strsplit(module_path, ".", fixed = TRUE)[[1]]
  if (any(!grepl("^[A-Z][A-Za-z0-9_]*$", segments))) {
    cli::cli_abort(c(
      "Invalid module path {.val {module_path}}.",
      i = "Each segment must start with an uppercase letter."
    ))
  }
  rel <- do.call(
    file.path,
    c(list("src"),
      as.list(segments[-length(segments)]),
      list(paste0(segments[length(segments)], ".Q")))
  )
  abs <- file.path(project, rel)
  fs::dir_create(dirname(abs))
  body <- render_template(
    "module.Q.tmpl",
    list(module_path = module_path, stub_name = stub_name)
  )
  writeLines(body, abs)
  cli::cli_alert_success("Created module {.path {abs}}")
  invisible(abs)
}


#' Add an R-package dependency to `quone.toml`
#'
#' Edits the `[dependencies]` section in place. When the section does
#' not yet exist it is appended.
#'
#' @param package R package name (e.g. `"dplyr"`).
#' @param version Optional version constraint string
#'   (e.g. `">= 1.1"`); when `NULL` no constraint is written.
#' @param project Project root.
#' @return Invisibly, the updated `quone.toml` lines.
#' @export
add_dependency <- function(package, version = NULL, project = ".") {
  toml_path <- file.path(project, "quone.toml")
  if (!file.exists(toml_path)) {
    cli::cli_abort("No quone.toml found at {.path {toml_path}}")
  }
  lines <- readLines(toml_path)
  entry <- if (is.null(version)) {
    sprintf('%s = "*"', package)
  } else {
    sprintf('%s = "%s"', package, version)
  }
  i <- which(grepl("^\\[dependencies\\]\\s*$", lines))
  if (length(i) == 0) {
    new <- c(lines, "", "[dependencies]", entry)
  } else {
    insert_at <- i[[1]] + 1
    new <- append(lines, entry, after = insert_at - 1)
  }
  writeLines(new, toml_path)
  cli::cli_alert_success("Added {.pkg {package}} to {.path {toml_path}}")
  invisible(new)
}


#' Remove a dependency from `quone.toml`
#'
#' @param package R package name to remove.
#' @param project Project root.
#' @return Invisibly, the updated `quone.toml` lines.
#' @export
remove_dependency <- function(package, project = ".") {
  toml_path <- file.path(project, "quone.toml")
  if (!file.exists(toml_path)) {
    cli::cli_abort("No quone.toml found at {.path {toml_path}}")
  }
  lines <- readLines(toml_path)
  pat <- sprintf("^%s\\s*=", package)
  new <- lines[!grepl(pat, lines)]
  writeLines(new, toml_path)
  cli::cli_alert_success("Removed {.pkg {package}} from {.path {toml_path}}")
  invisible(new)
}


render_template <- function(template_name, data) {
  path <- system.file(
    "templates", template_name,
    package = "quone",
    mustWork = FALSE
  )
  if (!nzchar(path) || !file.exists(path)) {
    path <- file.path("inst", "templates", template_name)
    if (!file.exists(path)) {
      cli::cli_abort("Template {.val {template_name}} not found")
    }
  }
  raw <- paste(readLines(path, warn = FALSE), collapse = "\n")
  for (key in names(data)) {
    raw <- gsub(
      paste0("{{", key, "}}"),
      data[[key]],
      raw,
      fixed = TRUE
    )
  }
  raw
}
