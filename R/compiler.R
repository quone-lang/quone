#' Locate the `quonec` compiler binary
#'
#' The discovery order is, in priority:
#'
#' 1. an explicit `path` argument;
#' 2. `getOption("quone.compiler_path")`;
#' 3. the `QUONEC` environment variable;
#' 4. `fs::which("quonec")`.
#'
#' @param path Optional explicit path to a `quonec` binary.
#' @param error If `TRUE` (default) signal an error when no compiler
#'   is found; if `FALSE`, return `NULL` instead.
#' @return A character path to the compiler, or `NULL` (when
#'   `error = FALSE` and nothing is found).
#' @export
compiler_path <- function(path = NULL, error = TRUE) {
  if (!is.null(path)) {
    if (!file.exists(path)) {
      if (error) {
        cli::cli_abort("`path` does not exist: {.path {path}}")
      }
      return(NULL)
    }
    return(normalizePath(path, mustWork = TRUE))
  }
  candidate <- getOption("quone.compiler_path")
  if (is.null(candidate) || !nzchar(candidate)) {
    candidate <- Sys.getenv("QUONEC", unset = "")
  }
  if (!nzchar(candidate)) {
    candidate <- unname(Sys.which("quonec"))
  }
  if (length(candidate) == 0 || !nzchar(candidate) ||
      !file.exists(candidate)) {
    if (error) {
      cli::cli_abort(c(
        "Could not find the {.code quonec} compiler.",
        i = "Run {.run quone::install_compiler()} to install it,",
        i = "or set {.code options(quone.compiler_path = \"...\")}."
      ))
    }
    return(NULL)
  }
  normalizePath(candidate, mustWork = TRUE)
}


#' The compiler's reported version string
#'
#' Runs `quonec version` and returns the parsed version, e.g.
#' `"0.0.1"`. The full reported text is preserved as the
#' `"raw"` attribute.
#' @return A character version string (length 1).
#' @export
compiler_version <- function() {
  out <- invoke_compiler("version", capture = TRUE)
  raw <- trimws(out$stdout)
  parsed <- regmatches(raw, regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", raw))
  if (length(parsed) == 0) parsed <- raw
  structure(parsed, raw = raw)
}


#' Install the `quonec` compiler
#'
#' Downloads a prebuilt `quonec` binary for the host platform and
#' installs it under `tools::R_user_dir("quone", "data")`. When the
#' download fails (or `source = "build-from-source"`), tries to
#' invoke `cabal install` against the sibling
#' [`quone-lang/compiler`](https://github.com/quone-lang/compiler)
#' repo.
#'
#' @param version Version to install. `"latest"` (the default) picks
#'   the most recent GitHub release.
#' @param source Where to fetch the compiler from. One of
#'   `"github-release"` (default) or `"build-from-source"`.
#' @return Invisibly, the path the binary was installed to.
#' @export
install_compiler <- function(
  version = "latest",
  source = c("github-release", "build-from-source")
) {
  source <- match.arg(source)
  dest_dir <- tools::R_user_dir("quone", "data")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, "quonec")

  if (source == "github-release") {
    cli::cli_alert_info("Downloading {.code quonec} {version} ...")
    asset <- release_asset_url(version)
    archive <- tempfile(fileext = ".tar.gz")
    on.exit(unlink(archive), add = TRUE)
    ok <- tryCatch({
      utils::download.file(asset, archive, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (ok && file.size(archive) > 0) {
      tmpdir <- tempfile()
      dir.create(tmpdir)
      on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
      utils::untar(archive, exdir = tmpdir)
      candidate <- list.files(
        tmpdir,
        pattern = "^quonec(\\.exe)?$",
        recursive = TRUE,
        full.names = TRUE
      )
      if (length(candidate) > 0) {
        file.copy(candidate[[1]], dest, overwrite = TRUE)
        Sys.chmod(dest, mode = "0755")
        cli::cli_alert_success(
          "Installed {.code quonec} to {.path {dest}}"
        )
        options(quone.compiler_path = dest)
        return(invisible(dest))
      }
    }
    cli::cli_alert_warning(
      "GitHub release download failed; falling back to source."
    )
    source <- "build-from-source"
  }

  if (source == "build-from-source") {
    cabal <- unname(Sys.which("cabal"))
    if (length(cabal) == 0 || !nzchar(cabal)) {
      cli::cli_abort(c(
        "{.code cabal} was not found on PATH.",
        i = "Install GHC + cabal via {.url https://www.haskell.org/ghcup/}."
      ))
    }
    sibling <- find_sibling_compiler()
    if (is.null(sibling)) {
      cli::cli_abort(c(
        "Could not locate a sibling {.path compiler/} directory.",
        i = "Clone {.url https://github.com/quone-lang/compiler} first."
      ))
    }
    cli::cli_alert_info("Building {.code quonec} from {.path {sibling}} ...")
    res <- processx::run(
      cabal,
      c("install", "exe:quonec",
        "--installdir", dest_dir, "--overwrite-policy=always"),
      wd = sibling,
      error_on_status = FALSE
    )
    if (res$status != 0) {
      cli::cli_abort("`cabal install` failed:\n{res$stderr}")
    }
    cli::cli_alert_success("Built {.code quonec} into {.path {dest_dir}}")
    options(quone.compiler_path = dest)
    invisible(dest)
  }
}


#' Build the URL of the prebuilt `quonec` release asset for the host
#'
#' Per `compiler/RELEASING.md` the canonical asset name is
#' `quonec-<os>-<arch>.tar.gz` with `os` in `c("macos", "linux",
#' "windows")` and `arch` in `c("x86_64", "arm64")`.
#'
#' Both this function and the compiler's release workflow use the
#' same name so the R-side download and the GitHub Actions upload
#' agree on a single string.
#' @keywords internal
release_asset_url <- function(version = "latest") {
  os <- detect_os()
  arch <- detect_arch()
  base <- "https://github.com/quone-lang/compiler/releases"
  segment <- if (identical(version, "latest")) {
    "latest/download"
  } else {
    paste0("download/", version)
  }
  sprintf(
    "%s/%s/quonec-%s-%s.tar.gz",
    base, segment, os, arch
  )
}


#' Normalised host OS string used in release-asset names
#' @keywords internal
detect_os <- function() {
  sys <- Sys.info()[["sysname"]]
  switch(
    sys,
    Darwin = "macos",
    Linux = "linux",
    Windows = "windows",
    tolower(sys)
  )
}


#' Normalised host arch string used in release-asset names
#' @keywords internal
detect_arch <- function() {
  m <- Sys.info()[["machine"]]
  if (m %in% c("x86_64", "amd64")) "x86_64"
  else if (m %in% c("aarch64", "arm64")) "arm64"
  else m
}


find_sibling_compiler <- function() {
  candidates <- c(
    file.path(getwd(), "..", "compiler"),
    file.path(getwd(), "compiler")
  )
  for (c in candidates) {
    if (dir.exists(c) && file.exists(file.path(c, "compiler.cabal"))) {
      return(normalizePath(c))
    }
  }
  NULL
}


#' Invoke the compiler with arbitrary arguments
#'
#' Low-level helper used by all the higher-level commands
#' ([build()], [check()], [run()], etc.). Diagnostics are always
#' requested in JSON form so the caller can parse them structurally.
#'
#' @param ... Arguments to pass to `quonec`. The
#'   `--diagnostics-format=json` flag is added automatically.
#' @param capture If `TRUE`, capture stdout/stderr; if `FALSE`, stream
#'   them to the calling process.
#' @param wd Working directory for the child process.
#' @param compiler Optional explicit compiler path; defaults to
#'   [compiler_path()].
#' @return A list with `status`, `stdout`, `stderr`.
#' @export
invoke_compiler <- function(
  ...,
  capture = TRUE,
  wd = NULL,
  compiler = NULL
) {
  bin <- compiler_path(compiler)
  args <- as.character(unlist(list(...)))
  if (!any(grepl("^--diagnostics-format=", args)) &&
      !"version" %in% args && !"--version" %in% args && !"--help" %in% args) {
    args <- c("--diagnostics-format=json", args)
  }
  res <- processx::run(
    bin,
    args,
    wd = wd,
    error_on_status = FALSE,
    spinner = FALSE,
    echo = !capture
  )
  list(status = res$status, stdout = res$stdout, stderr = res$stderr)
}


#' Compile a single `.Q` script
#'
#' @param path Path to the `.Q` file.
#' @param out Optional output directory. When `NULL` (the default),
#'   the generated `.R` is written next to `path`.
#' @param sourcemap If `TRUE` (default), also emit a `.R.map` sidecar
#'   used by [with_sourcemap()] for traceback rewriting.
#' @return Invisibly, the path to the generated `.R` file.
#' @export
build <- function(path, out = NULL, sourcemap = TRUE) {
  args <- list("build", path)
  if (!is.null(out)) {
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    args <- c(args, paste0("--out=", out))
  }
  if (isTRUE(sourcemap)) args <- c(args, "--emit-sourcemap")
  res <- do.call(invoke_compiler, args)
  if (res$status != 0) {
    abort_on_diagnostics(parse_ndjson(res$stderr))
  }
  out_path <- if (is.null(out)) {
    sub("\\.Q$", ".R", path)
  } else {
    file.path(out, sub("\\.Q$", ".R", basename(path)))
  }
  invisible(out_path)
}


#' Compile a multi-module Quone project into an R package
#'
#' @param project_dir Path to the project root (the directory holding
#'   `quone.toml`).
#' @param out Optional output directory; defaults to
#'   `<project_dir>/build`.
#' @param sourcemap If `TRUE` (default), also emit `.R.map` sidecars.
#' @param document If `TRUE` (default), invoke `roxygen2::roxygenise`
#'   over the resulting build directory to generate `NAMESPACE` and
#'   `man/`.
#' @return Invisibly, the path to the generated R package directory.
#' @export
build_package <- function(
  project_dir = ".",
  out = NULL,
  sourcemap = TRUE,
  document = TRUE
) {
  args <- list("build", "--package", project_dir)
  if (!is.null(out)) {
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    args <- c(args, paste0("--out=", out))
  }
  if (isTRUE(sourcemap)) args <- c(args, "--emit-sourcemap")
  res <- do.call(invoke_compiler, args)
  if (res$status != 0) {
    abort_on_diagnostics(parse_ndjson(res$stderr))
  }
  build_dir <- if (is.null(out)) file.path(project_dir, "build") else out
  if (isTRUE(document)) {
    if (!requireNamespace("roxygen2", quietly = TRUE)) {
      cli::cli_alert_warning(
        "{.pkg roxygen2} is not installed; skipping `document` step."
      )
    } else {
      roxygen2::roxygenise(build_dir)
    }
  }
  invisible(build_dir)
}


#' Type-check a `.Q` file
#'
#' Returns `TRUE` invisibly when the file type-checks; otherwise
#' raises a classed condition (see [abort_on_diagnostics()]).
#' @param path Path to a `.Q` file.
#' @return Invisibly, `TRUE`.
#' @export
check <- function(path) {
  res <- invoke_compiler("check", path)
  if (res$status != 0) {
    abort_on_diagnostics(parse_ndjson(res$stderr))
  }
  invisible(TRUE)
}


#' Compile and execute a `.Q` script
#'
#' @param path Path to a `.Q` file.
#' @param rscript If `TRUE` (default), shell out to `Rscript` and wait
#'   for it to finish. If `FALSE`, just compile and print the
#'   suggested command.
#' @param out Optional output directory for the generated `.R`.
#' @param sourcemap If `TRUE` (default), emit a `.R.map` sidecar.
#' @return Invisibly, the exit status of the underlying invocation.
#' @export
run <- function(path, rscript = TRUE, out = NULL, sourcemap = TRUE) {
  args <- list("run", path)
  if (isTRUE(rscript)) args <- c(args, "--rscript")
  if (!is.null(out)) {
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    args <- c(args, paste0("--out=", out))
  }
  if (isTRUE(sourcemap)) args <- c(args, "--emit-sourcemap")
  res <- do.call(invoke_compiler, args)
  if (res$status != 0) {
    abort_on_diagnostics(parse_ndjson(res$stderr))
  }
  invisible(res$status)
}


#' Scaffold a brand-new project (compiler-side `quonec new`)
#'
#' For most uses prefer the higher-level [create_project()], which
#' templates a project from R rather than the compiler.
#' @param name Project directory name.
#' @return Invisibly, the path to the new project.
#' @export
new_project <- function(name) {
  res <- invoke_compiler("new", name)
  if (res$status != 0) {
    abort_on_diagnostics(parse_ndjson(res$stderr))
  }
  invisible(name)
}
