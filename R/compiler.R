#' Locate the `quonec` compiler binary
#'
#' The discovery order is, in priority:
#'
#' 1. an explicit `path` argument;
#' 2. `getOption("quone.compiler_path")`;
#' 3. the `QUONEC` environment variable;
#' 4. a previously installed binary under
#'    `tools::R_user_dir("quone", "data")`;
#' 5. `Sys.which("quonec")`.
#'
#' Step 4 is what lets a freshly opened R session keep finding the
#' compiler after `install_compiler()` was run in a previous session.
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
    user_install <- user_compiler_path()
    if (file.exists(user_install)) {
      candidate <- user_install
    }
  }
  if (!nzchar(candidate)) {
    candidate <- unname(Sys.which("quonec"))
  }

  if (length(candidate) == 0 || !nzchar(candidate) ||
      !file.exists(candidate)) {
    if (error) {
      cli::cli_abort(c(
        "Couldn't find the Quone compiler ({.code quonec}).",
        i = "If this is your first time using {.pkg quone}, run \\
              {.run quone::setup()}.",
        i = "Otherwise install just the compiler with \\
              {.run quone::install_compiler()}."
      ))
    }
    return(NULL)
  }

  normalizePath(candidate, mustWork = TRUE)
}


#' Path that `install_compiler()` writes into.
#' @keywords internal
user_compiler_path <- function() {
  file.path(tools::R_user_dir("quone", "data"), "quonec")
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
#' Downloads a prebuilt `quonec` binary for your platform and stores
#' it under `tools::R_user_dir("quone", "data")`. The path is also
#' written to your user-level `~/.Renviron` (via [usethis::edit_r_environ()])
#' as `QUONEC=...` so future R sessions, and command-line `Rscript`
#' calls, can find it without rerunning the install.
#'
#' If the download fails (or `source = "build-from-source"`), the
#' function falls back to `cabal install` against a sibling clone of
#' [`quone-lang/compiler`](https://github.com/quone-lang/compiler).
#'
#' @param version Version to install. `"latest"` (the default) picks
#'   the most recent GitHub release.
#' @param source Where to fetch the compiler from. One of
#'   `"github-release"` (default) or `"build-from-source"`.
#' @param persist If `TRUE` (default), record the install location in
#'   your user `~/.Renviron` so a fresh R session can find it. Set to
#'   `FALSE` to keep the install scoped to the current session only.
#' @return Invisibly, the path the binary was installed to.
#' @export
install_compiler <- function(
  version = "latest",
  source = c("github-release", "build-from-source"),
  persist = TRUE
) {
  source <- match.arg(source)
  dest_dir <- tools::R_user_dir("quone", "data")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, "quonec")

  if (source == "github-release") {
    cli::cli_progress_step(
      "Downloading the Quone compiler ({.val {version}}) for your platform"
    )
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
        finalize_compiler_install(dest, persist = persist)
        return(invisible(dest))
      }
    }
    cli::cli_alert_warning(
      "Couldn't download a prebuilt compiler; trying to build one locally."
    )
    source <- "build-from-source"
  }

  if (source == "build-from-source") {
    cabal <- unname(Sys.which("cabal"))
    if (length(cabal) == 0 || !nzchar(cabal)) {
      cli::cli_abort(c(
        "Couldn't find {.code cabal} on your PATH.",
        i = "Install GHC + cabal from {.url https://www.haskell.org/ghcup/} \\
              and try again, or download a release binary by hand from \\
              {.url https://github.com/quone-lang/compiler/releases}."
      ))
    }
    sibling <- find_sibling_compiler()
    if (is.null(sibling)) {
      cli::cli_abort(c(
        "Couldn't find a sibling {.path compiler/} checkout to build from.",
        i = "Clone {.url https://github.com/quone-lang/compiler} next to \\
              this folder, or pass {.arg source = \"github-release\"} once \\
              a release is available."
      ))
    }
    cli::cli_progress_step(
      "Building the Quone compiler from {.path {sibling}} (this may take a few minutes)"
    )
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
    finalize_compiler_install(dest, persist = persist)
    invisible(dest)
  }
}


#' Finalize a successful compiler install: store path for this session,
#' optionally persist it to ~/.Renviron, and tell the user what happened
#' in plain language.
#' @keywords internal
finalize_compiler_install <- function(dest, persist = TRUE) {
  options(quone.compiler_path = dest)
  Sys.setenv(QUONEC = dest)

  cli::cli_alert_success(
    "Installed the Quone compiler to {.path {dest}}."
  )

  if (isTRUE(persist)) {
    persisted <- tryCatch(
      persist_compiler_path(dest),
      error = function(e) FALSE
    )
    if (isTRUE(persisted)) {
      cli::cli_alert_success(
        "Recorded {.envvar QUONEC} in your user {.file ~/.Renviron} so \\
         a fresh R session can find the compiler automatically."
      )
    } else {
      cli::cli_alert_info(c(
        "Couldn't update {.file ~/.Renviron} for you.",
        i = "Add this line to it manually so future R sessions find the \\
             compiler:",
        " " = "{.code QUONEC={dest}}"
      ))
    }
  }

  invisible(dest)
}


#' Append `QUONEC=<path>` to the user-level `.Renviron`, replacing any
#' previous quone-managed entry. Returns `TRUE` on success.
#' @keywords internal
persist_compiler_path <- function(dest) {
  if (!requireNamespace("usethis", quietly = TRUE)) {
    return(FALSE)
  }

  renviron <- path.expand("~/.Renviron")
  marker <- "# Added by quone::install_compiler()"
  new_lines <- c(marker, sprintf("QUONEC=%s", dest))

  current <- if (file.exists(renviron)) {
    readLines(renviron, warn = FALSE)
  } else {
    character()
  }

  marker_idx <- which(current == marker)
  if (length(marker_idx) > 0L) {
    keep <- seq_len(marker_idx[[1L]] - 1L)
    after <- marker_idx[[1L]] + 2L
    rest <- if (after <= length(current)) {
      current[seq.int(after, length(current))]
    } else {
      character()
    }
    current <- c(current[keep], rest)
  }

  prev <- options(usethis.quiet = TRUE)
  on.exit(options(prev), add = TRUE)
  usethis::write_union(renviron, c(current, new_lines), quiet = TRUE)
  TRUE
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
