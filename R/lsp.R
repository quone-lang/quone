#' Wire `quonec lsp` into LSP-aware editors
#'
#' Detects which editors are present on the host and registers
#' `quonec lsp` as the language server for `*.Q` files. For editors
#' that need a thin extension shipped from the compiler repo
#' (VS Code, Positron) the function downloads or builds the
#' extension; for terminal editors it writes copy-pasteable config
#' snippets to `tools::R_user_dir("quone", "config")`.
#'
#' @param editor One of `"auto"`, `"vscode"`, `"positron"`,
#'   `"rstudio"`, `"neovim"`, `"helix"`, `"zed"`. `"auto"` (the
#'   default) installs into every editor it can detect.
#' @return Invisibly, a tibble with one row per editor configured.
#' @export
install_lsp <- function(
  editor = c("auto", "vscode", "positron", "rstudio", "neovim", "helix", "zed")
) {
  editor <- match.arg(editor)
  bin <- compiler_path()
  config_dir <- tools::R_user_dir("quone", "config")
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)

  candidates <- if (identical(editor, "auto")) {
    detect_editors()
  } else editor

  rows <- lapply(candidates, function(name) {
    install_one_editor(name, bin, config_dir)
  })
  result <- do.call(rbind, rows)
  invisible(result)
}


#' Report which editors currently have `quonec lsp` wired up
#'
#' @return A tibble with `editor`, `installed`, `version`, `config_path`.
#' @export
lsp_status <- function() {
  config_dir <- tools::R_user_dir("quone", "config")
  bin <- tryCatch(compiler_path(error = FALSE), error = function(e) NULL)
  ver <- if (!is.null(bin)) compiler_version() else NA_character_
  rows <- lapply(known_editors(), function(name) {
    snippet <- editor_config_path(name, config_dir)
    tibble::tibble(
      editor = name,
      installed = file.exists(snippet),
      version = ver,
      config_path = snippet
    )
  })
  do.call(rbind, rows)
}


#' Remove `quonec lsp` configuration for an editor
#'
#' @param editor Editor to uninstall from. `"all"` removes every
#'   editor's snippet.
#' @return Invisibly, `TRUE`.
#' @export
lsp_uninstall <- function(editor = "all") {
  config_dir <- tools::R_user_dir("quone", "config")
  editors <- if (identical(editor, "all")) known_editors() else editor
  for (e in editors) {
    p <- editor_config_path(e, config_dir)
    if (file.exists(p)) file.remove(p)
  }
  cli::cli_alert_success("Uninstalled LSP config for: {.val {editors}}")
  invisible(TRUE)
}


# ---- helpers ---------------------------------------------------------


known_editors <- function() {
  c("vscode", "positron", "rstudio", "neovim", "helix", "zed")
}


detect_editors <- function() {
  Filter(function(e) {
    bin <- editor_binary(e)
    !is.null(bin) && nzchar(bin) && file.exists(bin)
  }, known_editors())
}


editor_binary <- function(name) {
  bin <- switch(
    name,
    vscode = "code",
    positron = "positron",
    rstudio = "rstudio",
    neovim = "nvim",
    helix = "hx",
    zed = "zed"
  )
  unname(Sys.which(bin))
}


editor_config_path <- function(name, config_dir) {
  ext <- switch(
    name,
    vscode = "vscode.json",
    positron = "positron.json",
    rstudio = "rstudio.json",
    neovim = "neovim.lua",
    helix = "helix.toml",
    zed = "zed.json"
  )
  file.path(config_dir, ext)
}


install_one_editor <- function(name, bin, config_dir) {
  snippet_path <- editor_config_path(name, config_dir)
  body <- editor_snippet(name, bin)
  writeLines(body, snippet_path)

  # For VS Code and Positron the snippet alone is not enough; the
  # editor needs the bundled extension installed via its CLI.
  if (name %in% c("vscode", "positron")) {
    install_vsix_into(name)
  }

  cli::cli_alert_success(
    "Wrote {.path {snippet_path}} for {.val {name}}"
  )
  tibble::tibble(
    editor = name,
    installed = TRUE,
    config_path = snippet_path
  )
}


# Install the bundled Quone VS Code extension into VS Code or Positron.
#
# Looks for a .vsix shipped at compiler/editors/vscode/dist/ (sibling
# repo) or in inst/extdata/ (when bundled into the R package). If
# neither is present, prints an actionable next-step; we never fail
# install_lsp() over a missing extension since the snippet is still
# useful for diagnostics.
install_vsix_into <- function(editor) {
  cli <- editor_binary(editor)
  if (!nzchar(cli)) {
    cli::cli_alert_warning(
      "{.val {editor}} CLI not found on PATH; install the extension by hand."
    )
    return(invisible(FALSE))
  }
  vsix <- find_bundled_vsix()
  if (is.null(vsix)) {
    cli::cli_alert_info(
      paste0(
        "Snippet written, but no bundled .vsix found. To finish, ",
        "build the extension once with `cd compiler/editors/vscode ",
        "&& npm install && npm run package`, then re-run install_lsp()."
      )
    )
    return(invisible(FALSE))
  }
  res <- processx::run(
    cli,
    c("--install-extension", vsix, "--force"),
    error_on_status = FALSE
  )
  if (res$status == 0L) {
    cli::cli_alert_success(
      "Installed {.path {vsix}} into {.val {editor}}"
    )
    invisible(TRUE)
  } else {
    cli::cli_alert_warning(
      paste0(
        editor, " --install-extension exited ", res$status, ":\n",
        res$stderr
      )
    )
    invisible(FALSE)
  }
}


# Look for a bundled Quone .vsix.
#
# Checks (in order):
#   1. inst/extdata/quone-*.vsix shipped with this R package;
#   2. ../compiler/editors/vscode/dist/quone-*.vsix when running from
#      a sibling clone of quone-lang/compiler;
#   3. NULL when neither is present.
find_bundled_vsix <- function() {
  bundled <- list.files(
    system.file("extdata", package = "quone"),
    pattern = "quone-.*\\.vsix$",
    full.names = TRUE
  )
  if (length(bundled) > 0L) return(bundled[[1]])

  sibling <- file.path("..", "compiler", "editors", "vscode", "dist")
  if (dir.exists(sibling)) {
    cand <- list.files(
      sibling, pattern = "quone-.*\\.vsix$", full.names = TRUE
    )
    if (length(cand) > 0L) return(normalizePath(cand[[1]]))
  }
  NULL
}


editor_snippet <- function(name, bin) {
  switch(
    name,
    vscode = vscode_snippet(bin),
    positron = vscode_snippet(bin),
    rstudio = rstudio_snippet(bin),
    neovim = neovim_snippet(bin),
    helix = helix_snippet(bin),
    zed = zed_snippet(bin)
  )
}


vscode_snippet <- function(bin) {
  jsonlite::toJSON(
    list(
      "languageclient.serverCommands" = list(
        list(
          languageId = "quone",
          command = list(bin, "lsp"),
          fileExtensions = list("Q")
        )
      )
    ),
    pretty = TRUE,
    auto_unbox = TRUE
  )
}


rstudio_snippet <- function(bin) {
  jsonlite::toJSON(
    list(
      lsp = list(
        quone = list(
          command = bin,
          args = list("lsp"),
          filetypes = list("Q")
        )
      )
    ),
    pretty = TRUE,
    auto_unbox = TRUE
  )
}


neovim_snippet <- function(bin) {
  c(
    "-- Add to your neovim config:",
    "vim.api.nvim_create_autocmd('FileType', {",
    "  pattern = 'quone',",
    "  callback = function()",
    sprintf(
      "    vim.lsp.start({ name = 'quonec', cmd = { '%s', 'lsp' } })",
      bin
    ),
    "  end,",
    "})"
  )
}


helix_snippet <- function(bin) {
  c(
    "# Add to your ~/.config/helix/languages.toml",
    "[language-server.quonec]",
    sprintf("command = \"%s\"", bin),
    "args = [\"lsp\"]",
    "",
    "[[language]]",
    "name = \"quone\"",
    "scope = \"source.quone\"",
    "file-types = [\"Q\"]",
    "language-servers = [\"quonec\"]"
  )
}


zed_snippet <- function(bin) {
  jsonlite::toJSON(
    list(
      lsp = list(
        quonec = list(
          command = bin,
          arguments = list("lsp")
        )
      ),
      languages = list(
        Quone = list(
          language_servers = list("quonec"),
          file_types = list("Q")
        )
      )
    ),
    pretty = TRUE,
    auto_unbox = TRUE
  )
}
