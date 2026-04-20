#' Wire `quonec lsp` into LSP-aware editors
#'
#' Detects which editors are installed on your computer and configures
#' them so they recognise `*.Q` files and talk to the Quone language
#' server (`quonec lsp`). VS Code, Cursor, and Positron also get the
#' bundled Quone extension installed automatically; Neovim, Helix and
#' Zed get a copy-pasteable snippet under
#' `tools::R_user_dir("quone", "config")`.
#'
#' Most R users should run [setup()] instead -- it calls this for you
#' after making sure the compiler is installed.
#'
#' @param editor Which editor to configure. `"auto"` (the default)
#'   sets up every editor it can detect; otherwise pick one of
#'   `"vscode"`, `"cursor"`, `"positron"`, `"rstudio"`, `"neovim"`,
#'   `"helix"`, or `"zed"`.
#' @param quiet If `TRUE`, skip the chatty per-step messages. The
#'   final summary is always printed.
#' @return Invisibly, a tibble with one row per editor configured.
#' @export
install_lsp <- function(
  editor = c(
    "auto", "vscode", "cursor", "positron",
    "rstudio", "neovim", "helix", "zed"
  ),
  quiet = FALSE
) {
  editor <- match.arg(editor)
  bin <- compiler_path()
  config_dir <- tools::R_user_dir("quone", "config")
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)

  if (!isTRUE(quiet)) {
    cli::cli_h1("Setting up Quone editor support")
    cli::cli_bullets(c(
      "*" = "Compiler: {.path {bin}}",
      "*" = "Editor configs will be written to {.path {config_dir}}"
    ))
  }

  candidates <- if (identical(editor, "auto")) {
    detect_editors()
  } else editor

  if (length(candidates) < 1L) {
    no_editors_detected_message()
    return(invisible(empty_lsp_result()))
  }

  if (!isTRUE(quiet)) {
    if (identical(editor, "auto")) {
      cli::cli_alert_info(
        "Detected {length(candidates)} editor{?s}: {.val {candidates}}"
      )
    } else {
      cli::cli_alert_info("Configuring {.val {candidates}}")
    }
  }

  rows <- lapply(candidates, function(name) {
    install_one_editor(name, bin, config_dir, quiet = quiet)
  })
  result <- do.call(rbind, rows)

  print_install_lsp_summary(result)

  invisible(result)
}


# Render the closing "what worked / what didn't" summary so the user
# never has to guess whether the editor extension was actually
# installed.
print_install_lsp_summary <- function(result) {
  fully_ok <- result$installed %in% TRUE | is.na(result$installed)
  needs_attention <- result[!fully_ok, , drop = FALSE]

  cli::cli_h2("Editor support summary")

  for (i in seq_len(nrow(result))) {
    row <- result[i, ]
    needs_vsix <- !is.na(row$extension_installed)
    if (needs_vsix && isTRUE(row$extension_installed)) {
      cli::cli_alert_success(
        "{.val {row$editor}}: ready (extension installed; restart the \\
         editor if it was already open)."
      )
    } else if (!needs_vsix) {
      cli::cli_alert_success(
        "{.val {row$editor}}: snippet written to {.path {row$config_path}}."
      )
    } else {
      cli::cli_alert_warning(
        "{.val {row$editor}}: snippet written, but the bundled \\
         extension wasn't installed -- {row$extension_message}"
      )
    }
  }

  cli::cli_text("")
  cli::cli_bullets(c(
    "i" = "Open a {.file .Q} file in your editor and watch the status \\
           bar -- the language server should report it has connected.",
    "i" = "Re-check what's wired up at any time with \\
           {.run quone::lsp_status()}."
  ))

  if (nrow(needs_attention) > 0L) {
    cli::cli_alert_info(
      "Re-run {.run quone::install_lsp()} after fixing the items above."
    )
  }
}


empty_lsp_result <- function() {
  tibble::tibble(
    editor = character(),
    installed = logical(),
    config_path = character()
  )
}


no_editors_detected_message <- function() {
  cli::cli_h2("No supported editors detected")
  cli::cli_bullets(c(
    "i" = "{.pkg quone} looks for these editors on your {.envvar PATH}: \\
           {.val {known_editors()}}.",
    "i" = "If you use one of them, install its command-line launcher \\
           (e.g. \"Shell Command: Install 'code' command in PATH\" in \\
           VS Code) and re-run {.run quone::install_lsp()}.",
    "i" = "Otherwise, configure your editor by hand using one of \\
           {.code install_lsp(\"vscode\")}, {.code install_lsp(\"cursor\")}, \\
           {.code install_lsp(\"positron\")}, {.code install_lsp(\"rstudio\")}, \\
           {.code install_lsp(\"neovim\")}, {.code install_lsp(\"helix\")}, \\
           or {.code install_lsp(\"zed\")}."
  ))
}


#' Report which editors currently have `quonec lsp` wired up
#'
#' Use this any time to double-check what [setup()] or [install_lsp()]
#' has done.
#'
#' Each row reports:
#'   * `editor` -- the editor name;
#'   * `snippet_written` -- whether [install_lsp()] has written a
#'     config snippet for it;
#'   * `extension_installed` -- for VS Code / Cursor / Positron only,
#'     whether the bundled Quone extension is registered with the
#'     editor (`NA` for editors that don't use the extension);
#'   * `version` -- the compiler version;
#'   * `config_path` -- where the snippet lives on disk.
#'
#' @return A tibble.
#' @export
lsp_status <- function() {
  config_dir <- tools::R_user_dir("quone", "config")
  bin <- tryCatch(compiler_path(error = FALSE), error = function(e) NULL)
  ver <- if (!is.null(bin)) compiler_version() else NA_character_
  rows <- lapply(known_editors(), function(name) {
    snippet <- editor_config_path(name, config_dir)
    needs_vsix <- name %in% c("vscode", "cursor", "positron")
    extension_installed <- if (needs_vsix) {
      detect_quone_extension(name)
    } else NA
    tibble::tibble(
      editor = name,
      snippet_written = file.exists(snippet),
      extension_installed = extension_installed,
      version = ver,
      config_path = snippet
    )
  })
  do.call(rbind, rows)
}


# Best-effort: ask the editor's CLI whether *our* extension is
# installed. Returns NA when the CLI isn't on PATH.
detect_quone_extension <- function(editor) {
  editor_cli <- editor_binary(editor)
  if (!nzchar(editor_cli)) return(NA)
  res <- processx::run(
    editor_cli, c("--list-extensions"),
    error_on_status = FALSE
  )
  if (res$status != 0L) return(NA)
  installed <- strsplit(res$stdout, "\\s+")[[1]]
  any(installed == "quone-lang.quone")
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
  c("vscode", "cursor", "positron", "rstudio", "neovim", "helix", "zed")
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
    cursor = "cursor",
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
    cursor = "cursor.json",
    positron = "positron.json",
    rstudio = "rstudio.json",
    neovim = "neovim.lua",
    helix = "helix.toml",
    zed = "zed.json"
  )
  file.path(config_dir, ext)
}


install_one_editor <- function(name, bin, config_dir, quiet = FALSE) {
  snippet_path <- editor_config_path(name, config_dir)
  body <- editor_snippet(name, bin)
  writeLines(body, snippet_path)

  needs_vsix <- name %in% c("vscode", "cursor", "positron")

  if (needs_vsix) {
    ext <- install_vsix_into(name, quiet = quiet)
    extension_installed <- isTRUE(ext$installed)
    extension_message <- ext$message
  } else {
    extension_installed <- NA
    extension_message <- NA_character_
    if (!isTRUE(quiet)) {
      cli::cli_alert_success(
        "{.val {name}}: wrote config to {.path {snippet_path}}."
      )
    }
  }

  tibble::tibble(
    editor = name,
    installed = if (needs_vsix) extension_installed else TRUE,
    config_path = snippet_path,
    extension_installed = extension_installed,
    extension_message = extension_message
  )
}


# Install the bundled Quone VS Code / Cursor / Positron extension.
#
# Returns a list with `installed` (TRUE/FALSE) and `message` (a short
# human-readable string suitable for a per-editor summary line).
#
# Behaviour:
#   1. If the editor's CLI is missing, report it and return FALSE.
#   2. Find a .vsix: prefer one shipped at inst/extdata/, then
#      compiler/editors/vscode/dist/. If neither exists but a
#      compiler/editors/vscode/ source checkout is present, build a
#      .vsix via [build_vsix()] (with a clear progress message).
#   3. If a previous, non-matching "quone.quone-lang" extension is
#      installed, uninstall it first so VS Code/Cursor doesn't keep
#      loading the broken stub.
#   4. Run `<editor> --install-extension <vsix> --force`.
install_vsix_into <- function(editor, quiet = FALSE) {
  editor_cli <- editor_binary(editor)
  if (!nzchar(editor_cli)) {
    msg <- paste0(
      editor, ": command-line launcher not on PATH; bundled extension ",
      "not installed."
    )
    cli::cli_alert_warning(
      "{.val {editor}}: command-line launcher not found on {.envvar PATH}, \\
       so the Quone extension wasn't installed. The config snippet is in \\
       place; install the editor's CLI launcher and re-run \\
       {.run quone::install_lsp(\"{editor}\")}."
    )
    return(list(installed = FALSE, message = msg))
  }

  vsix <- find_bundled_vsix()
  if (is.null(vsix)) {
    src <- find_vsix_source()
    if (is.null(src)) {
      msg <- paste0(
        editor, ": no bundled extension and no source checkout to ",
        "build one from."
      )
      if (!isTRUE(quiet)) {
        cli::cli_alert_info(
          "{.val {editor}}: no bundled extension shipped, and no \\
           {.path compiler/editors/vscode} checkout was found next to \\
           this folder, so I can't build one for you. Clone \\
           {.url https://github.com/quone-lang/compiler} as a sibling \\
           directory and re-run {.run quone::install_lsp(\"{editor}\")}."
        )
      }
      return(list(installed = FALSE, message = msg))
    }
    if (!isTRUE(quiet)) {
      cli::cli_alert_info(
        "{.val {editor}}: no bundled extension yet -- building one from \\
         {.path {src}}."
      )
    }
    vsix <- tryCatch(
      build_vsix(src = src, quiet = quiet),
      error = function(e) {
        cli::cli_alert_danger(
          "{.val {editor}}: couldn't build the bundled extension: \\
           {conditionMessage(e)}"
        )
        NULL
      }
    )
    if (is.null(vsix)) {
      msg <- paste0(editor, ": extension build failed.")
      return(list(installed = FALSE, message = msg))
    }
  }

  remove_stale_extension(editor_cli, editor, quiet = quiet)

  res <- processx::run(
    editor_cli,
    c("--install-extension", vsix, "--force"),
    error_on_status = FALSE
  )
  if (res$status != 0L) {
    cli::cli_alert_warning(
      "{.val {editor}}: {.code --install-extension} exited with status \\
       {.val {res$status}}:\n{res$stderr}"
    )
    return(list(
      installed = FALSE,
      message = paste0(
        editor, ": install command failed with status ", res$status, "."
      )
    ))
  }

  set_quone_compiler_path_setting(editor, quiet = quiet)

  if (!isTRUE(quiet)) {
    cli::cli_alert_success(
      "{.val {editor}}: installed bundled extension from \\
       {.path {basename(vsix)}}."
    )
  }
  list(
    installed = TRUE,
    message = paste0(editor, ": installed ", basename(vsix), ".")
  )
}


# Write the absolute path of `quonec` into the editor's user
# `settings.json` under `"quone.compilerPath"`. Without this the
# extension launches `quonec` from PATH; for users who installed via
# `install_compiler()` or via cabal, that path won't have `quonec` on
# it and the language server silently fails to start. We never blow
# up the install for this; on any failure we just leave a hint.
set_quone_compiler_path_setting <- function(editor, quiet = FALSE) {
  settings_path <- editor_user_settings_path(editor)
  if (is.null(settings_path)) return(invisible(FALSE))

  bin <- tryCatch(compiler_path(error = FALSE), error = function(e) NULL)
  if (is.null(bin)) return(invisible(FALSE))

  ok <- tryCatch({
    dir.create(
      dirname(settings_path),
      recursive = TRUE,
      showWarnings = FALSE
    )

    settings <- if (file.exists(settings_path)) {
      raw <- paste(readLines(settings_path, warn = FALSE), collapse = "\n")
      if (!nzchar(trimws(raw))) {
        list()
      } else {
        jsonlite::fromJSON(raw, simplifyVector = FALSE)
      }
    } else {
      list()
    }
    settings[["quone.compilerPath"]] <- bin
    writeLines(
      jsonlite::toJSON(settings, pretty = TRUE, auto_unbox = TRUE),
      settings_path
    )
    TRUE
  }, error = function(e) {
    cli::cli_alert_info(
      "{.val {editor}}: couldn't update {.path {settings_path}} \\
       automatically -- add {.code \"quone.compilerPath\": \"{bin}\"} \\
       to it by hand so the extension can find {.code quonec}."
    )
    FALSE
  })

  if (isTRUE(ok) && !isTRUE(quiet)) {
    cli::cli_alert_success(
      "{.val {editor}}: pointed {.code quone.compilerPath} at \\
       {.path {bin}} in {.path {basename(settings_path)}}."
    )
  }
  invisible(ok)
}


# Per-editor user settings.json location. Returns NULL when we don't
# know where the editor stores its user settings (so we just skip).
editor_user_settings_path <- function(editor) {
  os <- Sys.info()[["sysname"]]
  user_dir <- switch(
    editor,
    vscode = vscode_settings_dir(os, "Code"),
    cursor = vscode_settings_dir(os, "Cursor"),
    positron = vscode_settings_dir(os, "Positron"),
    NULL
  )
  if (is.null(user_dir)) return(NULL)
  file.path(user_dir, "User", "settings.json")
}


vscode_settings_dir <- function(os, app) {
  switch(
    os,
    Darwin = file.path(
      Sys.getenv("HOME"), "Library", "Application Support", app
    ),
    Linux = file.path(
      Sys.getenv("XDG_CONFIG_HOME", file.path(Sys.getenv("HOME"), ".config")),
      app
    ),
    Windows = file.path(
      Sys.getenv("APPDATA", file.path(Sys.getenv("USERPROFILE"), "AppData",
                                      "Roaming")),
      app
    ),
    NULL
  )
}


#' Build the bundled VS Code / Cursor / Positron extension
#'
#' Runs `npm install` and `npm run package` inside the
#' `compiler/editors/vscode/` source tree to produce a `.vsix` that
#' [install_lsp()] and [setup()] can install into VS Code, Cursor, or
#' Positron. Most users won't need to call this directly --
#' `install_lsp()` calls it for you when no prebuilt extension is
#' shipped.
#'
#' @param src Optional path to the extension source. Defaults to the
#'   sibling `../compiler/editors/vscode/` checkout. An error is
#'   raised if no source can be found.
#' @param quiet If `TRUE`, hide `npm` output (errors are still
#'   printed). Defaults to `FALSE`.
#' @return The absolute path to the freshly built `.vsix` (invisibly).
#' @export
build_vsix <- function(src = NULL, quiet = FALSE) {
  if (is.null(src)) {
    src <- find_vsix_source()
  }
  if (is.null(src) || !dir.exists(src)) {
    cli::cli_abort(c(
      "Couldn't find the extension source.",
      i = "Expected {.path ../compiler/editors/vscode/} next to this \\
            folder, or pass an explicit {.arg src}."
    ))
  }
  src <- normalizePath(src)

  npm <- unname(Sys.which("npm"))
  if (!nzchar(npm)) {
    cli::cli_abort(c(
      "Couldn't find {.code npm} on your {.envvar PATH}.",
      i = "Install Node.js from {.url https://nodejs.org/} and try again."
    ))
  }

  cli::cli_progress_step(
    "Installing npm dependencies in {.path {src}}"
  )
  res <- processx::run(
    npm, c("install", "--no-audit", "--no-fund"),
    wd = src,
    error_on_status = FALSE,
    echo = !isTRUE(quiet)
  )
  if (res$status != 0L) {
    cli::cli_abort(c(
      "{.code npm install} failed in {.path {src}}.",
      i = "Try running it by hand to see what went wrong."
    ))
  }

  cli::cli_progress_step("Packaging the Quone extension as a .vsix")
  res <- processx::run(
    npm, c("run", "package"),
    wd = src,
    error_on_status = FALSE,
    echo = !isTRUE(quiet)
  )
  if (res$status != 0L) {
    cli::cli_abort(
      "{.code npm run package} failed in {.path {src}}."
    )
  }

  vsix <- list.files(
    file.path(src, "dist"),
    pattern = "quone-.*\\.vsix$",
    full.names = TRUE
  )
  if (length(vsix) == 0L) {
    cli::cli_abort(
      "{.code npm run package} succeeded but no {.file *.vsix} was \\
       written under {.path {file.path(src, 'dist')}}."
    )
  }
  cli::cli_alert_success(
    "Built bundled extension {.path {basename(vsix[[1]])}}."
  )
  invisible(normalizePath(vsix[[1]]))
}


# Remove a stale, hand-installed Quone extension that doesn't match
# the publisher of the one we ship. VS Code/Cursor won't load two
# extensions for the same language, so a leftover from an earlier
# version (publisher `quone`, name `quone-lang`) silently breaks our
# install. We uninstall it best-effort.
remove_stale_extension <- function(editor_cli, editor, quiet = FALSE) {
  res <- processx::run(
    editor_cli,
    c("--list-extensions"),
    error_on_status = FALSE
  )
  if (res$status != 0L) return(invisible(FALSE))

  installed <- strsplit(res$stdout, "\\s+")[[1]]
  installed <- installed[nzchar(installed)]
  stale <- intersect(installed, c("quone.quone-lang", "quone-lang.quone-lang"))
  if (length(stale) == 0L) return(invisible(FALSE))

  for (id in stale) {
    if (!isTRUE(quiet)) {
      cli::cli_alert_info(
        "{.val {editor}}: removing previous {.val {id}} extension that \\
         conflicts with the new one."
      )
    }
    processx::run(
      editor_cli,
      c("--uninstall-extension", id),
      error_on_status = FALSE
    )
  }
  invisible(TRUE)
}


# Look for a prebuilt Quone .vsix.
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

  src <- find_vsix_source()
  if (!is.null(src)) {
    cand <- list.files(
      file.path(src, "dist"),
      pattern = "quone-.*\\.vsix$",
      full.names = TRUE
    )
    if (length(cand) > 0L) return(normalizePath(cand[[1]]))
  }
  NULL
}


# Locate the extension source checkout.
#
# Looks for a sibling compiler/editors/vscode/ directory (the layout
# of the quone-lang monorepo). Returns NULL when no source is found.
find_vsix_source <- function() {
  candidates <- c(
    file.path(getwd(), "..", "compiler", "editors", "vscode"),
    file.path(getwd(), "compiler", "editors", "vscode")
  )
  for (c in candidates) {
    if (dir.exists(c) && file.exists(file.path(c, "package.json"))) {
      return(normalizePath(c))
    }
  }
  NULL
}


editor_snippet <- function(name, bin) {
  switch(
    name,
    vscode = vscode_snippet(bin),
    cursor = vscode_snippet(bin),
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
