#' One-step setup for the Quone toolchain
#'
#' `setup()` is the recommended way to get going with `{quone}` from a
#' fresh R install. It walks you through:
#'
#' 1. installing the [`quonec`](https://github.com/quone-lang/compiler)
#'    compiler if it isn't already on your machine, and persisting
#'    its location to `~/.Renviron` so future R sessions find it
#'    automatically;
#' 2. wiring up Quone's language server in any LSP-aware editor it
#'    can detect (VS Code, Cursor, Positron, RStudio, Neovim, Helix,
#'    Zed);
#' 3. printing a short "what's next?" summary.
#'
#' By default `setup()` is interactive and asks you to confirm each
#' step that touches your machine. In non-interactive sessions
#' (`Rscript`, CI, etc.) it auto-confirms so it can run unattended;
#' you can force the same behaviour from an interactive session with
#' `setup(prompt = FALSE)`.
#'
#' @param prompt If `TRUE` (the default in interactive sessions), ask
#'   for confirmation before any install step. If `FALSE`, proceed
#'   without asking. In non-interactive sessions this is forced to
#'   `FALSE`.
#' @param editor Which editor(s) to configure. Passed through to
#'   [install_lsp()]; defaults to `"auto"` which configures every
#'   editor it detects.
#' @return Invisibly, a list with `compiler` (path to `quonec`) and
#'   `lsp` (the tibble [install_lsp()] returned, or `NULL` if the
#'   user declined).
#' @export
setup <- function(prompt = interactive(), editor = "auto") {
  prompt <- isTRUE(prompt) && interactive()

  cli::cli_h1("Welcome to {.pkg quone}")
  cli::cli_text(
    "This wizard installs the Quone compiler and registers the Quone \\
     language server in any editors it can find on your computer. \\
     You can re-run it any time to repair or update your setup."
  )

  compiler <- ensure_compiler(prompt = prompt)

  lsp_result <- if (is.null(compiler)) {
    NULL
  } else {
    ensure_lsp(prompt = prompt, editor = editor)
  }

  print_setup_summary(compiler = compiler, lsp = lsp_result)

  invisible(list(compiler = compiler, lsp = lsp_result))
}


# ---- helpers ---------------------------------------------------------


#' Make sure the compiler is installed; ask first when interactive.
#' Returns the compiler path on success, or NULL if the user declined.
#' @keywords internal
ensure_compiler <- function(prompt) {
  existing <- tryCatch(
    compiler_path(error = FALSE),
    error = function(e) NULL
  )
  if (!is.null(existing)) {
    cli::cli_h2("Step 1 of 2: Quone compiler")
    cli::cli_alert_success(
      "Found {.code quonec} at {.path {existing}}; nothing to do."
    )
    return(existing)
  }

  cli::cli_h2("Step 1 of 2: Install the Quone compiler")
  dest <- user_compiler_path()
  cli::cli_bullets(c(
    "i" = "{.pkg quone} needs the Quone compiler ({.code quonec}) to run.",
    "i" = "It's a small native binary -- about the size of an R package.",
    "i" = "It will be downloaded into your user data directory at \\
           {.path {dest}}.",
    "i" = "Its location will also be saved to {.file ~/.Renviron} so \\
           future R sessions can find it without rerunning this step."
  ))

  if (prompt && !confirm("Install the Quone compiler now?", default = TRUE)) {
    cli::cli_alert_info(
      "Skipped compiler install; run {.run quone::install_compiler()} \\
       later when you're ready."
    )
    return(NULL)
  }

  tryCatch(
    install_compiler(),
    error = function(e) {
      cli::cli_alert_danger(
        "Couldn't install the compiler automatically:"
      )
      cli::cli_text(conditionMessage(e))
      cli::cli_alert_info(
        "Try {.run quone::install_compiler(source = \"build-from-source\")} \\
         if you have {.code cabal} installed, or download a release by \\
         hand from {.url https://github.com/quone-lang/compiler/releases}."
      )
      NULL
    }
  )
}


#' Make sure editor LSP support is wired up; ask first when interactive.
#' @keywords internal
ensure_lsp <- function(prompt, editor) {
  detected <- if (identical(editor, "auto")) detect_editors() else editor
  cli::cli_h2("Step 2 of 2: Editor language-server support")

  if (length(detected) < 1L) {
    no_editors_detected_message()
    return(empty_lsp_result())
  }

  needs_vsix <- intersect(detected, c("vscode", "cursor", "positron"))
  cli::cli_bullets(c(
    "i" = "Found {length(detected)} editor{?s} we can configure: \\
           {.val {detected}}.",
    "i" = "We'll write a small config snippet for each.",
    if (length(needs_vsix) > 0L) {
      c(
        "i" = "We'll also install the bundled Quone extension into \\
               {.val {needs_vsix}}, building it locally with \\
               {.code npm} if it isn't already shipped."
      )
    }
  ))

  if (prompt &&
      !confirm("Configure these editors now?", default = TRUE)) {
    cli::cli_alert_info(
      "Skipped editor setup; run {.run quone::install_lsp()} later \\
       when you're ready."
    )
    return(NULL)
  }

  install_lsp(editor = editor, quiet = TRUE)
}


#' Print the closing "what's next?" summary.
#' @keywords internal
print_setup_summary <- function(compiler, lsp) {
  cli::cli_h1("Setup summary")

  if (is.null(compiler)) {
    cli::cli_alert_warning("Compiler: not installed.")
  } else {
    cli::cli_alert_success("Compiler: {.path {compiler}}")
  }

  if (is.null(lsp)) {
    cli::cli_alert_warning("Editor language server: not configured.")
  } else if (nrow(lsp) < 1L) {
    cli::cli_alert_warning("Editor language server: no editors configured.")
  } else {
    fully_ok <- lsp$installed %in% TRUE | is.na(lsp$installed)
    ready <- lsp$editor[fully_ok]
    not_ready <- lsp$editor[!fully_ok]
    if (length(ready) > 0L) {
      cli::cli_alert_success(
        "Editor language server: ready in {.val {ready}}."
      )
    }
    if (length(not_ready) > 0L) {
      cli::cli_alert_warning(
        "Editor language server: snippet written but extension not \\
         installed for {.val {not_ready}}."
      )
    }
  }

  cli::cli_h2("What's next?")
  cli::cli_bullets(c(
    "*" = "Restart any editor that was already open so it picks up the \\
           new extension.",
    "*" = "Open or create a {.file .Q} file in a configured editor to \\
           try the language server.",
    "*" = "Compile and run a script with {.run quone::run(\"hello.Q\")}.",
    "*" = "Re-check what's installed any time with \\
           {.run quone::lsp_status()} and {.run quone::compiler_version()}."
  ))
  invisible(NULL)
}


#' Tidyverse-style yes/no prompt that degrades gracefully when
#' `usethis` is missing or we're in a non-interactive session.
#' @keywords internal
confirm <- function(message, default = TRUE) {
  if (!interactive()) return(isTRUE(default))
  if (requireNamespace("usethis", quietly = TRUE)) {
    yes_choices <- c("Yes", "Sure", "Sounds good")
    no_choices <- c("No", "Not now", "Maybe later")
    return(isTRUE(usethis::ui_yeah(
      message,
      yes = yes_choices,
      no = no_choices,
      n_yes = 1,
      n_no = 1,
      shuffle = FALSE
    )))
  }
  ans <- utils::menu(c("Yes", "No"), title = message)
  identical(ans, 1L)
}
