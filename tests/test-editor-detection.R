old_positron <- Sys.getenv("POSITRON", unset = NA_character_)
old_extensions_dir <- Sys.getenv("QUONE_POSITRON_EXTENSIONS_DIR", unset = NA_character_)
on.exit({
  if (is.na(old_positron)) {
    Sys.unsetenv("POSITRON")
  } else {
    Sys.setenv(POSITRON = old_positron)
  }
  if (is.na(old_extensions_dir)) {
    Sys.unsetenv("QUONE_POSITRON_EXTENSIONS_DIR")
  } else {
    Sys.setenv(QUONE_POSITRON_EXTENSIONS_DIR = old_extensions_dir)
  }
}, add = TRUE)

Sys.setenv(POSITRON = "1")
stopifnot(identical(quone:::detect_editor(), "positron"))

recovered_settings <- quone:::recover_editor_settings(c(
  "// Settings imported from Visual Studio Code",
  "{",
  '  "files.associations": {',
  '    "renv.lock": "json"',
  "  },",
  "<<<<<<< Existing",
  '  "workbench.colorTheme": "Default Positron Dark",',
  "=======",
  '  "workbench.colorTheme": "Default High Contrast",',
  ">>>>>>> Incoming",
  '  "workbench.startupEditor": "none"',
  "}"
))
stopifnot(identical(recovered_settings[["workbench.colorTheme"]], "Default Positron Dark"))
stopifnot(identical(recovered_settings[["workbench.startupEditor"]], "none"))
stopifnot(identical(recovered_settings[["files.associations"]][["renv.lock"]], "json"))

configured_settings <- quone:::apply_quone_editor_settings(
  list("workbench.colorTheme" = "Default Positron Dark"),
  "/tmp/quonec"
)
stopifnot(identical(configured_settings[["workbench.colorTheme"]], "Default Positron Dark"))
stopifnot(identical(configured_settings[["quone.compilerPath"]], "/tmp/quonec"))
stopifnot(identical(configured_settings[["[quone]"]][["editor.defaultFormatter"]], "quone-lang.quone"))
stopifnot(isTRUE(configured_settings[["[quone]"]][["editor.formatOnSave"]]))

if (nzchar(Sys.which("zip"))) {
  root <- tempfile("fake-vsix-")
  dir.create(file.path(root, "extension", "dist"), recursive = TRUE)
  writeLines(
    paste(
      "{",
      '  "name": "quone",',
      '  "publisher": "quone-lang",',
      '  "version": "0.0.1"',
      "}",
      sep = "\n"
    ),
    file.path(root, "extension", "package.json")
  )
  writeLines("", file.path(root, "extension", "dist", "extension.js"))
  vsix <- tempfile(fileext = ".vsix")
  old_wd <- getwd()
  setwd(root)
  utils::zip(vsix, list.files("extension", recursive = TRUE, full.names = TRUE))
  setwd(old_wd)

  extensions_dir <- tempfile("positron-extensions-")
  Sys.setenv(QUONE_POSITRON_EXTENSIONS_DIR = extensions_dir)
  result <- quone:::install_positron_extension(vsix)

  stopifnot(identical(result$method, "positron-extension-dir"))
  stopifnot(isTRUE(result$restart_required))
  stopifnot(dir.exists(file.path(extensions_dir, "quone-lang.quone-0.0.1")))

  registry <- jsonlite::read_json(
    file.path(extensions_dir, "extensions.json"),
    simplifyVector = FALSE
  )
  ids <- vapply(registry, function(entry) entry$identifier$id, character(1))
  stopifnot("quone-lang.quone" %in% ids)
}

