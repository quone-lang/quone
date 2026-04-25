\name{quone-compiler}
\alias{check}
\alias{compile}
\alias{compile_dir}
\alias{compiler_path}
\alias{fmt}
\alias{install_compiler}
\alias{install_lsp}
\alias{invoke_compiler}
\alias{start}
\alias{write_demo}
\title{Quone compiler interface}
\description{
Minimal R interface for locating, installing, checking, formatting, and
compiling Quone source files, plus installing the editor integration and
writing a bundled demo file. Use \code{start()} for a guided first run that
sets up tooling, writes and opens a demo, compiles it, and opens the generated R.
}
\usage{
compiler_path(path = NULL, error = TRUE)
install_compiler(version = "latest", source = c("github-release", "build-from-source"), compiler_dir = NULL)
install_lsp(
  editor = c("auto", "code", "cursor", "positron"),
  version = "latest",
  source = c("github-release", "build-from-source"),
  extension_dir = NULL,
  compiler = NULL
)
start(
  path = "mtcars_summary.Q",
  editor = c("auto", "cursor", "code", "positron", "r"),
  install_lsp = TRUE,
  overwrite = FALSE,
  ask = interactive()
)
write_demo(path = "mtcars_summary.Q", name = c("mtcars_summary"), overwrite = FALSE)
compile(input, output = NULL, style = TRUE)
compile_dir(input_dir, output_dir, style = TRUE)
check(input)
fmt(input)
invoke_compiler(args, json = TRUE, compiler = NULL)
}
\arguments{
\item{path}{Optional compiler path for \code{compiler_path()}, or output path
for the bundled demo in \code{start()} and \code{write_demo()}.}
\item{error}{Whether to error when no compiler can be found.}
\item{version}{Compiler release version.}
\item{source}{Compiler or editor extension install source.}
\item{compiler_dir}{Optional compiler checkout for source builds.}
\item{editor}{Editor command to install into or open files with:
\code{"auto"}, \code{"cursor"}, \code{"code"}, \code{"positron"}, or
\code{"r"}. \code{"auto"} prefers the active Positron session when detected,
then falls back to editor commands on \code{PATH}.}
\item{extension_dir}{Optional path to the VS Code extension source directory.}
\item{install_lsp}{Whether \code{start()} should offer to install editor/LSP
support.}
\item{name}{Bundled demo name.}
\item{overwrite}{Whether to replace an existing demo output file.}
\item{ask}{Whether \code{start()} should ask before each setup step.}
\item{input}{A Quone source file.}
\item{output}{Optional generated R file or output directory.}
\item{style}{Whether to run \code{styler::style_file()} on generated R when
the optional \code{styler} package is installed.}
\item{input_dir}{Directory containing Quone source files.}
\item{output_dir}{Directory for generated R files.}
\item{args}{Compiler arguments.}
\item{json}{Whether to request JSON diagnostics.}
\item{compiler}{Optional compiler path.}
}
\value{
Paths or invisible success values, depending on the function.
}

