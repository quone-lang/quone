\name{quone-compiler}
\alias{check}
\alias{compile}
\alias{compile_dir}
\alias{compiler_path}
\alias{fmt}
\alias{install_compiler}
\alias{install_lsp}
\alias{invoke_compiler}
\alias{write_demo}
\title{Quone compiler interface}
\description{
Minimal R interface for locating, installing, checking, formatting, and
compiling Quone source files, plus installing the editor integration and
writing a bundled demo file.
}
\usage{
compiler_path(path = NULL, error = TRUE)
install_compiler(version = "latest", source = c("github-release", "build-from-source"))
install_lsp(
  editor = c("code", "cursor", "positron"),
  version = "latest",
  source = c("github-release", "build-from-source"),
  extension_dir = NULL,
  compiler = NULL
)
write_demo(path = "mean_score.Q", name = c("mean_score"), overwrite = FALSE)
compile(input, output = NULL)
compile_dir(input_dir, output_dir)
check(input)
fmt(input)
invoke_compiler(args, json = TRUE, compiler = NULL)
}
\arguments{
\item{path}{Optional compiler path.}
\item{error}{Whether to error when no compiler can be found.}
\item{version}{Compiler release version.}
\item{source}{Compiler or editor extension install source.}
\item{editor}{Editor command to install into: \code{"code"}, \code{"cursor"},
or \code{"positron"}.}
\item{extension_dir}{Optional path to the VS Code extension source directory.}
\item{name}{Bundled demo name.}
\item{overwrite}{Whether to replace an existing demo output file.}
\item{input}{A Quone source file.}
\item{output}{Optional generated R file or output directory.}
\item{input_dir}{Directory containing Quone source files.}
\item{output_dir}{Directory for generated R files.}
\item{args}{Compiler arguments.}
\item{json}{Whether to request JSON diagnostics.}
\item{compiler}{Optional compiler path.}
}
\value{
Paths or invisible success values, depending on the function.
}

