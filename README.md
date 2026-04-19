# quone

<!-- badges: start -->
[![R-CMD-check](https://github.com/quone-lang/quone/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/quone-lang/quone/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/quone)](https://CRAN.R-project.org/package=quone)
<!-- badges: end -->

The R companion for [Quone](https://quone-lang.org), a typed
functional language that compiles to readable R. `quone` is the
developer-experience layer: it discovers and drives the
[`quonec`](https://github.com/quone-lang/compiler) compiler,
scaffolds projects, watches for changes, surfaces compiler
diagnostics in your IDE, and registers a Quarto / knitr engine for
` ```{quone} ` chunks.

`quone` is for *authors* of Quone code. The R that `quonec`
generates has no runtime dependency on this package -- it depends
only on base R, [`dplyr`](https://dplyr.tidyverse.org),
[`purrr`](https://purrr.tidyverse.org),
[`readr`](https://readr.tidyverse.org), and any package you
foreign-import explicitly.

## Installation

You can install the development version of quone from
[GitHub](https://github.com/quone-lang/quone) with:

``` r
# install.packages("pak")
pak::pak("quone-lang/quone")
```

Then install the Quone compiler itself, which `quone` orchestrates:

``` r
quone::install_compiler()
```

## Usage

``` r
library(quone)

# Scaffold a new project
quone::create_project("scores")
setwd("scores")

# Add a module and an R-package dependency
quone::create_module("Stats.Summary")
quone::add_dependency("dplyr", ">= 1.1")

# Compile, run roxygen2, and load into the live R session.
# Polling is non-blocking; your prompt stays interactive.
quone::dev_session()
```

For a full tour, see `vignette("getting-started", package = "quone")`
or the four feature vignettes:

| Vignette                                                   | Topic                                              |
| ---------------------------------------------------------- | -------------------------------------------------- |
| `vignette("getting-started")`                              | install the compiler, build your first script     |
| `vignette("packages-and-roxygen")`                         | multi-module Quone packages, `document()`, install |
| `vignette("quarto-and-knitr")`                             | the `{quone}` engine for Quarto and R Markdown    |
| `vignette("debugging")`                                    | diagnostics, source maps, traceback rewriting     |

## Editor support

`quone::install_lsp()` wires the Quone Language Server (`quonec
lsp`) into VS Code, Positron, RStudio, Neovim, Helix, and Zed --
diagnostics, hover, completion, go-to-definition, and format-on-save
in any of them. RStudio addins for the formatter ship in the
package itself.

## Sibling projects

- **[quone-lang/compiler](https://github.com/quone-lang/compiler)** --
  the `quonec` compiler this package wraps. Includes the canonical
  language reference at `docs/LANGUAGE.md`.
- **[quone-lang/examples](https://github.com/quone-lang/examples)** --
  sample Quone programs.
- **[quone-lang/website](https://github.com/quone-lang/website)** --
  source for [quone-lang.org](https://quone-lang.org).

## Code of Conduct

Please note that the quone project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).
By contributing to this project, you agree to abide by its terms.
