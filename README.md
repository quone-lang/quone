# quone

`quone` is the minimal R package for installing and invoking the Quone compiler.
It is for authors of Quone code; generated R does not depend on this package.

Quone is early and experimental. The first release focuses on compiling typed
dataframe workflows to readable R.

## Installation

```r
# install.packages("pak")
pak::pak("quone-lang/quone")
quone::install_compiler()
quone::install_lsp("code")    # or "cursor" / "positron"
```

## Usage

```r
quone::check("analysis.Q")
quone::compile("analysis.Q")
quone::compile_dir("src", "build")
quone::fmt("analysis.Q")
```

For language details, see the compiler repository's `docs/LANGUAGE2.md` and
`docs/INITIAL_RELEASE_PLAN.md`.

