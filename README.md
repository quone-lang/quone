# quone

`quone` installs and invokes the Quone compiler from R.

Quone lets you write safer data transformation code in a typed functional
language, then compile it to readable R. The generated R does not depend on this
package.

The package is intentionally small for the first release: install the compiler,
write a demo, check code, compile code, format code, and install the
VS Code-compatible language extension.

## Five Minute Quickstart

```r
# install.packages("pak")
pak::pak("quone-lang/quone")

quone::start()
```

`quone::start()` asks before each step: installing the compiler and editor
support, writing and opening `mtcars_summary.Q`, compiling it, and opening the
generated `mtcars_summary.R`. The generated file is ordinary R, which is the
point: Quone should make authoring safer without locking collaborators into a
runtime they cannot inspect.

## API

```r
quone::start()
quone::install_compiler()
quone::install_lsp()
quone::write_demo("mtcars_summary.Q")
quone::check("analysis.Q")
quone::compile("analysis.Q")
quone::compile_dir("src", "build")
quone::fmt("analysis.Q")
```

## More Information

- Language spec: `compiler/docs/LANGUAGE.md`
- Initial release scope: `compiler/docs/INITIAL_RELEASE_PLAN.md`
- Examples: <https://github.com/quone-lang/examples>

