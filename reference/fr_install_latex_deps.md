# Install Required LaTeX Packages

Proactively installs all LaTeX packages needed by tlframe's PDF backend
via
[`tinytex::tlmgr_install()`](https://rdrr.io/pkg/tinytex/man/tlmgr.html).
Call this once after
[`tinytex::install_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html)
to ensure
[`fr_render()`](https://vthanik.github.io/tlframe/reference/fr_render.md)
can produce PDFs without interruption.

## Usage

``` r
fr_install_latex_deps()
```

## Value

Invisible `NULL`. Called for its side effect of installing packages.

## Details

The [tinytex](https://rdrr.io/pkg/tinytex/man/tinytex-package.html)
package must be installed. If it is not, `fr_install_latex_deps()`
raises an error with installation instructions.

Packages already present are silently skipped by `tlmgr`.

## See also

[`fr_latex_deps()`](https://vthanik.github.io/tlframe/reference/fr_latex_deps.md)
for the package list,
[`fr_render()`](https://vthanik.github.io/tlframe/reference/fr_render.md)
for PDF output.

## Examples

``` r
if (requireNamespace("tinytex", quietly = TRUE)) {
  fr_install_latex_deps()
}
#> ℹ Installing 7 LaTeX packages via tlmgr.
#> tlmgr update --all --self
#> tlmgr install tabularray fontspec geometry xcolor fancyhdr lastpage booktabs
```
