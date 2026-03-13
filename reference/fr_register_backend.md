# Register a Custom Render Backend

Registers a new output format backend so that
[`fr_render()`](https://vthanik.github.io/tlframe/reference/fr_render.md)
can produce output in that format. Third-party packages can use this to
add HTML, DOCX, or other formats without modifying tlframe source.

## Usage

``` r
fr_register_backend(format, extensions, render_fn, description = "")
```

## Arguments

- format:

  Character scalar. Format identifier (e.g., `"html"`).

- extensions:

  Character vector. File extensions that map to this format (e.g.,
  `c("html", "htm")`).

- render_fn:

  Function. The render function with signature
  `function(spec, page_groups, col_panels, path)`.

- description:

  Character scalar. Human-readable description.

## Value

Invisibly returns `NULL`.

## See also

[`fr_backends()`](https://vthanik.github.io/tlframe/reference/fr_backends.md)
to list registered backends,
[`fr_render()`](https://vthanik.github.io/tlframe/reference/fr_render.md).

## Examples

``` r
# Register a stub HTML backend
fr_register_backend(
  format = "html",
  extensions = c("html", "htm"),
  render_fn = function(spec, page_groups, col_panels, path) {
    writeLines("<html><body>stub table</body></html>", path)
  },
  description = "Stub HTML tables"
)

# Verify it appears in the backend list
fr_backends()
#>   format extensions               description
#> 1    rtf   rtf, doc          Rich Text Format
#> 2  latex        tex LaTeX source (tabularray)
#> 3    pdf        pdf           PDF via XeLaTeX
#> 4   html  html, htm          Stub HTML tables

# Use the custom backend
out <- file.path(tempdir(), "demo.html")
tbl_demog |> fr_table() |> fr_render(out)
readLines(out)
#> [1] "<html><body>stub table</body></html>"
unlink(out)
```
