# Generate tidyverse code to replicate data edits

`data_code` compares an original dataset with an edited version and
generates tidyverse-style R code (using `dplyr` verbs) that transforms
the original data into the edited version. The generated code uses
`rename()`, `select()`, `mutate()`, `slice()`, and
[`tibble::add_row()`](https://tibble.tidyverse.org/reference/add_row.html)
to replicate column renames, column additions/removals, cell value
changes, and row additions/removals. When meaningful row names are
present (i.e. non-default row names such as those in `mtcars`), the
generated code preserves them using
[`tibble::rownames_to_column()`](https://tibble.tidyverse.org/reference/rownames.html)
and
[`tibble::column_to_rownames()`](https://tibble.tidyverse.org/reference/rownames.html).

## Usage

``` r
data_code(x, x_edit, name = "data")
```

## Arguments

- x:

  original data prior to editing, a `data.frame` or `matrix`. If `NULL`,
  creation code will be generated for `x_edit`.

- x_edit:

  edited data after modifications, a `data.frame` or `matrix`.

- name:

  character string for the name of the data object in the generated
  code, set to `"data"` by default.

## Value

a character string containing tidyverse code that can be printed to the
console or written to an R script.

## Author

Dillon Hammill, <dillon.hammill21@gmail.com>

## Examples

``` r
# original data
x <- data.frame(
  A = c(1, 2, 3),
  B = c("x", "y", "z"),
  stringsAsFactors = FALSE
)

# edited data (changed a value and renamed a column)
x_edit <- data.frame(
  A = c(1, 5, 3),
  Beta = c("x", "y", "z"),
  stringsAsFactors = FALSE
)

# generate code
cat(data_code(x, x_edit, name = "x"))
#> library(dplyr)
#> 
#> x <- x %>%
#>   rename(Beta = B) %>%
#>   mutate(A = replace(A, 2, 5))
```
