
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DataEditR <img src="man/figures/logo.png" align="right" alt="" width="240"/>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis build
status](https://travis-ci.com/DillonHammill/DataEditR.svg?branch=master)](https://travis-ci.com/DillonHammill/DataEditR)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/DillonHammill/DataEditR?branch=master&svg=true)](https://ci.appveyor.com/project/DillonHammill/DataEditR)
[![CRAN
status](https://www.r-pkg.org/badges/version/DataEditR)](https://CRAN.R-project.org/package=DataEditR)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

Manual data entry and editing in R can be tedious, especially if you
have limited coding experience and are accustomed to using software with
a Graphical User Interface (GUI). **DataEditR** is an R package that
makes it easy to view, enter and edit data within R, due to its
convenient interactive GUI that supports many of the data manipulation
operations supported by other commonly used GUI-oriented software. If
you are new to **DataEditR** visit
<https://dillonhammill.github.io/DataEditR/> to get started.

## Installation

**DataEditR** can be installed from CRAN:

``` r
install.packages("DataEditR")
```

The development version of **DataEditR** can be installed directly from
GitHub:

``` r
library(devtools)
install_github("DillonHammill/DataEditR")
```

## Usage

**DataEditR** ships with three distinct shiny modules, namely
`dataInput`, `dataEdit` and `dataOutput` which have been wrapped up into
a single function called `data_edit()` to make it possible to
interactively view, enter or edit data in R. You can use `data_edit()`
as a standalone application, or include the relevant modules within your
own shiny applications. Alternatively, you can use the RStudio add-in
that ships with `DataEditR`.

#### General features:

  - RStudio add-in
  - flexible display options (either `dialog` box, `browser` or RStudio
    `viewer` pane)
  - fast rendering to quickly view datasets
  - ability to interactively create data.frames from scratch
  - load tabular data saved to file using any reading function
    (e.g. `read.csv()`)
  - save edited data to file using any writing function
    (e.g. `write.csv()`)
  - return appropriately formatted data as an R object for downstream
    use
  - code required to create edited data can be optionally printed to the
    console
  - support for custom themes through `shinythemes` package
  - customisable user interface (title, logo and modules)
  - row indices are always displayed for easy navigation
  - switch between datasets or files without having to leave the
    application

#### Data editing features:

  - edit row or column names
  - addition or removal of rows or columns
  - manual column resizing
  - drag to fill cells
  - copy or paste data to and from external software
  - custom column types to simplify user input (e.g. checkboxes and
    dropdown menus)
  - support for readonly columns to prevent users from editing certain
    columns
  - control over which column names can be edited
  - stretch columns horizontally to fill available space
  - programmatically add columns or rows to data prior to loading into
    the data editor

A quick demonstration of some of these features can be seen below, where
we use `data_edit()` to make changes to the `mtcars` dataset and save
the result to a new csv file:

``` r
# Load required packages
library(DataEditR)

# Save output to R object & csv file
mtcars_new <- data_edit(mtcars,
                        save_as = "mtcars_new.csv")
```

<img src="https://raw.githubusercontent.com/DillonHammill/DataEditR/master/man/figures/DataEditR/DataEditR-README.gif" width="100%" style="display: block; margin: auto;" />

## Credits

**DataEditR** is built using the fantastic
[rhandsontable](https://github.com/jrowen/rhandsontable) package.
**DataEditR** makes use of many features for entering and editing data,
but **rhandsontable** has support for much more sophisticated
interactive representations of data should you need them. The user
interface of **DataEditR** has been inspired by the
[editData](https://github.com/cardiomoon/editData) package which is a
great alternative to **DataEditR**.

## Code of Conduct

Please note that the **DataEditR** project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Citation

If you use **DataEditR** in your work, please cite the package as
follows:

``` r
citation("DataEditR")
#> 
#> To cite package 'DataEditR' in publications use:
#> 
#>   Dillon Hammill (2020). DataEditR: An Interactive Editor for Viewing,
#>   Entering & Editing Data. R package version 0.0.7.
#>   https://github.com/DillonHammill/DataEditR
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {DataEditR: An Interactive Editor for Viewing, Entering & Editing Data},
#>     author = {Dillon Hammill},
#>     year = {2020},
#>     note = {R package version 0.0.7},
#>     url = {https://github.com/DillonHammill/DataEditR},
#>   }
```
