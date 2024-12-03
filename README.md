
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quickinspectR

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

The goal of **quickinspectR** is to make it easier (and quicker) for
beginner R programmers to graphically inspect their data. The package is
centered around an expanding collection of simple `inspect` functions
that help users visualize their data.

Currently, this package supports:

- `inspect_normality`: graphically inspect the distribution of numeric
  variables in your data.
- `inspect_balance`: graphically inspect the class balance (or lack
  thereof) in your data.
- `inspect_missing`: graphically inspect missingness in your data.

## Installation

You can install the development version of **quickinspectR** from
[GitHub](https://github.com/andrewfullerton/quickinspectR) with:

``` r
# install.packages("devtools")
devtools::install_github("andrewfullerton/quickinspectR")
```

## Examples

By design, all **quickinspectR** functions require only one argument: a
data frame or a tibble (`data`). Unless otherwise specified, functions
will display all the relevant variables contained in the data frame and
default to easy-to-read plot styling.

To get started, load the package.

``` r
library(quickinspectR)
```

If you want to see how the numeric variables in your data are
distributed (e.g. check if they are skewed), you can use
`inspect_normality`.

``` r
inspect_normality(data = iris)
```

<img src="man/figures/README-example1-1.png" width="100%" />

If you want to check for class imbalance (e.g. unequal distribution of
classes within categorical variables), you can use `inspect_balance`.
**Tip:** if you’re only interested in a few key variables in your data,
then you can use the `vars` argument to manually specify which variables
will be displayed.

``` r
inspect_balance(data = palmerpenguins::penguins, vars = c("species", "island"))
```

<img src="man/figures/README-example2-1.png" width="100%" />

If you want to quickly see if you’re missing any data (and more
importantly: where you’re missing data), then you can use
`inspect_missing`.

``` r
inspect_missing(data = palmerpenguins::penguins)
```

<img src="man/figures/README-example3-1.png" width="100%" />

Even though all `inspect` functions will run with `data` as their sole
argument, there may be times when you want to stylize your plots a bit
more. To make this more accessible, each `inspect` function contains
several additional (but completely optional) arguments to enable basic
plot customization. Here’s an example using `inspect_missing`:

``` r
inspect_missing(data = palmerpenguins::penguins,
                na_colour = "purple", # Change colour used to represent missing values
                fill_colour = "darkgreen", # Change colour used to represent non-missing values
                title = "Missing values in penguins dataset") # Add title
```

<img src="man/figures/README-example4-1.png" width="100%" />

And while advanced plot customization is not the goal of
**quickinspectR**, all its functions are built on top of **ggplot2** and
support more advanced customization via additional arguments. To learn
more about this, you can read this vignette.

## Thanks for checking out quickinspectR!
