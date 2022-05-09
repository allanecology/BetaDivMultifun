
<!-- README.md is generated from README.Rmd by devtools::build_readme(). Please edit the .Rmd file -->

create random conflict from desktop

# BetaDivMultifun

<!-- badges: start -->
<!-- badges: end -->

The goal of BetaDivMultifun is to …

## Installation

For installation, please consider the vignette
`how-to-use-this-package.Rmd`.

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("allanecology/BetaDivMultifun")
```

## Content

-   The scripts you need are in the folder vignettes

\#TODO : - add a content description (TOC) - add the script overview
image

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(BetaDivMultifun)
## basic example code
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

# creating `assembled_functions` dataset

is described in 3 scripts in another github folder :
<https://github.com/biodiversity-exploratories-synthesis/2019_grassland_functions>

In this package, the required dataset is just read in and an analysis is
performed. As the dataset was constructed from within this directory,
the commits can be looked up here.

After installing the package and being connected to the “planteco” drive
at IPS: \* find the file `nonpublic.R`, it is stored at the “planteco”
drive where the data is. \* run `nonpublic.R` \* run
`1read_raw_datasets.Rmd` then `2calc_raw_dataset.R` \* the file
`"~/Desktop/december2018_assembled_functions_dataset.csv"` will be
written on your Desktop.
