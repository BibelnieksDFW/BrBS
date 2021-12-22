
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BrBS

<!-- badges: start -->

<!-- badges: end -->

The goal of BrBS is to provide convenient functions for reading portions
of the BBS database into R, performing exploratory analyses, and
creating visualizations.

## Installation

You can install the development version of BrBS from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BibelnieksDFW/BrBS")
```

## Usage

A typical workflow for exploring survey data begins by selecting an
island to look at, and then reading in the corresponding sheet from the
database.

``` r
library(BrBS)
## Read in Saipan data, get years, etc...
```

Once we have the data for that island read in, we may want to look at
survey conduct metrics over the years.

``` r
## Observer, number of stations, start times, which birds can we look at....
```

To look at trends in counts over the years, we need to first identify
the bird species we want to consider. Then we can plot basic trends over
time.

``` r
## Basic plotting of counts...
```
