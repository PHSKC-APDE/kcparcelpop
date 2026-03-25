# kcparcelpop

kcparcelpop, via the `kcparcelpop::point_pop` function, allows users to access sub-block population estimates for King County (partially derived from parcels). `point_pop` returns an sf-data.frame of points within King County and each point is assigned an estimated number of people living at that point.

These population estimates should be considered "approximately correct" insofar as the spatial pattern is probably decent, but the actual estimate of people at a given location may be wonky.

## Installation

You can install the development version of kcparcelpop from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("PHSKC-APDE/kcparcelpop")
```

## Usage

kcparcelpop is primarily designed to be used in conjunction with the [spatagg package](https://github.com/PHSKC-APDE/spatagg) to facilitate the crosswalking of estimates between non-nesting geographies.
