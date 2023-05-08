# kcparcelpop

kcparcelpop is a data package. The primary dataset, accessed via `kcparcelpop::parcel_pop` , is sf-data.frame of points within King County and each point (parcel centriod) is assigned an estimate number of people living at that point. Parcel level population estimates derive from the number of beds in each parcel by type (small residential vs. apartment vs. condo) and calibrated to 2020 Census estimates at the tract level.

These population estimates should be considered "approximately correct" insofar as the spatial pattern is probably decent, but the actual estimate of people at a given parcel may be wonky. Beyond estimation error, folks living in group quarters or those with transient sleeping locations may not be fully counted/properly geocoded.

## Installation

You can install the development version of kcparcelpop from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("PHSKC-APDE/kcparcelpop")
```

## Usage

kcparcelpop is primarily designed to be used in conjunction with the [spatagg package](https://github.com/PHSKC-APDE/spatagg) to facilitate the crosswalking of estimates between non-nesting geographies.
