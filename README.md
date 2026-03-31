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

### Methods

The point populations are created over several steps (described below). [ddb_block_parcel_pop.R](parcel_pop/ddb_block_parcel_pop.R) is the script that combines parcel data, Census block geographies, and OFM SADE population estimates into a set of sample points (with a population estimate). This process is conducted by year.

1.  Extract and standardize the parcel data via [extract_parceldata.R](misc/extract_parceldata.R). The parcel shapefile and the PIN codes for apartments, condos, and residential parcels are the main components for the parcel population process.
2.  Load Census blocks
3.  Use `apde.data::population` to get OFM SADE block level population estimates.
4.  Load parcel shapes and subset to those representing buildings that people can live in.
5.  Spatially intersect the living parcels with Census blocks that have population \> 0. 25% of a parcel's area must overlap with the target block to be included in the spatial intersection. Blocks with population, but no parcel coverage, are included with the entire block considered "liveable". The resulting spatial intersection creates the surface of possible places people can live.
6.  For each block (masked by living parcels), randomly spatially sample a point per `ceiling(pop/20)` people. Before spatial sampling, the area is negatively buffered by .25 survey feet to avoid some precision differences between various implementations of point-in-polygon predicates. Once sampled, the points are assigned a population value of `block population / # of points in the block`

### Storage

Intermediate steps are stored in year specific duckdb databases on cifs while the final data are stored on the ref schema on HHSAW.

### Comparison to the previous version

[A detailed comparison can be found here](misc/compare_methods.md). In short, this new method is easier and faster to compute/update without much loss in utility.
