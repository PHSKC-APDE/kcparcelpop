library('sf')
library('mapview')
library('arrow')
library('data.table')
library('dplyr')
library(future.apply)

plan(multisession(workers = 3))

year = 2025

# Load block data
## load block shape
bshp = read_sf("//dphcifs/APDE-CDIP/Shapefiles/Census_2020/block/kc_block.shp")

## load block pop
bpop = apde.data::population(years = year, geo_type = 'blk')

## Combine, and limit to blocks that have population
blks = merge(bshp, bpop[pop>0, .(GEOID20 = geo_id, pop)], by = 'GEOID20')

# load parcel data
## Load PINs of parcels with housing on them
housing = c(
  paste0("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/", year, "/condocomplex_extr.parquet"),
  paste0("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/",year,"/condounit_extr.parquet"),
  paste0("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/",year,"/resbldg_extr.parquet"),
  paste0("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/",year,"/aptcomplex_extr.parquet")
) |> lapply(read_parquet)

pinz = lapply(housing, function(x) setDT(x)[, .(PIN)]) |> rbindlist()
## Load parcel shapefile

parcel = read_sf(paste0("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/",year,"/parcel_area.shp"))

## limit to parcels with residential structions
housing_parcels = parcel |> filter(PIN %in% pinz$PIN)

# ## Create an outline shp
# housing_parcels_outline = housing_parcels |> summarize()

# Combine block and parcel
## Compare area of blocks
blks$start_area = st_area(blks)

## split blocks into tracts
tsplit = blks[,c('TRACTCE20', 'GEOID20'), drop = T] |> setDT()
tsplit[, tgrp := floor(.GRP/10), TRACTCE20]

blks = blks |> select(GEOID20, TRACTCE20, ALAND20, pop, start_area)

bsplit = split(blks, tsplit$tgrp)

## For each tract's worth of blocks, find the intersection and compute the remaining area
ans = future_lapply(bsplit, function(b){
  st = Sys.time()
  
  housep = st_join()
  
  hop = st_crop(housing_parcels_outline, st_bbox(b))
  b_si = st_intersection(b, hop)
  b_si$end_area = st_area(b_si)
  ed = Sys.time()
  print(ed-st)
  b_si
})

ans = bind_rows(ans)
ans = bind_rows(ans, blks |> filter(!GEOID20 %in% ans$GEOID20 & pop>0) |> mutate(end_area = start_area))
ans = ans |> mutate(fraction = as.numeric(round( 100 * end_area/start_area)))
ans = ans |> mutate(density = pop/as.numeric(units::set_units(end_area, 'km^2')))

## examine blocks with relatively high population and relatively low remaining area (e.g. UW)
a = ans |> filter(fraction < 5) |> arrange(+fraction) |> slice(5)
mapview(blks |> filter(GEOID20 %in% a$GEOID20)) + mapview(a)

# Sample points within intersection shape, roughly one point per 10 people.






