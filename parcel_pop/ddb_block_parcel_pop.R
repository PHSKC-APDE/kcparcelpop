library('DBI')
library('duckdb')
library('sf')
library('arrow')
library('data.table')
library('dplyr')
library('future.apply')

floc = "//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/"
year = 2025
db_rw = DBI::dbConnect(duckdb::duckdb(), file.path(floc, 'parcel_pop.duckdb'))
dbExecute(db_rw, "load spatial;")

# Load in population data
bpop = apde.data::population(years = year, geo_type = 'blk')
dbWriteTable(db_rw, 'pop', bpop[, .(GEOID20 = geo_id, pop)], overwrite = T)

# Add block data to duckdb

dbExecute(db_rw, "
          create table blks as (
            select l.*, r.pop from st_read('//dphcifs/APDE-CDIP/Shapefiles/Census_2020/block/kc_block.shp') as l
            inner join pop as r on l.GEOID20 = r.GEOID20
          )
          ")

# Add housing parcels
## Load pins
housing = c(
  paste0("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/", year, "/condocomplex_extr.parquet"),
  paste0("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/",year,"/condounit_extr.parquet"),
  paste0("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/",year,"/resbldg_extr.parquet"),
  paste0("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/",year,"/aptcomplex_extr.parquet")
) |> lapply(read_parquet)

pinz = lapply(housing, function(x) setDT(x)[, .(PIN)]) |> rbindlist()
pinz = unique(pinz)
dbWriteTable(db_rw, 'hp_pins', pinz, overwrite = T)

## load parcels
pshp_path = paste0("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/",year,"/parcel_area.shp")
dbExecute(db_rw, glue::glue_sql(.con = db_rw,
     "
      create or replace table housing_parcels as (
        select l.* from st_read({`pshp_path`}) as l
        inner join hp_pins as r on l.PIN = r.PIN
      )
"))

# For each set of tracts
## list of tracts
lotrt = dbGetQuery(db_rw, 'select distinct TRACTCE20 from blks order by TRACTCE20')
lotrt = split(lotrt$TRACTCE20, floor(seq_along(lotrt$TRACTCE20)/10))


ans = lapply(lotrt, function(tlist){
  print(Sys.time())

  ## Pull the blks in that tract
  b = dbGetQuery(db_rw, glue::glue_sql(.con = db_rw,
      "
        select GEOID20, st_aswkb(geom) as geom from blks
        where TRACTCE20 in ({tlist*})
      ")) |>
    mutate(geom = st_as_sfc(geom)) |>  
    st_sf(sf_column_name = 'geom', crs = 2926)
  
  ## pull the parcels that intersect with the tract
  p = dbGetQuery(db_rw, glue::glue_sql(.con = db_rw,
      "
        with b as (
          select st_union_agg(geom) as geom from blks
          where TRACTCE20 in ({tlist*})
        )
        select l.MAJOR, l.MINOR, l.PIN, st_aswkb(l.geom) as geom from housing_parcels as l
        inner join b on st_intersects(l.geom, b.geom)
      ")) |>
    mutate(geom = st_as_sfc(geom)) |>  
    st_sf(sf_column_name = 'geom', crs = 2926)
  
  # keep only parcels that fit into a block at least 25%
  isect = st_intersection(
    p |> select(PIN) |> mutate(parea = st_area(p)),
    b |> select(GEOID20) |> mutate(barea = st_area(b))
  )
  isect = isect |> mutate(iarea = st_area(geom))
  isect = isect |> filter(as.numeric(iarea/parea) > .25)
  
  # Return to blocks
  iblk = isect |> group_by(GEOID20) |> summarize(original_area = first(barea))
  iblk = iblk |> mutate(final_area = st_area(geom))
  iblk = iblk |> mutate(fraction = round(100 * as.numeric(final_area/original_area)))
  iblk
})

# Combine
ans = bind_rows(ans)
ans = unique(ans)

# Save the results to the ddb
ttt = tempfile(fileext = '.gpkg')
st_write(obj = ans, ttt)
dbExecute(db_rw, 'create or replace table iblks as (
            select * from {`ttt`}
          )' |> glue::glue_sql(.con = db_rw))


# Find out which ones are mostly water
water = dbGetQuery(db_rw, 'select GEOID20, AWATER20, ALAND20, AWATER20 + ALAND20 as AREA from blks')
setDT(water)
water[, p_water := AWATER20/AREA]

# # Blocks that are not well covered by parcels and are not water
# chkme = ans |> filter(!GEOID20 %in% water[p_water > .5, GEOID20] & fraction < 15)

# Fetch blocks that do not have housing parcels in them, but do have population
duckdb::duckdb_register(db_rw, 'ans', ans[, c('GEOID20', 'fraction'), drop = T], overwrite = T)

blk_no_parcel = dbGetQuery(db_rw, glue::glue_sql(.con = db_rw,
"
        select l.GEOID20, pop, st_aswkb(geom) as geom
        from blks as l
        left join ans as r on l.GEOID20 = r.GEOID20
        where r.GEOID20 IS NULL and pop>0
")) |>
  mutate(geom = st_as_sfc(geom)) |>  
  st_sf(sf_column_name = 'geom', crs = 2926)

blk_w_parcel = dbGetQuery(db_rw, "
            select l.GEOID20, pop
            from blks as l
            inner join ans as r on l.GEOID20 = r.GEOID20 
          ") |> setDT()

ans = merge(ans, blk_w_parcel, by = 'GEOID20')

presamp = rbind(ans[, c('GEOID20', 'pop')], blk_no_parcel[,c('GEOID20', 'pop')])
presamp = presamp |> filter(pop>0) |>
  mutate(npts = ceiling(pop/20)) |> 
  mutate(grp = floor(seq_len(n())/1001))

# make the points
plan(multisession(workers = 3))
pts = future_lapply(split(presamp, presamp$grp), function(poly){
  start = Sys.time()
  r = st_sample(poly, poly$npts)
  a = st_sf(
    GEOID20 = rep(poly$GEOID20, poly$npts),
    pop = rep(poly$pop/poly$npts, poly$npts),
    geom = r, 
    sf_column_name = 'geom')
  
  print(Sys.time() - start)
  
  return(a)
  
}, future.seed = TRUE)

pts = bind_rows(pts)

stopifnot(all.equal(dbGetQuery(db_rw, 'select sum(pop) as p from pop')$p, sum(pts$pop)))

ttt = tempfile(fileext = '.gpkg')
sf::st_write(pts, ttt)
dbExecute(db_rw, 'create or replace table pts as (
            select * from {`ttt`}
          )' |> glue::glue_sql(.con = db_rw))
parcel_pop = pts
usethis::use_data(parcel_pop, overwrite = TRUE)
