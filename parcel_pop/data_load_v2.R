library('data.table')
library('arrow')
library('duckdb')
library('DBI')
base = '//dphcifs/APDE-CDIP/Shapefiles/historical_parcel'
years = 2002:2024

db = DBI::dbConnect(duckdb::duckdb())
dbExecute(db,'install spatial; load spatial')
housing = lapply(years, function(y){

  left = file.path(base, y, 'beds_per_housing_unit_v2.parquet')
  right = file.path(base, y, 'parcel_area.shp')
  
  
  notcondo = dbGetQuery(db, glue::glue_sql(.con = db, 
  "select l.*, st_X(st_centroid(r.geom)) as X, st_Y(st_centroid(r.geom)) as Y
             from {left} as l
  left join st_read({right}) as r
  on l.MAJOR = r.MAJOR and l.MINOR = r.MINOR
  where NOT (type in ('Condo', 'Condo, Mobile'))
  "))  
  setDT(notcondo)
  
  condo = dbGetQuery(db, glue::glue_sql(.con = db, 
 "select l.*, st_X(st_centroid(r.geom)) as X, st_Y(st_centroid(r.geom)) as Y
             from {left} as l
  left join st_read({right}) as r
  on l.MAJOR = r.MAJOR
  where (type in ('Condo', 'Condo, Mobile'))
  "))  
  
  
  r = rbind(notcondo, condo)
  
  stopifnot(nrow(r) == dbGetQuery(db, glue::glue_sql(.con = db, 'select count(*) as N from {left}'))$N)
  
  return(r)
  
})

ub = fread(file.path(base, "UnitBreakdown", 'UnitBreakDown2022.csv'), integer64 = 'character')

hps = file.path(base, years, 'beds_per_housing_unit.parquet')

housing = lapply(hps, function(x) setDT(arrow::read_parquet(x)))
