library('sf')
library('arrow')
library('dplyr')
library('data.table')
library('duckdb')
library('DBI')
library('mapview')
blk = read_sf('//dphcifs/APDE-CDIP/Shapefiles/Census_2020/block/kc_block.shp')
parcel = read_sf("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/2022/parcel_area.shp")

isect = st_join(st_centroid(parcel), blk)[,c('PIN', 'GEOID20'), drop = T]
pyp = arrow::read_parquet('//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/2022/pin2blk.parquet')

setDT(isect)
setDT(pyp)
isect[, R := 1]
pyp[, P := 1]

r = merge(isect, pyp, by = c('PIN', 'GEOID20'), all = T)




d = DBI::dbConnect(duckdb())
dbExecute(d, 'install spatial; load spatial')

dbGetQuery(d, "create or replace table pcl as select PIN, ST_Centroid(geom) as geom from st_read('//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/2022/parcel_area.shp')")
dbGetQuery(d, "create or replace table blk as select * from st_read('//dphcifs/APDE-CDIP/Shapefiles/Census_2020/block/kc_block.shp')")

rd = dbGetQuery(d,
                "
                select l.PIN, r.GEOID20 from pcl as l
                inner join blk as r
                on st_intersects(r.geom, l.geom)
                
                ")
setDT(rd)

rd[, D:= 1]

rr = merge(r, rd, by = c('PIN', 'GEOID20'), all = T)

rr[is.na(R), R:= 0]
rr[is.na(P), P := 0]
rr[is.na(D), D := 0 ]
rr[, .N, keyby = .(R,P,D)]

# Examine a few examples of where R finds something, but not duckdb
r1 = rr[R == 1 & P==0][1,]
p1 = parcel |> filter(PIN %in% r1[, PIN])
b1 = blk |> filter(GEOID20 %in% r1[,GEOID20])
mapview(b1, col.regions = 'red') + mapview(p1, col.regions = 'blue') + mapview(st_centroid(p1), col.regions ='black')

psub = filter(parcel, PIN %in% rr[R==1 & P ==0, PIN]) |> filter(MINOR != 'UNKN')
blki = st_join(blk, psub) |> filter(!is.na(PIN))
mapview(blki, col.regions = 'blue') + mapview(psub, col.regions = 'red') + mapview(st_centroid(psub), col.region = 'black')
