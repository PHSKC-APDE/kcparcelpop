library('DBI')
library('duckdb')
library('sf')
library('arrow')
library('data.table')
library('dplyr')
library('future.apply')
years = 2020:2025
floc = "//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/"
plan(multisession(workers = 3))
reload_pop = FALSE
reload_blocks_n_parcels = FALSE
rerun_isect = FALSE
for(year in years){
  print(year)
  start = Sys.time()
  db_rw = DBI::dbConnect(duckdb::duckdb(), file.path(floc, year, 'parcel_pop.duckdb'))
  dbExecute(db_rw, "load spatial;")
  
  # Load in population data
  if(reload_pop){
    bpop = apde.data::population(years = year, geo_type = 'blk')
    dbWriteTable(db_rw, 'pop', bpop[, .(GEOID20 = geo_id, pop)], overwrite = T)
  }

  if(reload_blocks_n_parcels){
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
  }
  
  if(rerun_isect){
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
    
    rm(ans)
  }
  
  # Find out which ones are mostly water
  water = dbGetQuery(db_rw, 'select GEOID20, AWATER20, ALAND20, AWATER20 + ALAND20 as AREA from blks')
  setDT(water)
  water[, p_water := AWATER20/AREA]
  
  # # Blocks that are not well covered by parcels and are not water
  # chkme = ans |> filter(!GEOID20 %in% water[p_water > .5, GEOID20] & fraction < 15)
  
  # Fetch blocks that do not have housing parcels in them, but do have population
  # duckdb::duckdb_register(db_rw, 'ans', ans[, c('GEOID20', 'fraction'), drop = T], overwrite = T)
  # 
  blk_no_parcel = dbGetQuery(db_rw, glue::glue_sql(.con = db_rw,
  "
          select l.GEOID20, pop, st_aswkb(l.geom) as geom
          from blks as l
          left join iblks as r on l.GEOID20 = r.GEOID20
          where r.GEOID20 IS NULL and pop>0
  ")) |>
    mutate(geom = st_as_sfc(geom)) |>  
    st_sf(sf_column_name = 'geom', crs = 2926)
  
  blk_w_parcel = dbGetQuery(db_rw, "
              select l.GEOID20, pop, st_aswkb(r.geom) as geom
              from blks as l
              inner join iblks as r on l.GEOID20 = r.GEOID20 
            ") |> 
    mutate(geom = st_as_sfc(geom)) |>  
    st_sf(sf_column_name = 'geom', crs = 2926)
  

  presamp = rbind(blk_w_parcel[, c('GEOID20', 'pop')], blk_no_parcel[,c('GEOID20', 'pop')])
  presamp = presamp |> filter(pop>0) |>
    arrange(GEOID20) |> 
    mutate(npts = ceiling(pop/20)) |> 
    mutate(grp = floor(seq_len(n())/503)) |>
    st_buffer(units::set_units(-.25, 'us_survey_foot'))
  
  special_sample = function(pcl){
    ch = st_concave_hull(pcl, ratio = .8)
    init_points = st_sample(ch, sum(pcl$npts) * 3, type = 'random')
    init_points = st_sf(pid = seq_along(init_points), geom = init_points, sf_column_name = 'geom')
    
    # Assign to parcels
    init_points = st_join(init_points, pcl, left = FALSE)
    init_points = init_points |> group_by(GEOID20) |> mutate(gid = seq_len(n()))
    
    # compute doneness
    done = init_points |> st_drop_geometry() |> group_by(GEOID20) |> summarize(npts = first(npts), nsamp = n())
    notdone = done |> filter(nsamp<npts)
    
    # sample the rest
    gids = pcl |> filter(GEOID20 %in% unique(c(notdone$GEOID20, setdiff(pcl$GEOID20, done$GEOID20)))) 
    gids = merge(gids, notdone[, c('GEOID20', 'nsamp')], all.x = T, by = 'GEOID20')
    gids$nsamp[is.na(gids$nsamp)] <- 0
    gids = gids |> mutate(togo = npts - nsamp)
    remain = gids |> st_sample(gids$togo, type = 'random')
    remain = st_sf(
      GEOID20 = rep(gids$GEOID20, gids$togo),
      geom = remain, 
      sf_column_name = 'geom'
    )
    
    ans = rbind(
      init_points |> select(GEOID20),
      remain
    )
    
    ans = merge(ans, st_drop_geometry(pcl), by = 'GEOID20')
    ans = ans |> group_by(GEOID20) |> mutate(npts2 = n())
    
    stopifnot(all(ans$npts==ans$npts2))
    stopifnot(all(pcl$GEOID20 %in% ans$GEOID20))
    
    ans
    
  }
  
  # make the points
  pts = future_lapply(split(presamp, presamp$grp), function(poly){
    start = Sys.time()
    r = special_sample(poly)
    r$pop = r$pop/r$npts
    r = r[, c('GEOID20' ,'pop')]
    r
  }, future.seed = TRUE)
  
  pts = bind_rows(pts)
  
  stopifnot(all.equal(dbGetQuery(db_rw, 'select sum(pop) as p from pop')$p, sum(pts$pop)))
  
  ttt = tempfile(fileext = '.gpkg')
  sf::st_write(pts, ttt)
  dbExecute(db_rw, 'create or replace table pts as (
              select * from {`ttt`}
            )' |> glue::glue_sql(.con = db_rw))
  print(Sys.time() - start)
# parcel_pop = pts
# usethis::use_data(parcel_pop, overwrite = TRUE)
  dbDisconnect(db_rw, shutdown = T)
}


# Read all the results and compile them together
ans = lapply(years, function(y){
  db = dbConnect(duckdb::duckdb(), file.path(floc, y, 'parcel_pop.duckdb'), read_only = TRUE)
  dbExecute(db, 'load spatial;')
  r = dbGetQuery(db, 'select GEOID20, pop, st_aswkb(geom) as geom from pts') |>
    mutate(geom = st_as_sfc(geom), year = y) |>  
    st_sf(sf_column_name = 'geom', crs = 2926)
  dbDisconnect(db, shutdown = T)
  r
})

ans = bind_rows(ans)
ans$ptid = seq_len(nrow(ans))
bbox = st_bbox(ans)

# Convert points to xy columns
ans = cbind(st_drop_geometry(ans), st_coordinates(ans))

# # Round some rows to prevent floating point madness
# ans = ans |> mutate(pop = round(pop, 3),
#                     X = round(X, 3),
#                     Y = round(Y, 3))

# Creation date
ans$creation_date = Sys.Date()

con = apde.data::authenticate_hhsaw()
dbWriteTable(con, DBI::Id(schema = 'ref', table = 'kc_point_pop'), (ans), overwrite = T)

# Convert to a geometry object
dbExecute(con, 'alter table ref.kc_point_pop add geom geometry')
dbExecute(con, "update ref.kc_point_pop set geom = geometry::STGeomFromText(concat('POINT(', X, ' ', Y,')'), 2926)")

# add primary key
dbExecute(con, "
          alter table ref.kc_point_pop
          alter column ptid int not null
          
          ")
dbExecute(con, "
          alter table ref.kc_point_pop
          ADD CONSTRAINT PK_kpp_ptid PRIMARY KEY CLUSTERED (ptid);
          
          ")

# create indices
dbExecute(con, glue::glue_sql(.con = con,
  "
    create spatial index sindex_kpp_geom
    on [ref].[kc_point_pop](geom)
    with(BOUNDING_BOX = (
    xmin = {bbox['xmin']},
    ymin = {bbox['ymin']},
    xmax = {bbox['xmax']},
    ymax = {bbox['ymax']}
    ));
  
  "                            
))

dbExecute(con,
          "
            create index kpp_idx on ref.kc_point_pop(year)
          "
          )


# blah = dbGetQuery(con, 'select X,Y,geom.STAsText() as geom from ref.kc_point_pop') |>
#   mutate(geom = st_as_sfc(geom)) |>
#   st_sf(sf_column_name = 'geom', crs = 2926)
