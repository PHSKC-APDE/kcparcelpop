library('sf')
library('data.table')
ofol = '//dphcifs/APDE-CDIP/Shapefiles/historical_parcel'
ub_fol = file.path(ofol, 'UnitBreakDown2000_2022')
db_loc = '//gisdw/kclib/Plibrary/_ALL_HISTORIC_DATA/'
dbs = list.files(db_loc, pattern = 'Year', full.names = T)
dbs = rev(dbs)
for(db in dbs){
  print(basename(db))
  y = as.integer(substr(tools::file_path_sans_ext(basename(db)),5,8))
  print(glue::glue('{y}: {Sys.time()}'))
  extr = sf::st_layers(db)
  
  # has parcels
  hasparcels = 'parcel_area' %in% extr$name
  
  if(hasparcels){
    dir.create(file.path(ofol, y))
    # Copy parcel shape
    # gdalUtilities::ogr2ogr(src_datasource_name = db,
    #                        dst_datasource_name = file.path(ofol, y, 'parcel_area.gpkg'),
    #                        layer = 'parcel_area',
    #                        overwrite = T)
    
    # # Extra files
    extraz = extr$name[grep('extr', tolower(extr$name))]
    for(eee in extraz){
      a = st_read(db, eee)
      # gdalUtilities::ogr2ogr(src_datasource_name = db,
      #                        dst_datasource_name = file.path(ofol, y, paste0(eee, )),
      #                        layer = 'parcel_area')
      arrow::write_parquet(a, file.path(ofol, y, tolower(paste0(eee, '.parquet'))))
    }

  }
  # %>% filter(grepl(layer_name, 'extr'))
  

}

# convert all the xlsx tables to csvs
fff = list.files(ub_fol, pattern = '.xlsx', full.names = T)
for(f in fff){
  r = openxlsx2::read_xlsx(f)
  fwrite(r, file = file.path(dirname(f), paste0(tools::file_path_sans_ext(basename(f)), '.csv')))
}
