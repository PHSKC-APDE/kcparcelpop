#' Fetch population points
#' @param year numeric vector of the year(s) to pull. Defaults to most recent.
#' @param geog_window A sf data.frame (presumably of polygons). Only points intersecting the union of the input shapes will be returned.
#' @param table DBI::Id of the table containing the point pop
#' @param con a DBI data base connection
#' @importFrom DBI dbGetQuery SQL
#' @importFrom glue glue_sql glue
#' @importFrom sf st_transform st_union st_as_text st_sf st_as_sfc st_buffer st_intersects
#' @returns An sf data.frame of points and a column describing the number of people approximately at that point
#' @export
point_pop = function(year, geog_window, table = DBI::Id(schema = 'ref', table = 'kc_point_pop'), con = apde.data::authenticate_hhsaw()){
  
  years = DBI::dbGetQuery(con, glue::glue_sql(.con = con, "select distinct year from {`table`}"))
  
  if(missing(year)){
    year <- years$year |> max()
  }
  
  year_dif = setdiff(year , years$year)
  if(length(year_dif)>0){
    year_dif = paste0(year_dif, collapse = ', ')
    err_msg = glue::glue("Year(s): {year_dif} not available.")
    stop(err_msg)
  }
  
  if(!missing(geog_window)){
    base = sf::st_transform(geog_window, crs = 2926) |> sf::st_union()
    # buffered = base |> sf::st_buffer(1) |> sf::st_buffer(0)
    gq = base |> sf::st_as_text()
    gq1 = glue::glue_sql(.con = con, "
                        with bbox as (
                          select
                          geometry::STGeomFromText({gq}, 2926) as geom
                        )")
    gq2 = SQL('inner join bbox as r on l.geom.STIntersects(r.geom) = 1')
  }else{
    gq1 = SQL('')
    gq2 = SQL('')
  }
  
  ans = DBI::dbGetQuery(con, glue::glue_sql(.con = con,
    "
      {gq1}
      select ptid, GEOID20, pop, year, creation_date, l.geom.STAsText() as geom 
      from {`table`} as l
      {gq2}
      where year in ({year*})
    "))
  
  if(nrow(ans)==0){
    stop('0 rows retrieved. If you specified a geog_window, make sure the geography is within King County. \n
         If you have checked that and you still get an error, something is goofy in the geographic subset in SQL. \n
         Consider running the function without a geog_window argument and subsetting after the fact.')
  }
  
  ans$geom = sf::st_as_sfc(ans$geom)
  ans = sf::st_sf(ans, sf_column_name = 'geom', crs = 2926)
  
  # If the geography window was called, use SF to standardize the pull
  if(!missing(geog_window)) ans = ans[st_intersects(ans, base, sparse = F)[,1],]
  
  ans

}
