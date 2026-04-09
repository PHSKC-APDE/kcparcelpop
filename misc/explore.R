library('mapview')
library('dplyr')
library('kcparcelpop')
library('sf')

new_pp = kcparcelpop::point_pop(2022)
old_pp = read_sf("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/old_parcel_pop.gpkg")

hra = st_read("//dphcifs/APDE-CDIP/Shapefiles/HRA/hra_2020.shp", quiet = T)

uw = hra |> filter(name == 'Seattle - University District')
old_uw = old_pp |> st_join(uw, left = FALSE)
new_uw = new_pp |> st_join(uw, left = FALSE)
mapview(uw) + mapview(old_uw, col.regions = 'red') + mapview(new_uw, col.regions = 'gold')
