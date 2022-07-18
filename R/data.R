#' Geometries of parcels with beds in King County
#' 
#' A dataset containing information related to all KC parcels that seem to have 
#' at least one bed in/on them (and studios)
#' 
#' @format A sf data frame
#' 
'housing_parcels_shp'

#' Bed counts per parcel in King County
#' 
#' A dataset containing information related to all KC parcels that seem to have 
#' at least one bed in/on them (and studios). Provides information about beds in
#' apartment buildings, condos, and residential (e.g. <= 4 units) on a given parcel.
#' It also contains the GEOID for the 2020 census tract that a given parcel
#'  geographically overlaps with the most (via st_join(largest = TRUE))
#' 
#' @format A data.table
#' 
'beds_per_parcel'