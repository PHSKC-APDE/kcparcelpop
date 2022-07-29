#' Centriods of parcels with beds in King County
#' 
#' A dataset containing information related to all KC parcels that seem to have 
#' at least one bed in/on them (and studios)
#' 
#' @format A sf data frame
#' 
'pcoords'

#' Bed counts per parcel in King County
#' 
#' A dataset containing information related to all KC parcels that seem to have 
#' at least one bed in/on them (and studios). Provides information about beds in
#' apartment buildings, condos, and residential (e.g. <= 4 units) on a given parcel.
#' It also contains the GEOID for the 2020 census tract that ther parcel centriod
#' intersects.
#' 
#' @format A data.table
#' 
'beds_per_parcel'

#' Parcel level population estimates
#' 
#' Parcel level population estimates based on the following model:
#' tract_pop ~ 0 + apt_beds + condo_beds + residential_beds
#' 
#' The model is fit as a simple lm (poisson models were not downscaling as well)
#' at the tract level and downscaled via the coefficients to the parcel level.
#' 
#' The initial parcel level estimates are scaled to recover tract level estimates.
#' 
#' 
#' @format A sf (points) data frame
#' 
'parcel_pop'