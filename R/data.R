#' Point level population estimates
#' 
#' 2020 Census blocks were intersected with the 2025 KC parcel map (subsetted to residential parcels).
#' Blocks with no or low overlap with the parcel layer were included entirely.
#' Points (roughly, one point per 20 people within the block) were randomly sampled spatially. 
#' 
#' 
#' @format A sf (points) data frame
#' 
'parcel_pop'