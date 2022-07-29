#' Estimate the % of people living at each parcel
#' pop ~ 1 + # of single family beds + # of beds from 2-, 3-, and 4- plexes + 
#' # of apartment/condo beds + median appraised value per bed at parcel + ICAR (+ iid)?--or nested block, bg, tract
#' 
library('data.table')
library('sf')
library('stringr')
library('tigris')
library('usethis')
options(tigris_use_cache = TRUE)

#KCIT parcel shapefile
pshp = st_read("//kcitfsrprpgdw01/KCLIB/Plibrary2/property/shapes/polygon/parcel_address_4co.shp")
pshp = subset(pshp, COUNTY == 'KING')
# File descriptions:
# lookup
lookup = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_LookUp.csv")

# Accessory is where the actual assessed values are kept
# Apartment complex: provides details on apartment buildings in a complex, but not unit level details
ac = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_AptComplex.csv")

# condo complex and units: building level details about condo buildings
condo = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_CondoUnit2.csv")

# parcel level data
parcel = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_Parcel.csv")

# residential bulding: one record per building. Residential units have between 1 - 3 living units
rb = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_ResBldg.csv")

# unit breakdown: per each real property, provides the number of of bedrooms and sqft
ub = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_UnitBreakdown.csv")

# apartment units
apts = merge(ac, ub, all.x = T, by = c('Major', 'Minor'))
apts[, nbeds := NbrBedrooms]
apts[nbeds == 'S', nbeds := 1]
apts[, nbeds := as.numeric(nbeds)]
apts[, nbeds := nbeds * as.numeric(NbrThisType)]
apt_sum = apts[, .(beds = sum(nbeds, na.rm = T), address = first(Address)), .(Major, Minor)]
apt_sum[, type := 'apt']
# Residential
rb_sum = rb[, .(beds = sum(Bedrooms, na.rm = T), address = first(Address)), .(Major, Minor)]
rb_sum[, type := 'res']

#condo
condo[, nbeds := NbrBedrooms]
condo[nbeds == 'S', nbeds := 1]
condo[, nbeds := as.numeric(nbeds)]
ads = c("BuildingNumber", "Fraction", "DirectionPrefix", "StreetName", 
        "StreetType", "DirectionSuffix","ZipCode")
condo[, (ads) := lapply(.SD, trimws), .SDcols = ads]
condo[, address := do.call(paste, .SD), .SDcols = ads]
condo[, address := gsub('\\s+', ' ', address)]
condo_sum = condo[, .(beds = sum(nbeds, na.rm = T), address = first(address)), .(Major)]
condo_sum[, Minor := 0]
condo_sum[, type := 'condo']

# create beds dataset
beds = rbind(apt_sum, condo_sum, rb_sum)
beds[, address := trimws(gsub('\\s+', ' ', address))]
beds[, address := first(address), .(Major, Minor)]
beds = dcast(beds, Major + Minor + address ~ type, value.var = 'beds')
setnames(beds, c('apt', 'condo', 'res'), c('apt_beds', 'condo_beds', 'res_beds'))
beds[, tot_beds := rowSums(.SD,na.rm = T), .SDcols = c('apt_beds', 'res_beds', 'condo_beds')]
beds[, c('apt_beds', 'res_beds', 'condo_beds') := lapply(.SD, function(x){
  x[is.na(x)] = 0
  x
}), .SDcols = c('apt_beds', 'res_beds', 'condo_beds')]
beds = beds[tot_beds>0]

# create PIN
beds[, PIN := paste0(str_pad(Major, width = 6,side = 'left', pad = 0),str_pad(Minor, width = 4,side = 'left', pad = 0))]

# limit pshp into parcels with beds
pshp_ads = unique(data.table(pshp)[, .(PIN, COMP_ADDR)])
pshp = subset(pshp, pshp$PIN %in% beds[, PIN])

# assign tracts to parcels (blocks with privacy disclosure are probably too wonky)
tracts = tigris::tracts(state = 'WA', county = '033', cb = FALSE, year = 2020)
tracts = tracts[, c('GEOID', 'ALAND', 'AWATER')]
tracts = st_transform(tracts, st_crs(pshp))

pshp = pshp[, c('PIN')]

# Assign parcels by centriod overlap with tracts
# to save computation time
pcoords = st_centroid(pshp)
ptrt = st_join(pcoords, tracts)

beds_per_parcel = merge(beds, ptrt[, c('PIN', 'GEOID')], all.x = T, by = 'PIN')
beds_per_parcel[, geometry:=NULL]

# Fix parcel addresses-- in a lazy way
for(id in unique(beds_per_parcel[address=='' | is.na(address), PIN])){
  
  newad = pshp_ads[PIN %in% id, COMP_ADDR]
  if(length(newad)>0){
    beds_per_parcel[PIN == id, address := newad]
  }
  
 
}


# geocode the addresses that didn't work
# this doesn't seem to get enough to make it worthwhile-- probably if the addresses were cleaned
# 
# geome = unique(beds_per_parcel[is.na(GEOID), address])
# xy = lapply(geome, kcgeocode:::kc_singleline)
# xy = rbindlist(xy, use.names = TRUE)


# Save some objects
usethis::use_data(pcoords, beds_per_parcel, overwrite = TRUE)
tools::resaveRdaFiles('data/')
