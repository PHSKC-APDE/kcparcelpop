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
apt_sum = apts[, .(apt_beds = sum(nbeds, na.rm = T)), .(Major, Minor)]

# Residential
rb_sum = rb[, .(res_beds = sum(Bedrooms, na.rm = T)), .(Major, Minor)]

#condo
condo[, nbeds := NbrBedrooms]
condo[nbeds == 'S', nbeds := 1]
condo[, nbeds := as.numeric(nbeds)]
condo_sum = condo[, .(condo_beds = sum(nbeds, na.rm = T)), .(Major)]
condo_sum[, Minor := 0]

beds = parcel[, .(Major, Minor)]
for(i in list(apt_sum, rb_sum, condo_sum)){
  beds = merge(beds, i, all.x = T, by = c('Major', 'Minor'))
}
beds[, tot_beds := rowSums(.SD,na.rm = T), .SDcols = c('apt_beds', 'res_beds', 'condo_beds')]
beds = beds[tot_beds>0]

# create PIN
beds[, PIN := paste0(str_pad(Major, width = 6,side = 'left', pad = 0),str_pad(Minor, width = 4,side = 'left', pad = 0))]

# limit pshp into parcels with beds
pshp = subset(pshp, pshp$PIN %in% beds[, PIN])

# assign tracts to parcels (blocks with privacy disclosure are probably too wonky)
tracts = tigris::tracts(state = 'WA', county = '033', cb = TRUE, year = 2020)
tracts = tracts[, c('GEOID', 'ALAND', 'AWATER')]
tracts = st_transform(tracts, st_crs(pshp))
ptrt = st_join(pshp, tracts, largest = TRUE)

#Make sure its unique by parcel
st_geometry(ptrt) = NULL
setDT(ptrt)
stopifnot(ptrt[, .N, PIN][, all(N == 1)])

ptrt = merge(ptrt, beds, by = 'PIN')

housing_parcels_shp = pshp
beds_per_parcel = ptrt


# Save some objects
usethis::use_data(housing_parcels_shp, beds_per_parcel, overwrite = TRUE)
tools::resaveRdaFiles('data/')
