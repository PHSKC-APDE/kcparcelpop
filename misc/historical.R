library('data.table')

pfol = '//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/'

lookup = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_LookUp.csv")

## Apartment complex ----
## provides details on apartment buildings in a complex, but not unit level details
## https://aqua.kingcounty.gov/extranet/assessor/Apartment%20Complex.zip
ac = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_AptComplex.csv")

## Condo ----
## condo complex and units: building level details about condo buildings
## https://aqua.kingcounty.gov/extranet/assessor/Condo%20Complex%20and%20Units.zip
condo = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_CondoUnit2.csv")

## parcel level data ----
## https://aqua.kingcounty.gov/extranet/assessor/Parcel.zip
parcel = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_Parcel.csv")

## residential --
## residential bulding: one record per building. Residential units have between 1 - 3 living units
## https://aqua.kingcounty.gov/extranet/assessor/Residential%20Building.zip
rb = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_ResBldg.csv")

## unit ----
## unit breakdown: per each real property, provides the number of of bedrooms and sqft
## https://aqua.kingcounty.gov/extranet/assessor/Unit%20Breakdown.zip
ub = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_UnitBreakdown.csv")

## commercial ----
## https://aqua.kingcounty.gov/extranet/assessor/Commercial%20Building.zip
comm = fread("C:/Users/dcasey/local_downloads/parcel_data/EXTR_CommBldg.csv")


# check existance
chk_ext = function(fol){
  data.table(file = c('lookup', 'ac','condo', 'parcel', 'rb', 'ub', 'comm'),
             name = c('lookup_extr', 'aptcomplex_extr', 'condounit_extr', 'parcel_area.shp',
                      'resbldg_extr', ''))
}

ac = readRDS("//dphcifs/APDE-CDIP/Shapefiles/historical_parcel/2022/aptcomplex_extr.rds")
setDT(ac)
