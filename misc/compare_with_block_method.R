library('data.table')
library('sf')
library('apde.data')
library('kcparcelpop')
library('dplyr')

ppop = kcparcelpop::parcel_pop
bpop = apde.data::population(years = 2022, geo_type = 'blk')
bshp = read_sf("//dphcifs/APDE-CDIP/Shapefiles/Census_2020/block/kc_block.shp")

blks = merge(bshp, bpop[pop>0, .(GEOID20 = geo_id, pop)], by = 'GEOID20')

# Make a point for 10 people in the block
blks$npt = floor(blks$pop/10)
blks = blks |> mutate(npt = case_when(npt == 0 ~ 1, TRUE ~ npt))
blks$ppp = blks$pop/blks$npt

blkpts = st_sample(blks, blks$npt)
