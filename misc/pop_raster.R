library('kcparcelpop')
library('terra')
library('sf')
library('stars')
library('rads')
library('ggplot2')
library('spatagg')
library('data.table')

zip = st_read("//dphcifs/APDE-CDIP/Shapefiles/ZIP/zipcode_shore_kc_clip.shp")
zpop = get_population(geo_type ='zip')
zip = merge(zip[, c('ZIP')], zpop[, .(ZIP = geo_id, pop)], all.x = T, by = 'ZIP')
zip$pop[is.na(zip$pop)] = 0
hra = st_read("//dphcifs/APDE-CDIP/Shapefiles/HRA/hra_2020_nowater.shp")
pp = kcparcelpop::parcel_pop
bb = st_bbox(hra)
r = st_as_stars(bb, dx = units::as_units(1, 'km'), dy = units::as_units(1, 'km'))
poprast = stars::st_rasterize(pp, template = r, )
pp = pp[!is.na(pp$pop),]
pp$relpop = pp$pop/sum(pp$pop) * 100
pp$relpar = 1/nrow(pp) * 100

# half mile or so
br = rast(ext(hra), res = c(5280/2, 5280/2), crs = crs(hra))

# relative parcel pop
relpopr = terra::rasterize(pp, br, field = 'relpop', fun = sum)

# relative parcel
relparr = terra::rasterize(pp, br, field = 'relpar', fun = sum)

# relative by ZIP
zras = terra::rasterize(zip, br, field = 'ZIP', fun = 'max')
for(z in unique(zras)$ZIP){
  zras[zras==z] <- subset(zip,  ZIP == z)$pop * 1/sum(values(zras)==z,na.rm = T)
}
zras = zras/sum(values(zras),na.rm = T) * 100

# compare geographic overlap with pop
z2hg = rads.data::spatial_zip_to_hra20_geog
z2hp = rads.data::spatial_zip_to_hra20_pop
z2hg = z2hg[, .(ZIP, hra20_id, hra20_name, geog = round(100*s2t_fraction))]
z2hp = z2hp[, .(ZIP, hra20_id, hra20_name, pfrac = round(100 * s2t_fraction))]
z2h = merge(z2hp, z2hg, all = T , by = c('ZIP', 'hra20_id', 'hra20_name'))
z2h[is.na(geog), geog := 0]
z2h[is.na(pfrac), pfrac := 0]
# z2h = melt(z2h, id.vars = c('ZIP', 'hra20_id', 'hra20_name'))
# z2h[, step := ifelse(variable == 'geog', 1, 2)]
z2h = z2h[geog>3 | pfrac>.3]
z2h = z2h[hra20_id %in% sample(unique(hra20_id),9)]
g = ggplot(z2h, aes(x = 0, xend = 1, y = geog, yend = pfrac, group = ZIP)) + 
  facet_wrap(~hra20_name) + geom_segment(linewidth = 1.1) + theme_bw() +
  ggtitle('Overlap; Geographic -> Population')


# graphs n things

# Flat ZIP code population
plot(zras)

# Population based on parcel density
plot(relparr)

# Parcel Population
plot(relpopr)

# Difference between geographic overlap and HRAs
g

# Be careful with going from ZIPs to other stuff -- especially at the edge of the county
# 20% of the geography might fight into KC, but because parcelpop is only KC (For now) 100% of the population will
# You can generally mix and match (within a source) the crosswalk-- although it'll take some manual work

# Be careful about water when using spatagg







