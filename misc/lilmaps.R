library('kcparcelpop')
library('terra')
library('sf')
library('stars')
library('rads')
library('ggplot2')
library('spatagg')
library('data.table')

hra = st_read("//dphcifs/APDE-CDIP/Shapefiles/HRA/hra_2020_nowater.shp")
pp = kcparcelpop::parcel_pop
bb = st_bbox(hra)
# r = st_as_stars(bb, dx = units::as_units(1, 'km'), dy = units::as_units(1, 'km'))
# poprast = stars::st_rasterize(pp, template = r)
# pp = pp[!is.na(pp$pop),]
# pp$relpop = pp$pop/sum(pp$pop) * 100
# pp$relpar = 1/nrow(pp) * 100

# half mile or so
br = rast(ext(hra), res = c(5280/4, 5280/4), crs = crs(hra))

# relative parcel pop
popr = terra::rasterize(pp, br, field = 'pop', fun = sum)

blah = ggplot() + geom_sf(data = pp, aes(color = pop)) + 
  geom_sf(data = hra, fill = NA) +
  theme_bw()
blah
# plot(pp[, 'pop'])
# plot(hra, add = T, color = 'grey', fill = NA)

# relative parcel
relparr = terra::rasterize(pp, br, field = 'relpar', fun = sum)