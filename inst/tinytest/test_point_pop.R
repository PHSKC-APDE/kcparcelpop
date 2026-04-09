library('tinytest')
library('kcparcelpop')
library('apde.data')
library('sf')

hra = read_sf("//dphcifs/APDE-CDIP/Shapefiles/HRA/hra_2020.shp")
county = read_sf("//dphcifs/APDE-CDIP/Shapefiles/Census_2020/county/wa_counties.shp", query = "select * from wa_counties where NAME = 'KING'")
county = st_transform(county, 2926)

con = apde.data::authenticate_hhsaw()

pop = apde.data::population(years = 2020:2025,geo_type = 'hra', group_by = 'years')

# Focus on Seattle S beacon hill, georgetown, southpark
spop = pop[geo_id_code == 30]

# Get one year of point point, most recent year, all of king county
p1 = kcparcelpop::point_pop(year = 2025)

# Make sure the population aligns
tinytest::expect_equal(sum(p1$pop), pop[year == 2025, sum(pop)])

# Make sure everything falls within county boundaries
i2 = st_intersects(p1, county, sparse = FALSE)
tinytest::expect_true(all(i2[,1]))

# Pull down 3 years for georgetown and what not
p2 = kcparcelpop::point_pop(2022:2025, subset(hra, id == 30)) #2022:
bgs = st_join(p1, subset(hra, id == 30), left = FALSE)
tinytest::expect_equal(nrow(bgs), nrow(subset(p2, year == 2025)))

tinytest::expect_equal(
  spop[year %in% 2022:2025, sum(pop)],
  sum(p2$pop[p2$year %in% 2022:2025])
)
# 
# pts25 = p2 |> filter(year == 2025)
# 
# b2h = rads.data::spatial_block20_to_hra20_to_region20
# p2 |> filter(!GEOID20 %in% b2h[hra20_id == 30, GEOID20])
# 
# mp = p1 |> filter(GEOID20 %in% b2h[hra20_id==30, GEOID20]) |> mutate(inout = ptid %in% pts25$ptid)
# 
# mapview(filter(hra, id == 30)) + mapview(mp |> filter(inout == FALSE))
