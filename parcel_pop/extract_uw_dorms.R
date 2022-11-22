library('data.table')
library('xml2')
library('rvest')
library('sf')
library('dplyr')

# useful site: https://hfs.uw.edu/About/Facts-Figures

uwu = read_html('https://hfs.uw.edu/live-on-campus/Undergraduate-Housing/') %>%
  html_elements('a') %>% html_attr('href')

#find the living on campus
uloc = grep('live-on-campus/Undergraduate-Housing/', uwu, fixed = T, value = T)
uloc = paste0('https://hfs.uw.edu', uloc)

uwg = read_html('https://hfs.uw.edu/live-on-campus/graduate-student-apartments') %>% 
  html_elements('a') %>% html_attr('href')
gloc = grep('Graduate-Housing/', uwg, fixed = T, value = T)
gloc = paste0('https://hfs.uw.edu/live-on-campus/', gloc)


extab = function(url){
  print(url)
  name = basename(url)
  
  tab = read_html(URLencode(url)) %>% html_elements('.mb-5') %>% html_table()
  
  tab = tab[[1]]
  tab$name = name
  tab$url = url
  
  return(tab)
}

loc = c(gloc, uloc)
r = lapply(loc, extab)
r = rbindlist(r)
# for determining beds per building write.csv(unique(r[, `ROOM TYPE`]), row.names = FALSE, 'C:/Users/dcasey/local_downloads/parcel_data/uwbeds.csv')

beds_per_room = unique(r[, .(`ROOM TYPE`, `#UNITS`, name)])

# From: https://hfs.uw.edu/About/Facts-Figures
# UW provides # of beds per res hall and units per apartments
# parse the hall counts
hall = c('Alder Hall 644, Cedar Apartments 344, Elm Hall 584, Haggett Hall 813, Hansee Hall 332, Lander Hall 678, Madrona Hall 541, Maple Hall 919, McCarty Hall 971, McMahon Hall 1,044, Mercer Court A–C 633, Mercer Court D–E 283, Nordheim Court 458, Oak Hall 382, Poplar Hall 303, Stevens Court 521, Terry Hall 352, Willow Hall 563')
hall = strsplit(hall, ', ',)
hall = data.table(hall[[1]])
lastspace = gregexpr(' ', hall[,V1])
lastspace = vapply(lastspace, function(x) x[[which.max(x)]], 1)
hall[, nbeds := substr(V1, lastspace, nchar(V1))]
hall[, nbeds := as.integer(gsub(',', '', nbeds, fixed = T))]
hall[, name := trimws(substr(V1, 1, lastspace))]

# apartment housing
apt = c("Blakeley-Village-FH", "Commodore-Duchess", 
  "Commodore-Duchess-FH", "Laurel-Village-FH", "Radford-Court", "Radford-Court-FH", "Stevens-Court-FH")

apt = unique(r[name %in% apt, .(`ROOM TYPE`, name, `#UNITS`)])
apt[, name := gsub('-FH', '', name)]
apt = unique(apt)

bedsptype = fread('C:/Users/dcasey/local_downloads/parcel_data/uwbeds.csv')
apt = merge(apt, bedsptype, all.x = T, by = 'ROOM TYPE')
apt[name %in% c('Commodore-Duchess','Blakeley-Village','Laurel-Village',"Radford-Court","Stevens-Court"), name := gsub('-', ' ', name, fixed = T)]
apt = apt[apt[, max(.I), by = .(name, `#UNITS`, nbeds)]$V1, ]

apt = apt[, .(nbeds = sum(`#UNITS` * nbeds)), .(name)]

# create a df of residence halls
res = rbind(hall[, .(name, nbeds)], apt)
res[name == 'Mercer Court A–C', name := 'Mercer Court A, B, C']
res[name == 'Mercer Court D–E', name := 'Mercer Court D, E']
res = res[, .(nbeds = sum(nbeds)), name]

# standardize the names and geoms of the uw buildings
bnames = fread('C:/Users/dcasey/local_downloads/parcel_data/uwhouse.csv')
uwb = st_read("C:/Users/dcasey/local_downloads/parcel_data/uwHousingBuildings/HousingBuildings.shp")
uwb = merge(uwb, bnames[, .(FacNum, name = ComplexName)], all.x = TRUE, by = 'FacNum')
uwb = uwb %>% group_by(name) %>% summarize() %>% 
  st_centroid %>% st_transform(crs = 4326)

# Add the X/Y for the remaining complexes
addme = st_as_sf(data.frame(
  name = c('Oak Hall', 'Commodore Duchess', 'Radford Court'),
  Y = c(47.65972694532848, 47.65628715284317, 47.675168239702465),
  X = c(-122.30546206051724, -122.31189936191605, -122.259605)
), coords = c('X','Y'), crs = 4326)

uwb = rbind(uwb, addme)

# Add in bed counts
uwb = merge(uwb, res, all.y = T, by = 'name')

usethis::use_data(uwb,overwrite = TRUE, version = 3)
