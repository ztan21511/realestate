library(dplyr)
library(shiny)
library(data.table)
library(tidyr)
library(ggplot2)
library(sf)
library(leaflet)
library(rgeos)

spd_crime_beats_pre_2008 <- st_read(dsn="data/spdbeat_WGS84_pre2008/spdbeat_WGS84.shp", stringsAsFactors = FALSE) %>% 
  mutate(begin_year=NA, end_year=2008, beat_end_year=paste(BEAT, 2008, sep = "-"))
spd_crime_beats_2008_2015 <- st_read(dsn="data/SPD_BEATS_WGS84_2008-2015/SPD_BEATS_WGS84.shp", stringsAsFactors = FALSE) %>% 
  mutate(begin_year=2008, end_year=2015, beat_end_year=paste(BEAT, 2015, sep = "-"))
spd_crime_beats_2015_2017 <- st_read(dsn="data/SPD_BEATS_WGS84_2015-2017/SPD_BEATS_WGS84.shp", stringsAsFactors = FALSE) %>% 
  mutate(begin_year=2015, end_year=2017, beat_end_year=paste(beat, 2017, sep = "-")) %>% 
  rename(BEAT=beat)
spd_crime_beats_2018_Present <- st_read(dsn="data/SPD_BEATS_WGS84_2018+/SPD_BEATS_WGS84.shp", stringsAsFactors = FALSE) %>% 
  mutate(begin_year=2018, end_year=NA, beat_end_year=paste(beat, 2018, sep = "-")) %>% 
  rename(BEAT=beat, SECTOR=sector)

#spd_crime_beats_pre2008_Present <- 
#  merge(merge(merge(spd_crime_beats_pre_2008, spd_crime_beats_2008_2015),
#        spd_crime_beats_2015_2017),
#        spd_crime_beats_2018_Present)

#do.call(what= sf::rbind, args = list(spd_crime_beats_pre_2008, spd_crime_beats_2008_2015, spd_crime_beats_2015_2017, spd_crime_beats_2018_Present))
spd_crime_beats_pre2008_Present <- mapedit:::combine_list_of_sf(list(spd_crime_beats_pre_2008, spd_crime_beats_2008_2015, spd_crime_beats_2015_2017, spd_crime_beats_2018_Present)) %>% 
  select(PRECINCT, SECTOR, BEAT, begin_year, end_year, geometry)

#ggplot(spd_crime_beats_pre_2008)+
#  geom_sf(aes(fill=beat_end_year), alpha=0.25)

spd_mcpp_neighborhoods <- st_read(dsn='data/Seattle Police Micro-Community Policing Plans Neighborhoods/geo_export_29a19543-7dda-45bf-99f2-2be3980bb1e9.shp', stringsAsFactors = FALSE)

#use spatial intersection instead of joining and opening anything with multipolygon or other ISO standard complex geospatial coordinate descriptions
spd_crime <- fread('data/Crime_data.csv') %>% 
  group_by(Neighborhood, `Crime Subcategory`, Yr = year(as.Date(`Reported Date`, "%m/%d/%Y"))) %>% 
  summarize(N=n())


#%>% 
#  left_join(spd_crime_beats_pre2008_Present, by=c("Beat"="BEAT")) 
#%>% 
#  left_join(spd_mcpp_neighborhoods, by=c("Neighborhood"="name"))

#KC GIS portal data
#seattleParcelAddress <- fread("data/Parcels_for_King_County_with_Address_with_Property_Information__parcel_address_area.csv", stringsAsFactors = FALSE) %>% 
#  filter( CTYNAME == "SEATTLE", !is.na(LON), !is.na(LAT)) %>% 
#  mutate(LAT = as.double(LAT), LON= as.double(LON))


#seattleAddress <- st_read("data/Addresses_in_King_County__address_point/Addresses_in_King_County__address_point.shp", stringsAsFactors=FALSE) %>% 
#  filter( CTYNAME == "SEATTLE", !is.na(LON), !is.na(LAT))

#st_write(seattleAddress, "seattleAddresses_in_King_County__address_point.shp", driver="ESRI Shapefile")

seattleAddress <- st_read("data/seattleAddresses_in_King_County__address_point/seattleAddresses_in_King_County__address_point.shp", stringsAsFactors=FALSE) %>% 
#filter for single family residential properties, rather than skewing or requiring
#more work to ajust for rental prices as multi-family properties where the building
#is very expensive, but each individual unit is some small fraction of that (and could fluctuate by market demand)
  filter(SITETYPE == "R1")


#MUST have LON and LAT to create sf
#coordinates(seattleParcelAddress, CRS=3426) <- c("LON", "LAT") 

spd_mcpp_neighborhoods <- st_transform(spd_mcpp_neighborhoods, 4326)

#seattleParcelAddress$coords <- seattleParcelAddress %>% st_as_sf(coords=c("LON", "LAT")) %>% 
#  st_set_crs(4326)

#seattleParcelAddress$nbh <- st_join(seattleParcelAddress$coords, spd_mcpp_neighborhoods["geometry"])
seattleAddressMCPP <- st_join(seattleAddress, spd_mcpp_neighborhoods)

rm(seattleAddress)
#rm(spd_mcpp_neighborhoods)

seattleAddressMCPP <- seattleAddressMCPP %>% 
  mutate(Parcel=paste(MAJOR, MINOR, sep=""))

#seattleAddressMCPP

#BAF (demographics by school districts)


#KCA (assessors office)
#apprValue <- fread("data/cd/EXTR_ValueHistory_V.csv")
saleHist <- fread("data/cd/EXTR_RPSale.csv", stringsAsFactors = FALSE) %>% 
  mutate(Parcel=paste(Major, Minor, sep = ""))

neighborhoodAvgSale <- saleHist %>% 
  left_join(seattleAddressMCPP, by=c("Parcel"="Parcel")) %>% 
  mutate(Yr = year(as.Date(DocumentDate, "%m/%d/%Y"))) %>% 
  filter(SalePrice != 0) %>% 
  group_by(name, Yr, PropertyType, PropertyClass) %>% 
  summarize(AvgSalePrice = mean(SalePrice)) %>% 
  left_join(spd_mcpp_neighborhoods, by=c("name"="name"))

rm(saleHist)

st_write(neighborhoodAvgSale, "data/neighborhoodAvgSale", driver="ESRI Shapefile")
