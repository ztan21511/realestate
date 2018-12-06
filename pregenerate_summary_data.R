library(dplyr)
library(shiny)
library(data.table)
library(tidyr)
library(ggplot2)
library(sf)
library(leaflet)
library(rgeos)

#load crime beat geo data from multiple spans of time
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

#combine all spans into one data.frame
spd_crime_beats_pre2008_Present <- mapedit:::combine_list_of_sf(list(spd_crime_beats_pre_2008, spd_crime_beats_2008_2015, spd_crime_beats_2015_2017, spd_crime_beats_2018_Present)) %>% 
  select(PRECINCT, SECTOR, BEAT, begin_year, end_year, geometry)

#test plotting beats (they all appear to be the same boundaries over time)
#ggplot(spd_crime_beats_pre_2008)+
#  geom_sf(aes(fill=beat_end_year), alpha=0.25)

#load micro community policing plans neighborhoods to use in calculating which
#parcels are in which neighborhoods (allows joining parcels to SPD crime data)
spd_mcpp_neighborhoods <- st_read(dsn='data/Seattle Police Micro-Community Policing Plans Neighborhoods/geo_export_29a19543-7dda-45bf-99f2-2be3980bb1e9.shp', stringsAsFactors = FALSE)

#load crime data and summarize data by crime subcategory counts by MCPP neighborhood
spd_crime <- fread('data/Crime_data.csv') %>% 
  group_by(Neighborhood, `Crime Subcategory`, Yr = year(as.Date(`Reported Date`, "%m/%d/%Y"))) %>% 
  summarize(N=n())

#avoiding repeating data per row to conserve memory/disk storage space, but
#using 
#%>% 
#  left_join(spd_crime_beats_pre2008_Present, by=c("Beat"="BEAT")) 
#%>% 
#  left_join(spd_mcpp_neighborhoods, by=c("Neighborhood"="name"))

#KC GIS portal data, this parcel data as CSV is more difficult to work with in sf
# and was superceded by the use of the following Addresses_in_King_County__address_point
# shapefile
#seattleParcelAddress <- fread("data/Parcels_for_King_County_with_Address_with_Property_Information__parcel_address_area.csv", stringsAsFactors = FALSE) %>% 
#  filter( CTYNAME == "SEATTLE", !is.na(LON), !is.na(LAT)) %>% 
#  mutate(LAT = as.double(LAT), LON= as.double(LON))

#cull out KC data outside seattle
#seattleAddress <- st_read("data/Addresses_in_King_County__address_point/Addresses_in_King_County__address_point.shp", stringsAsFactors=FALSE) %>% 
#  filter( CTYNAME == "SEATTLE", !is.na(LON), !is.na(LAT))

#store the reduced data set
#st_write(seattleAddress, "seattleAddresses_in_King_County__address_point.shp", driver="ESRI Shapefile")

#load the previously culled parcel/address shapefile
seattleAddress <- st_read("data/seattleAddresses_in_King_County__address_point/seattleAddresses_in_King_County__address_point.shp", stringsAsFactors=FALSE) %>% 
#filter for single family residential properties, rather than skewing or requiring
#more work to ajust for rental prices as multi-family properties where the building
#is very expensive, but each individual unit is some small fraction of that (and could fluctuate by market demand)
  filter(SITETYPE == "R1")

#MUST have LON and LAT to create sf
#coordinates(seattleParcelAddress, CRS=3426) <- c("LON", "LAT") 

#when loading this dataset, this CRS 4326 is specified, but joins fail without
#setting it explicitly here
spd_mcpp_neighborhoods <- st_transform(spd_mcpp_neighborhoods, 4326)

#this was an attempt to add a sfc column to the previous CSV data, it failed
#and examples did not lead to a working solution, hence this file was replaced
#with the Address dataset
#seattleParcelAddress$coords <- seattleParcelAddress %>% st_as_sf(coords=c("LON", "LAT")) %>% 
#  st_set_crs(4326)
#seattleParcelAddress$nbh <- st_join(seattleParcelAddress$coords, spd_mcpp_neighborhoods["geometry"])

#use spatial intersection instead of joining and opening anything with multipolygon or other ISO standard complex geospatial coordinate descriptions
seattleAddressMCPP <- st_join(seattleAddress, spd_mcpp_neighborhoods)

#free up memory (the original dataset is 2G, this filtered dataset is still 600M)
rm(seattleAddress)
#rm(spd_mcpp_neighborhoods)

#create new Parcel column for unique id from major and minor components
#this is used to join this dataset to KCA sales price history
seattleAddressMCPP <- seattleAddressMCPP %>% 
  mutate(Parcel=paste(MAJOR, MINOR, sep=""))

#seattleAddressMCPP

#OFM
#seattleDemographics <- fread("data/sade_all_2000_to_2010/sade_all_2000_to_2010.csv", stringsAsFactors = FALSE) %>%
#  filter( ) %>% 
#  mutate( begin_year=2000, end_year=2010) %>% 
#  merge( fread("data/sade_all_2010_to_2017/sade_all_2010_to_2017.csv", stringsAsFactors = FALSE) %>%
#    filter() %>% 
#    mutate(begin_year=2010, end_year=2017))

#this section was inteded to load demographic data related information to use
#as join (df or spatial) keys
#BAF (demographics by school districts)

#selected just using salesHist when facing time and memory limits 
#KCA (assessors office)
#apprValue <- fread("data/cd/EXTR_ValueHistory_V.csv")
saleHist <- fread("data/cd/EXTR_RPSale.csv", stringsAsFactors = FALSE) %>% 
  mutate(Parcel=paste(Major, Minor, sep = ""))

#attempted to use this data to allow the user to examine the types and classes
#of property as they relate to avg sales price of those given types and classes
#propertyClass <- read.csv("data/PROPERTYCLASS.csv")
#propertyType <- read.csv("data/PROPERTYTYPE.csv")

#summary data for avg sale prices by neighborhood, this data was more complex
#but simplified for time, storage, memory and load time (feature creep too late in sprint)
neighborhoodAvgSale <- saleHist %>% 
  left_join(seattleAddressMCPP, by=c("Parcel"="Parcel")) %>% 
  #left_join(propertyType, by=c("PropertyType"="ID")) %>% 
  #left_join(propertyClass, by=c("PropertyClass"="ID")) %>% 
  mutate(Yr = year(as.Date(DocumentDate, "%m/%d/%Y"))) %>% 
  filter(SalePrice != 0) %>% 
  #select( -PropertyClass, -PropertyType) %>% 
  #group_by(name, Yr, HumanReadablePropertyType, HumanReadablePropertyClass) %>% 
  group_by(name, Yr) %>% 
  summarize(AvgSlPr = mean(SalePrice))

#considered precalculating aggregate summaries to improve app runtime performance,
#but storage (github) limits of 50M max per file made this unrealistic
#allNeighborhoodAvgSale <- saleHist %>% 
#  left_join(seattleAddressMCPP, by=c("Parcel"="Parcel")) %>% 
#  mutate(HumanReadablePropertyType= "All") %>% 
#  mutate(HumanReadablePropertyClass= "All") %>% 
#  mutate(Yr = year(as.Date(DocumentDate, "%m/%d/%Y"))) %>% 
#  filter(SalePrice != 0) %>% 
#  select( -PropertyClass, -PropertyType) %>% 
#  group_by(name, Yr) %>% 
#  summarize(AvgSlPr = mean(SalePrice))

#this would have essentially acted as rbind(), like spd_beats earlier in file
#neighborhoodAvgSale <- mapedit:::combine_list_of_sf(list(neighborhoodAvgSale, allNeighborhoodAvgSale))

#free up memory
rm(saleHist)

#was writing a shapefile, with geometry for neighborhoods, but this also made the
#file larger with redundant info, made the tradeoff to load spd_mcpp shapefile
#in the shiny app, join the avg sales price for a given year to it, and use
#the avg sales price to color code neighborhoods
#st_write(neighborhoodAvgSale, delete_dsn=TRUE, "data/neighborhoodAvgSale", driver="ESRI Shapefile")
write.csv(neighborhoodAvgSale, file="data/neighborhoodAvgSale.csv")