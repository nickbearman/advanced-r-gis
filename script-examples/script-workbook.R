# Practical 3: Spatial Decision Making
# sf version

setwd("~/work/confident-spatial-analysis/data-user")

download.file("http://www.nickbearman.me.uk/data/r/GreaterManchester_lsoa_2011.zip",
              "GreaterManchester_lsoa_2011.zip")
#unzip
unzip("GreaterManchester_lsoa_2011.zip")

library(dplyr)
library(sf)
library(tmap)
manchester_lsoa <- st_read("GreaterManchester_lsoa_2011.shp")

qtm(manchester_lsoa)

imd <- read.csv("imd.csv", header = TRUE)
#delete columns we don't need
imd <- imd[,1:7]
#rename columns
colnames(imd) <- c("LSOAcode","LSOAname","LADcode","LADname","IMDscore","IMDrank","IMDdecile")

head(imd)

manchester_lsoa <- merge(manchester_lsoa, imd, by.x = "CODE", by.y = "LSOAcode")

head(manchester_lsoa)

tm_shape(manchester_lsoa) +
  tm_polygons("IMDscore", title = "IMD Score", palette = "Blues", style = "jenks") +
  tm_layout(legend.title.size = 0.8)

#download data
download.file("http://www.nickbearman.me.uk/data/r/tram.zip","tram.zip")
#unzip
unzip("tram.zip")
#read in tramline data
tramlines <- st_read(dsn = "tramlines.geojson")

qtm(tramlines)

#tram stations

tram_stations_CSV <- read.csv("metrolink-stations.csv", header = TRUE)
head(tram_stations_CSV)

#make as a sf layer
tram_stations <- st_as_sf(tram_stations_CSV, coords = c('X', 'Y'), crs = 27700)

qtm(tram_stations)
head(tram_stations)

tm_shape(tram_stations) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") 

tm_shape(tram_stations) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") + 
  tm_shape(manchester_lsoa) +
  tm_borders()

qtm(tram_stations)

#point in polygon, tram stations in LSOAs
stations_in_LSOA <- st_join(tram_stations, manchester_lsoa)

stations_in_LSOA %>% View()

#count stations in LSOA
stations_in_LSOA_count <- count(as_tibble(stations_in_LSOA), LSOAname) %>%
  print()


#examine one feature

stations_in_LSOA[1,]
class(stations_in_LSOA)
qtm(stations_in_LSOA)

#reorder
stations_in_LSOA <- stations_in_LSOA[order(stations_in_LSOA$IMDscore, decreasing = TRUE), ]

View(stations_in_LSOA)

#stations
tm_shape(stations_in_LSOA) +
  tm_dots(stations_in_LSOA, size = 0.1, shape = 19, col = "darkred") 

#show top / bottom stations
tm_shape(stations_in_LSOA) +
  tm_dots(size = 0.1, shape = 19, col = "darkred") +
  tm_shape(stations_in_LSOA[1:10,]) +
  tm_dots(size = 0.1, shape = 19, col = "red") +
  tm_shape(stations_in_LSOA[82:92,]) +
  tm_dots(size = 0.1, shape = 19, col = "blue") 

# to page 13 ( 1 to 1 join)

##Tram Stop Buffers
#taking average IMD score for catchment area
# point to polygon - buffer etc. 

qtm(tram_stations)

tram_stations_1200_buffer <- st_buffer(tram_stations, 1200)

qtm(tram_stations_1200_buffer)

tm_shape(tram_stations_1200_buffer) +
  tm_borders(col = "red") +
  tm_shape(tram_stations) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") 

tm_shape(tram_stations_1200_buffer) +
  tm_borders(col = "red") +
  tm_shape(manchester_lsoa) +
  tm_borders(col = "blue") 

manchester_lsoa_points <- st_centroid(manchester_lsoa)  

qtm(manchester_lsoa_points)

tm_shape(manchester_lsoa) +
  tm_borders(col = "red") +
  tm_shape(manchester_lsoa_points) +
  tm_dots(manchester_lsoa_points, size = 0.1, shape = 19, col = "darkred") 

#it has the IMD data in it
View(manchester_lsoa_points)

#map of one station buffer with LSOA points in it
tm_shape(tram_stations_1200_buffer[1,]) +
  tm_borders(col = "red") +
  tm_shape(manchester_lsoa_points) +
  tm_dots(manchester_lsoa_points, size = 0.1, shape = 19, col = "darkred") 


#st_join is a left join
#join each station to the LSOAs within the buffer
tram_stations_IMD <- st_join(tram_stations_1200_buffer, manchester_lsoa_points)
View(tram_stations_IMD)
#group by Station, take average IMDscore. 
station_LSOA_IMD <- tram_stations_IMD %>% group_by(Label_Text) %>% summarise(mean(IMDscore)) 

View(station_LSOA_IMD)
qtm(station_LSOA_IMD)

#still buffers (of each station), so convert to points (centriods)

station_LSOA_IMD_pt <- st_centroid(station_LSOA_IMD)
qtm(station_LSOA_IMD_pt)

#map
tm_shape(tram_stations_1200_buffer) +
  tm_borders(col = "red") +
  tm_shape(station_LSOA_IMD_pt) +
  tm_dots(station_LSOA_IMD_pt, size = 0.1, shape = 19, col = "darkred")

#reorder, most deprived at the top
station_LSOA_IMD_pt_ordered <- station_LSOA_IMD_pt[order(station_LSOA_IMD_pt$`mean(IMDscore)`, decreasing = TRUE), ]

head(station_LSOA_IMD_pt_ordered)

#plot map of average IMD score by station (top 10 in Red, bottom 10 in Blue)
tm_shape(station_LSOA_IMD_pt_ordered) +
  tm_dots(station_LSOA_IMD_pt_ordered, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(station_LSOA_IMD_pt_ordered[1:10,]) +
  tm_dots(station_LSOA_IMD_pt_ordered[1:10,], size = 0.1, shape = 19, col = "red") +
  tm_shape(station_LSOA_IMD_pt_ordered[81:91,]) +
  tm_dots(station_LSOA_IMD_pt_ordered[81:91,], size = 0.1, shape = 19, col = "blue") 
