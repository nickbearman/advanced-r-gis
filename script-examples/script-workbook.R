# Practical 1: Spatial Analysis
## LISA in R (optional exercise)

# based on https://geodacenter.github.io/rgeoda/articles/rgeoda_tutorial.html

# install.packages("rgeoda")
library(rgeoda)
library(sf)

#read in shapefile
manchester_lsoa <- st_read("lsoa_manchester_age_imd.shp")

# Calculate Spatial Weights
queen_w <- queen_weights(manchester_lsoa)
summary(queen_w)

# To access the details of the weights: e.g. list the neighbors of a specified observation:
  
nbrs <- get_neighbors(queen_w, idx = 1)
cat("\nNeighbors of the 1-st observation are:", nbrs)

# Calculating Local Indicators of Spatial Association–LISA
# Local Moran

manchester_IMD <- manchester_lsoa["IMDscor"]
lisa <- local_moran(queen_w, manchester_IMD)

#we can access different bits of the calcuations:

lms <- lisa_values(gda_lisa = lisa)
lms

pvals <- lisa_pvalues(lisa)
pvals

cats <- lisa_clusters(lisa, cutoff = 0.05)
cats

lbls <- lisa_labels(lisa)
lbls

#join labels on

manchester_lsoa$lisaCats <- cats
head(manchester_lsoa)

qtm(manchester_lsoa, fill = "lisaCats")

lisa_colors <- lisa_colors(lisa)
lisa_labels <- lisa_labels(lisa)

tm_shape(manchester_lsoa) + 
  tm_polygons("lisaCats", palette = lisa_colors[1:5], labels = lisa_labels[1:5])

# for more information on the mapping, see https://geodacenter.github.io/rgeoda/articles/rgeoda_tutorial.html#exploratory-spatial-data-analysis


# Practical 2: Spatial Decision Making
# sf version - older sp version available in script-workbook-sp.R

setwd("~/work/confident-spatial-analysis/data-user")

library(sf)
library(tmap)

#read in shapefile
  manchester_lsoa <- st_read("lsoa_manchester_age_imd.shp")
#plot data
  qtm(manchester_lsoa)
#head
  head(manchester_lsoa)

#plot tmap
  tm_shape(manchester_lsoa) +
    tm_polygons("IMDscor", title = "IMD Score", palette = "Blues", style = "jenks") +
    tm_layout(legend.title.size = 0.8)

#download data
  #download.file("http://www.nickbearman.me.uk/data/r/tram.zip","tram.zip")
#unzip
  #unzip("tram.zip")

#read in tramline data
  tramlines <- st_read("Metrolink_Lines_Functional.shp")
#show map
  qtm(tramlines)

#tram stations
  stations_CSV <- read.csv("TfGMMetroRailStops.csv", header = TRUE)
  head(stations_CSV)

#subset out Metrolink stations
  metrolink_stations_CSV <- stations_CSV[which(stations_CSV$NETTYP == "M"),]
  head(metrolink_stations_CSV)

#make as a sf layer
  tram_stations <- st_as_sf(metrolink_stations_CSV, coords = c('GMGRFE', 'GMGRFN'), crs = 27700)
  
#plot tram stations
  qtm(tram_stations)
  head(tram_stations)

#plot just tram stations
  tm_shape(tram_stations) +
    tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") 

#plot tram stations and tram lines, for context
  tm_shape(tram_stations) +
    tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(tramlines) +
    tm_lines(col = "black")

  #plot tram stations and LSOAs
  tm_shape(tram_stations) +
    tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") + 
  tm_shape(manchester_lsoa) +
    tm_borders()

## IMD of Tram Station Catchment Areas

#spatial join, point in polygon, tram stations in LSOAs
  stations_in_LSOA <- st_join(tram_stations, manchester_lsoa)
  
#view the data  
  head(stations_in_LSOA)



#count stations in LSOA
  library(dplyr)
  stations_in_LSOA_count <- count(as_tibble(stations_in_LSOA), NAME) %>%

#View data
  View(stations_in_LSOA_count)

#examine one feature
  stations_in_LSOA[1,]
  class(stations_in_LSOA)
  qtm(stations_in_LSOA)

## Showing Most and Least Deprived Stations

#reorder
  stations_in_LSOA <- stations_in_LSOA[order(stations_in_LSOA$IMDscor, decreasing = TRUE), ]

  head(stations_in_LSOA)

#plot stations
tm_shape(stations_in_LSOA) +
  tm_dots(stations_in_LSOA, size = 0.1, shape = 19, col = "darkred") 

#show top / bottom stations
tm_shape(stations_in_LSOA) +
  tm_dots(size = 0.1, shape = 19, col = "darkred") +
  tm_shape(stations_in_LSOA[1:10,]) +
  tm_dots(size = 0.1, shape = 19, col = "red") +
  tm_shape(stations_in_LSOA[89:99,]) +
  tm_dots(size = 0.1, shape = 19, col = "blue") 

##Tram Stop Buffers

#plot the tram stations
  qtm(tram_stations)

#calculate the buffer (distance is 1200 meters)
  tram_stations_1200_buffer <- st_buffer(tram_stations, 1200)
#plot the buffer
  qtm(tram_stations_1200_buffer)

#plot buffer and stations
  tm_shape(tram_stations_1200_buffer) +
    tm_borders(col = "red") +
  tm_shape(tram_stations) +
    tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") 

# overlay one tram stop and the LSOAs
#plot the buffer, lsoas and tram stations
  tm_shape(tram_stations_1200_buffer[4,]) +
    tm_polygons() +
  tm_shape(manchester_lsoa) +
    tm_polygons(alpha=0.1) +
  tm_shape(tram_stations[4,]) +
    tm_dots()
  
#convert lsoa polygons to points
  manchester_lsoa_points <- st_centroid(manchester_lsoa)  

  qtm(manchester_lsoa_points)

  #plot points and LSOA
  tm_shape(manchester_lsoa) +
    tm_borders(col = "red") +
  tm_shape(manchester_lsoa_points) +
    tm_dots(manchester_lsoa_points, size = 0.1, shape = 19, col = "darkred") 

#it has the IMD data in it
  View(manchester_lsoa_points)

#map of one station buffer with LSOA points in it
  tm_shape(tram_stations_1200_buffer[1,]) +
    tm_borders(col = "red") +
  tm_shape(tram_stations[1,]) +
    tm_dots(size = 0.5) +
  tm_shape(manchester_lsoa_points) +
    tm_dots(manchester_lsoa_points, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(manchester_lsoa) +
    tm_polygons(alpha=0.1) 

#st_join is a left join
#join each station to the LSOAs within the buffer
  tram_stations_IMD <- st_join(tram_stations_1200_buffer, manchester_lsoa_points)
  View(tram_stations_IMD)

#group by Station, take average IMDscore. 
  station_LSOA_IMD <- tram_stations_IMD %>% group_by(Label_Text) %>% summarise(mean(IMDscore)) 

#view the average IMD score for each station
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
  tm_shape(station_LSOA_IMD_pt_ordered[89:99,]) +
  tm_dots(station_LSOA_IMD_pt_ordered[89:99,], size = 0.1, shape = 19, col = "blue")

## Future Tram Expansion (optional exercise)

#read in tramline data
  future_tramlines <- st_read(dsn = "future_tramlines.geojson")
  
# read in data
  future_tramstops <- st_read("future_tramstops.shp")
# plot data
  tm_shape(future_tramlines) +
    tm_lines() +
  tm_shape(future_tramstops) +
    tm_dots(size = 0.1)
  