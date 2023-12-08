# Practical 1: Spatial Analysis
# Moran’s I and LISA in R (optional exercise)

library(rgeoda)
library(sf)
library(tmap)
setwd("~/work/confident-spatial-analysis/data-user")

#read in shapefile
manchester_lsoa <- st_read("lsoa_manchester_age_imd.shp")

# Calculate Spatial Weights
queen_w <- queen_weights(manchester_lsoa)
summary(queen_w)

# To access the details of the weights: e.g. list the neighbours of a specified observation:
nbrs <- get_neighbors(queen_w, idx = 1)
cat("\nNeighbors of the 1-st observation are:", nbrs)

####


lag <- spatial_lag(queen_w, manchester_lsoa['IMDscor'])
lag

imd <- manchester_lsoa$IMDscor

lag <- as.integer(unlist(lag))

plot(imd,lag)
plot(scale(imd),scale(lag))

#Calculate Moran's I value
I <- cor(imd, lag) * sd(lag) / sd(imd)

#Add to plot:
plot(scale(imd),scale(lag), main = paste0("Moran's I: ",round(I,3)))
#add line
abline(0,I, col = "red")

  ####


# Calculating Local Indicators of Spatial Association–LISA
# Local Moran
manchester_IMD <- manchester_lsoa["IMDscor"]
lisa <- local_moran(queen_w, manchester_IMD)

#Get the values of the local Moran's I
lms <- lisa_values(gda_lisa = lisa)
lms

#get the pseudo-p values of significance of local Moran computation, the green significance map
pvals <- lisa_pvalues(lisa)
pvals

#get the cluster indicators of local Moran computation, the blue-red map values
cats <- lisa_clusters(lisa, cutoff = 0.05)
cats

#labels
lbls <- lisa_labels(lisa)
lbls

table(cats)

#join labels on to the data
manchester_lsoa$lisaCats <- cats
head(manchester_lsoa)

# access colours and labels
lisa_colors <- lisa_colors(lisa)
lisa_labels <- lisa_labels(lisa)

#draw map
tm_shape(manchester_lsoa) +
  tm_polygons("lisaCats", palette = lisa_colors[1:5], labels = lisa_labels[1:5])

# Practical 2: Spatial Decision Making

library(sf)
library(tmap)

#read in data
setwd("~/work/confident-spatial-analysis/data/tram")
manchester_lsoa <- st_read("lsoa_manchester_age_imd.shp")

#plot data
qtm(manchester_lsoa)
head(manchester_lsoa)

#check whether these are 2011 LSOAs - guessing so. Need to wait for IMD to 
#update to 2021 LSOAs

#map of imd
tm_shape(manchester_lsoa) + 
  tm_polygons("IMDscor", title = "IMD Score", palette = "Blues", style = "jenks") +
  tm_layout(legend.title.size = 0.8)

## public transport services

# read in tramlines
tramlines <- st_read("Metrolink_Lines_Functional.shp")
qtm(tramlines)
head(tramlines)

#read in CSV with tram station locations
stations_CSV <- read.csv("TfGMMetroRailStops.csv", header = TRUE)
head(stations_CSV)
View(stations_CSV)

which(stations_CSV$NETTYP == "M")
stations_CSV[92,] 

#subset out Metrolink stations
metrolink_stations_CSV <- stations_CSV[which(stations_CSV$NETTYP == "M"),]
head(metrolink_stations_CSV)

#make as a sf layer
tram_stations <- st_as_sf(metrolink_stations_CSV, coords = c('GMGRFE', 'GMGRFN'), crs = 27700)
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

## IMD of Tram Stations Catchment Areas

#spatial join
stations_in_LSOA <- st_join(tram_stations, manchester_lsoa)

#view the data
View(stations_in_LSOA)

#count stations in LSOA
library(dplyr)
stations_in_LSOA_count <- count(as_tibble(stations_in_LSOA), NAME)

View(stations_in_LSOA_count)

which(manchester_lsoa$NAME == "Manchester 054C")

#example of LSOA with one tram station in
tm_shape(manchester_lsoa[918,]) +
  tm_borders() +
  tm_shape(tram_stations) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(manchester_lsoa) +
  tm_borders() +
  tm_shape(tramlines) +
  tm_lines(col = "black")

#example of LSOA with more than one tram station in
tm_shape(manchester_lsoa[1643,]) +
  tm_borders() +
  tm_shape(tram_stations) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(manchester_lsoa) +
  tm_borders() +
  tm_shape(tramlines) +
  tm_lines(col = "black")

## Showing Most and Least Deprived Stations
stations_in_LSOA <- stations_in_LSOA[order(stations_in_LSOA$IMDscor, decreasing = TRUE), ]

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

#most deprived
stations_in_LSOA[1,]
#least deprived
stations_in_LSOA[99,]

#tram stop buffers example
#which index do we need?
which(tram_stations$RSTNAM == "St Werburgh's Road")
which(tram_stations$RSTNAM == "Chorlton")
which(tram_stations$RSTNAM == "Withington")
#map them
tm_shape(tram_stations[c(84,19,94),]) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(manchester_lsoa) + 
  tm_polygons("IMDscor", title = "IMD Score", palette = "Blues", style = "jenks") +
  tm_shape(tram_stations[84,]) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "red") +
    tm_shape(tramlines) +
  tm_lines(col = "black")

#plot the tram stations
qtm(tram_stations)
#calculate the buffer (distance is 1200 meters)
tram_stations_1200_buffer <- st_buffer(tram_stations, 1200)
#plot the buffer
qtm(tram_stations_1200_buffer)

which(tram_stations_1200_buffer$RSTNAM == "St Werburgh's Road")
#to add to our earlier example
tm_shape(tram_stations_1200_buffer[84,]) +
  tm_polygons(alpha=0) +
  tm_shape(tram_stations[c(84,19,94),]) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(manchester_lsoa) + 
  tm_polygons("IMDscor", title = "IMD Score", palette = "Blues", style = "jenks") +
  tm_shape(tram_stations_1200_buffer[84,]) +
  tm_polygons(alpha=0.3) +
  tm_shape(tramlines) +
  tm_lines(col = "black") +
  tm_shape(tram_stations) +
  tm_dots(tram_stations, size = 0.2, shape = 19, col = "black") +
  tm_shape(tram_stations[84,]) +
  tm_dots(tram_stations, size = 0.2, shape = 19, col = "red")

 # buffer experiment
tram_stations_buffer <- st_buffer(tram_stations, 600)
tm_shape(tram_stations_buffer[c(84,19,94),]) +
  tm_polygons(alpha=0) +
  tm_shape(tram_stations[c(84,19,94),]) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(manchester_lsoa) + 
  tm_polygons("IMDscor", title = "IMD Score", palette = "Blues", style = "jenks") +
  tm_shape(tram_stations_buffer) +
  tm_polygons(alpha=0.3) +
  tm_shape(tramlines) +
  tm_lines(col = "black") +
  tm_shape(tram_stations) +
  tm_dots(tram_stations, size = 0.2, shape = 19, col = "black") +
  tm_shape(tram_stations[84,]) +
  tm_dots(tram_stations, size = 0.2, shape = 19, col = "red")

#buffer
tram_stations_buffer <- st_buffer(tram_stations, 600)

#point in polygon
#convert polygons to points
manchester_lsoa_points <- st_centroid(manchester_lsoa)
#plot points and LSOA
tm_shape(manchester_lsoa) +
  tm_borders(col = "red") +
  tm_shape(manchester_lsoa_points) +
  tm_dots(manchester_lsoa_points, size = 0.1, shape = 19, col = "darkred")
head(manchester_lsoa_points)

#map of four station buffers with LSOA points in it
tm_shape(tram_stations_buffer[c(84,19,94),]) +
  tm_polygons(alpha=0) +
  tm_shape(tram_stations[c(84,19,94),]) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(manchester_lsoa) + 
  tm_polygons("IMDscor", title = "IMD Score", palette = "Blues", style = "jenks") +
  tm_shape(manchester_lsoa_points) +
  tm_dots(manchester_lsoa_points, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(tram_stations_buffer) +
  tm_polygons(alpha=0.3) +
  tm_shape(tramlines) +
  tm_lines(col = "black") +
  tm_shape(tram_stations) +
  tm_dots(tram_stations, size = 0.2, shape = 19, col = "black") +
  tm_shape(tram_stations[84,]) +
  tm_dots(tram_stations, size = 0.2, shape = 19, col = "red")

#simpler map
tm_shape(tram_stations_buffer[c(84),]) +
  tm_polygons(alpha=0) +
  tm_shape(manchester_lsoa) + 
  tm_polygons() +
  tm_shape(tram_stations[c(84),]) +
  tm_dots(tram_stations, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(manchester_lsoa_points) +
  tm_dots(manchester_lsoa_points, size = 0.1, shape = 19, col = "darkgreen") +
  tm_shape(tram_stations_buffer[c(84),]) +
  tm_polygons(alpha=0)

#st_join is a left join
#join each station to the LSOAs within the buffer
tram_stations_IMD <- st_join(tram_stations_buffer, manchester_lsoa_points)
View(tram_stations_IMD)
     
#group by Station, take average IMDscore.
station_LSOA_IMD <- tram_stations_IMD %>% group_by(RSTNAM) %>% summarise(mean(IMDscor))
#view the average IMD score for each station
View(station_LSOA_IMD)
qtm(station_LSOA_IMD)

#still buffers (of each station), so convert to points (centroids)
station_LSOA_IMD_pt <- st_centroid(station_LSOA_IMD)
qtm(station_LSOA_IMD_pt)

#map with IMD score
tm_shape(station_LSOA_IMD) +
  tm_polygons("mean(IMDscor)", title = "IMD Score", palette = "Blues", style = "jenks") +
  tm_shape(station_LSOA_IMD_pt) +
  tm_dots(station_LSOA_IMD_pt, size = 0.1, shape = 19, col = "darkred")

#reorder, most deprived at the top
station_LSOA_IMD_pt_ordered <- station_LSOA_IMD_pt[order(station_LSOA_IMD_pt$`mean(IMDscor)`, 
                                                         decreasing = TRUE), ]

head(station_LSOA_IMD_pt_ordered)

#plot map of average IMD score by station (top 10 in Red, bottom 10 in Blue)
tm_shape(station_LSOA_IMD) +
  tm_polygons("mean(IMDscor)", title = "IMD Score", palette = "Blues", style = "jenks") +
  tm_shape(station_LSOA_IMD_pt_ordered) +
  tm_dots(station_LSOA_IMD_pt_ordered, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(station_LSOA_IMD_pt_ordered[1:10,]) +
  tm_dots(station_LSOA_IMD_pt_ordered[1:10,], size = 0.1, shape = 19, col = "red") +
  tm_shape(station_LSOA_IMD_pt_ordered[89:99,]) +
  tm_dots(station_LSOA_IMD_pt_ordered[89:99,], size = 0.1, shape = 19, col = "blue") 

## Polygon Polygon Overlay (optional exercise)
# see st_intersection_code.R
