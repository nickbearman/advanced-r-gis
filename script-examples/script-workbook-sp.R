# Practical 3: Spatial Decision Making

library(rgdal)
library(rgeos) 
library(sp)

#download data
  download.file("http://www.nickbearman.me.uk/data/r/tram.zip","tram.zip")
#unzip
  unzip("tram.zip")
#read in tramline data
  tramlines <- readOGR(dsn = "tramlines.geojson")
#if this doesn't work, try:
# tramlines <- readOGR(dsn = "tramlines.geojson", layer="OGRGeoJSON")

  #read in CSV with tram station locations
  tram_stations_CSV <- read.csv("metrolink-stations.csv", header = TRUE)

head(tram_stations_CSV)

#extract coordinates
  coords <- cbind(Easting = tram_stations_CSV$X, Northing = tram_stations_CSV$Y)
#create spatialPointsDataFrame, removing coordinates from the @data element
#we can use the same projection information as manchester_lsoa as we know it is identical
  tram_stations <- SpatialPointsDataFrame(coords, tram_stations_CSV[, -(1:2)], 
                                    proj4string = manchester_lsoa@proj4string)

  #read in data
  manchester_lsoa <- readOGR(".", "GreaterManchester_lsoa_2011_imd")
  
  
  #plot LSOAs
plot(manchester_lsoa)
#plot tram lines, in blue
plot(tramlines, col = "blue", lwd = 5, add = TRUE)
#plot stations, in red
plot(tram_stations, pch = ".", col = "red", cex = 5, add = TRUE)

# This is another R package, allowing GIS overlay operations
  library(rgeos) 
# this extracts the tram stations that are over the LSOAs (all of them in our case)
  z <- tram_stations[!is.na(over(tram_stations, geometry(manchester_lsoa))),]
# this takes the attribute data from the LSOA that contain the tram_stations, 
  #and adds it to the data frame of z     
    z@data <- data.frame(z@data, na.omit(over(tram_stations, manchester_lsoa)))
#this copies the project from tram_stations to z    
    z@proj4string <- tram_stations@proj4string
#copy z to tram_stations_joined
    tram_stations_joined <- z
#preview data
    head(tram_stations_joined)
    head(tram_stations_joined@data)

#Z is just a temporary variable to hold the updated tram_stations information. If we wanted to be tidy, we should really delete the z variable when we have finished: `rm(z)` 

## Reordering data


    tram_stations_joined_reordered <- 
      tram_stations_joined[order(tram_stations_joined@data$IMDscore, decreasing = TRUE),]
#plot top 10 and bottom 10
plot(tram_stations_joined_reordered[1:10,], col = "red")
plot(tram_stations_joined_reordered[81:91,], col = "blue", add = TRUE)


#most deprived
tram_stations_joined_reordered[1,]
#least deprived
tram_stations_joined_reordered[91,]

#map with the 10 most deprived stations in red and the top 10 least deprived stations in blue as well as identifying the most and least deprived stations? As an optional extra, see if you can include an appropriate legend and title.


plot(tramlines)
plot(tram_stations, pch=".", cex = 3, add = TRUE)
plot(tram_stations_joined_reordered[1:10,], pch=".", cex = 5, col = "red", add = TRUE)
plot(tram_stations_joined_reordered[81:91,], pch=".", cex = 5, col = "blue", add = TRUE)
plot(tram_stations_joined_reordered[1,], pch=".", cex = 9, col = "red", add = TRUE)
plot(tram_stations_joined_reordered[91,], pch=".", cex = 9, col = "blue", add = TRUE)

## Creating a Function (optional exercise)

#tram_stations_joined <- SpatialJoin(tram_stations, manchester_lsoa)

#function to join points with the attribute data of the polygons they overlay
SpatialJoin <- function(pts, polys) {
  #error checking
    if (!inherits(polys, "SpatialPolygonsDataFrame")) 
        stop("MUST BE SP SpatialPolygonsDataFrame OBJECT")
    if ((inherits(pts, "SpatialPointsDataFrame") | inherits(pts, "SpatialPoints")) == FALSE) 
        stop("Must be sp SpatialPointsDataFrame object")
  #extract points in overlay
    z <- pts[!is.na(over(pts, geometry(polys))),]
  #join attribute data  
    z@data <- data.frame(z@data, na.omit(over(pts,polys)) )
  #update projection
    z@proj4string <- pts@proj4string
  #return z
    z
}

#use this line to run:
  tram_stations_joined <- SpatialJoin(tram_stations, manchester_lsoa)
  head(tram_stations_joined@data)

## Tram Stop Buffers


#plot the tram stations
  plot(tram_stations_joined)
#calculate the buffer (distance is 1200 meters)
  tram_stations_joined_buffer <- gBuffer(tram_stations_joined, width = 1200, byid = TRUE)
#plot the buffer (add to existing plot)
  plot(tram_stations_joined_buffer, add = TRUE)

#convert polygons to points  
  manchester_lsoa_points <- gCentroid(manchester_lsoa, byid = TRUE)  
  plot(manchester_lsoa_points)
#create coordinates
  coords = data.frame(manchester_lsoa_points@coords[,1:2])
#create SpatialPointsDataFrame using coords and data
  manchester_lsoa_points <- SpatialPointsDataFrame(coords, data=data.frame(
    manchester_lsoa@data$IMDscore), proj4string= manchester_lsoa@proj4string)
#plot points  
  plot(manchester_lsoa_points)
#show head
  head(manchester_lsoa_points@data)
#plot first tram station
  plot(tram_stations_joined_buffer[1,])  
#plot lsoa points on top
  plot(manchester_lsoa_points, add = TRUE)  

#Create point in polygon list
  o <- over(tram_stations_joined_buffer,manchester_lsoa_points,returnList=TRUE)
#View length of the list
  length(o)
#If we examine the object o, we will see also see that this comprises a list of dataframes. 
#The summary function tells you about an object - head, is used to wrap around the function 
#so only the first six elements are shown
  head(summary(o))
#View an item from the list (in this case sixth) (i.e. the sixth tram station and all of 
#the LSOAs that are linked with it)
  o[[6]]
#get a list of those LSOAs that are within this buffer using the rownames
#make them numeric
  row_o <- as.numeric(rownames(o[[6]]))
#plot tram station buffer number 6
  plot(tram_stations_joined_buffer[6,])
#Plot all the LSOA centroids
  plot(manchester_lsoa_points,pch=19,cex=.5,col="#7F7F7F",add=TRUE)
#Plot those LSOA centroids which are inside the buffer - i.e. we use the row_o to select the rows
  plot(manchester_lsoa_points[row_o,],pch=15,cex=1,col="#FF4500",add=TRUE)

  
#shows the tram station buffer (the circle), all of the LSOA centroids (grey dots) and those LSOA centroids within the buffer (red squares). For each tram buffer, we need to calculate the average IMD score, as we are likely to have a range of deprivation levels within the tram station buffer. The `colMeans()` function will calculate the mean value for each column (as you might have guessed!). We want to do this for each tram station buffer and the `lapply()` function allows us to do this - to apply a function (`colMeans()`) across the list of tram station buffers. We can then join this back onto the tram_stations data frame. 


  average_deprivation <- lapply(o, colMeans)
#View the first six items from the list
  average_deprivation[1:6]
#collapse list back into a normal data frame
  tram_deprivation <- data.frame(matrix(unlist(average_deprivation),
                                        nrow=length(average_deprivation), byrow=T))
#Change the column names to something sensible
  colnames(tram_deprivation) <- c("average_deprivation")
#This should look like
  head(tram_deprivation)
#Join - the ordering has not been changed so this is valid
  tram_stations@data <- cbind(tram_stations@data,tram_deprivation)
#show data
  head(tram_stations@data)
  
  
  