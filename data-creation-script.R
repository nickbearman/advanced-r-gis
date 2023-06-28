

setwd("~/work/confident-spatial-analysis/data-user")

download.file("http://www.nickbearman.me.uk/data/r/GreaterManchester_lsoa_2011.zip",
              "GreaterManchester_lsoa_2011.zip")

library(sf)
library(tmap)
manchester_lsoa <- st_read("GreaterManchester_lsoa_2011.shp")
 head(manchester_lsoa)

 imd <- read.csv("imd.csv", header = TRUE)
 #delete columns we don't need
 imd <- imd[,1:7]
 #rename columns
 colnames(imd) <- c("LSOAcode","LSOAname","LADcode","LADname","IMDscore","IMDrank","IMDdecile")
 
 head(imd)
 
 manchester_lsoa <- merge(manchester_lsoa, imd, by.x = "CODE", by.y = "LSOAcode")
 
 head(manchester_lsoa) 

 download.file("http://www.nickbearman.me.uk/data/r/england_lsoa_2011.zip","england_lsoa_2011.zip") 

 unzip("england_lsoa_2011.zip") 
 
 newcolnames <- c("AllUsualResidents","Age00to04","Age05to07",
                  "Age08to09","Age10to14","Age15","Age16to17",
                  "Age18to19","Age20to24","Age25to29",
                  "Age30to44","Age45to59","Age60to64",
                  "Age65to74","Age75to84","Age85to89",
                  "Age90andOver","MeanAge","MedianAge")
 #apply these to pop2011 data frame
 colnames(pop2011)[5:23] <- newcolnames
 head(pop2011[,1:9])
 head(pop2011)
 download.file("http://www.nickbearman.me.uk/data/r/nomis-2011-age-data.zip","nomis-2011-age-data.zip")
 #unzip csv file
 unzip("nomis-2011-age-data.zip")
 pop2011 <- read.csv("bulk.csv", header = TRUE)
 head(pop2011)
 #create a new variable which contains the new variable names
 
 #apply these to pop2011 data frame
 colnames(pop2011)[5:23] <- newcolnames
 head(pop2011[,1:9])
head(pop2011)

head(manchester_lsoa)

#join attribute data to LSOA
manchester_lsoa <- merge(manchester_lsoa, pop2011, by.x="CODE", by.y="geography.code")
 
head(manchester_lsoa)

#write shape file

st_write(manchester_lsoa, "lsoa_manchester_age_imd.shp")
