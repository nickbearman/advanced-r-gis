# load libraries
library(sf)
library(tmap)

# Download census2021-ts007a-oa.csv from Nomisweb following these instructions: 

# Navigate to https://www.nomisweb.co.uk/.
#Under Census Statistics (scroll down) click 2021 Data catalogue.
#Select Bulk Data Downloads (near the bottom of the page).
# Scroll down to TS007A Age by five-year age bands.
#Download the census2021-ts007a.zip zip file.
# Open up the zip file - you will see a csv file for each geography - LSOA, MSOA, OA and various others.
# Extract the file census2021-ts007a-lsoa.csv

# read in data
pop2021 <- read.csv("census2021-ts007a-lsoa.csv", header = TRUE)
head(pop2021)

#create a new variable which contains the new variable names
newcolnames <- c("Total","Age00to04","Age05to09","Age10to14","Age15to19",
                 "Age20to24","Age25to29","Age30to34","Age35to39",
                 "Age40to44","Age45to49","Age50to54","Age55to59",
                 "Age60to64","Age65to69","Age70to74","Age75to79",
                 "Age80to84","Age85andOver")

#apply these to pop2011 data frame
colnames(pop2021)[4:22] <- newcolnames

head(pop2021)

# Download england_lsoa_2021.shp from https://borders.ukdataservice.ac.uk following these instructions:

# Go to https://borders.ukdataservice.ac.uk/
#   Select Boundary Data Selector.
# Then set:
#   Country to England
# Geography to Statistical Building Block
# Dates to 2021 and later
# click Find.
# Select English Lower Layer Super Output Areas, 2021 and click List Areas.
# Select England from the list and choose Expand Selection.
# Select North West [England] from the list and choose Expand Selection.
# Select Manchester, Salford, Bolton, Bury, Oldham, Rochdale, Stockport, Tameside, Trafford, and Wigan. These areas form Greater Manchester.
# Click Extract Boundary Data.
# After a 5 to 20 second wait, click BoundaryData.zip to download the files.
# Extract the file england_lsoa_2021.shp

#read in shapefile
LSOA_full <- st_read("england_lsoa_2021.shp")

#preview
#qtm(LSOA_full) #the data is a little on the large side (18mb) so the map might take a few seconds to draw
object.size(LSOA_full) #show object size

#simplify data
# see https://r.geocompx.org/geometry-operations.html?q=simplify#simplification for details on st_simplify
LSOA = st_simplify(LSOA_full, dTolerance = 3)  # 3 m # had to experiment quite a bit here to find the right balance between filesize and detail for zoomed in map (tm_shape(LSOA[979,]))
object.size(LSOA) #4.3mb
qtm(LSOA)

#join attribute data to LSOA
LSOA <- merge(LSOA, pop2021, by.x="lsoa21cd", by.y="geography.code")

head(LSOA)

# Download the IMD data

# Go to https://www.gov.uk/government/statistics/english-indices-of-deprivation-2025
# Download File 1: Index of Multiple Deprivation
# Use Excel (or other spreadsheet of your choice, e.g. Calc from LibreOffice) to export the IMD25 tab as a CSV file

# read in CSV file
imd <- read.csv("File_1_IoD2025_Index_of_Multiple_Deprivation.csv", header = TRUE)

head(imd)

#create a new variable which contains the new variable names
newcolnames <- c("LSOAcode2021","LSOAname2021","LADcode2024","LADname2024",
                 "IMDrank","IMDdecile")

#apply these to pop2011 data frame
colnames(imd)[1:6] <- newcolnames

head(imd)

# Join IMD data to LSOA
LSOA <- merge(LSOA, imd, by.x="lsoa21cd", by.y="LSOAcode2021")

head(LSOA)

# Do a quick map

qtm(LSOA, fill = "IMDdecile")

# Save as shapefile
st_write(LSOA, "Greater_Manchester_LSOA_Age_IMD.shp")

Greater_Manchester_LSOA <- st_read("Greater_Manchester_LSOA_Age_IMD.shp")
head(Greater_Manchester_LSOA)

# note as a shapefile, some field names are abbreviated. 
# Warning message:
# In abbreviate_shapefile_names(obj) :
#  Field names abbreviated for ESRI Shapefile driver

# Save as GeoPackage
st_write(LSOA, "Greater_Manchester_LSOA_Age_IMD.gpkg")

Greater_Manchester_LSOA <- st_read("Greater_Manchester_LSOA_Age_IMD.gpkg")
head(Greater_Manchester_LSOA)

# Both shapefile and geopackage can be read by GeoDa.
