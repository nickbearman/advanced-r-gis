
library(dplyr)

# Calculate the weighted IMD per tram stop
wt_imd <- st_intersection(tram_stations_buffer,
                          manchester_lsoa) %>% 
  mutate(area = st_area(.)) %>% 
  group_by(RSTNAM) %>% 
  mutate(prop_area = as.numeric(area/sum(area)),
         sum_area = sum(area)) %>% 
  summarise(wt_mean_imd = sum(prop_area * IMDscor)) %>% 
  ungroup() %>% 
  as_tibble()

#there is quite a lot going on in this code. you can do one part at a time
#by starting with, for example:

#wt_imd <- st_intersection(tram_stations_buffer,
#                          manchester_lsoa) 

#and then adding each section as you go. 

# Join weighted IMD with tram station pts
wt_imd_pt <- left_join(station_LSOA_IMD_pt, wt_imd, 
                       by = "RSTNAM")

wt_imd_pt_reordered <- wt_imd_pt[order(wt_imd_pt$`mean(IMDscor)`, decreasing = TRUE), ]
head(wt_imd_pt_reordered)

# Plot weighted IMD

tm_shape(wt_imd_pt) +
  tm_dots(size = 0.2, fill = "darkred") +
  tm_shape(wt_imd_pt[1:10,]) +
  tm_dots(size = 0.2, fill = "red") +
  tm_shape(wt_imd_pt[89:99,]) +
  tm_dots(size = 0.2, fill = "blue")

#this is a plot using the package ggspatial (part of ggplot2) 
# if you are interested in trying this
library(ggplot2)
library(ggspatial)
library(prettymapr)
ggplot(wt_imd_pt) +
  ggspatial::annotation_map_tile(zoomin = 1) +
  geom_sf(aes(colour = wt_mean_imd), size = 4) +
  scale_colour_continuous("Weighted IMD", type = "viridis") +
  theme_void()

##Many thanks to Tom Cunningham, who wrote this and shared it on 2023-05-04

#weighted-average-graph.png

