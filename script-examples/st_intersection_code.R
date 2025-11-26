
library(dplyr)

# Calculate the weighted IMD per tram stop
wt_imd <- st_intersection(tram_stations_buffer,
                          manchester_lsoa) %>% 
  mutate(area = st_area(.)) %>% 
  group_by(RSTNAM) %>% 
  mutate(prop_area = as.numeric(area/sum(area)),
         sum_area = sum(area)) %>% 
  summarise(wt_mean_imd = sum(prop_area * IMDdecile)) %>% 
  ungroup() %>% 
  as_tibble()

#alternative approach with pipe and destination at the end of the pipe
st_intersection(tram_stations_buffer,
                          manchester_lsoa) %>% 
  mutate(area = st_area(.)) %>% 
  group_by(RSTNAM) %>% 
  mutate(prop_area = as.numeric(area/sum(area)),
         sum_area = sum(area)) %>% 
  summarise(wt_mean_imd = sum(prop_area * IMDdecile)) %>% 
  ungroup() %>% 
  as_tibble() -> 
  wt_imd

#there is quite a lot going on in this code. you can do one part at a time
#by starting with, for example:

#wt_imd <- st_intersection(tram_stations_buffer,
#                          manchester_lsoa) 

#and then adding each section as you go. 

# Join weighted IMD with tram station pts
wt_imd_pt <- left_join(station_LSOA_IMD_pt, wt_imd, 
                       by = "RSTNAM")

wt_imd_pt_reordered <- wt_imd_pt[order(wt_imd_pt$`mean(IMDdecile)`, decreasing = TRUE), ]
head(wt_imd_pt_reordered)

# Plot weighted IMD
tm_shape(wt_imd_pt_reordered) +
  tm_dots(size = 0.2, fill = "darkred") +
  tm_shape(wt_imd_pt_reordered[1:10,]) +
  tm_dots(size = 0.2, fill = "red") +
  tm_shape(wt_imd_pt_reordered[89:99,]) +
  tm_dots(size = 0.2, fill = "blue")

#tmap plot filled with colour for IMD decile
tm_shape(wt_imd_pt) +
  tm_dots(size = 0.7, fill = "wt_mean_imd",
              fill.scale = tm_scale_intervals(values = "brewer.blues", style = "jenks"),
              fill.legend = tm_legend(title = "Average IMD Decile")) 


#this is a plot using the package ggspatial (part of ggplot2) 
# if you are interested in trying this
library(ggplot2)
library(ggspatial)
library(prettymapr)
ggplot(wt_imd_pt) +
  geom_sf(aes(colour = wt_mean_imd), size = 4) +
  scale_colour_continuous("Weighted IMD Decile", type = "viridis") +
  theme_void()

##Many thanks to Tom Cunningham, who wrote this and shared it on 2023-05-04

#weighted-average-graph.png

## an alternative approach, splitting up the pipe code a bit

  poly_poly <- st_intersection(tram_stations_buffer, manchester_lsoa) %>%
  mutate(area = st_area(.))

poly_poly_wght <- poly_poly %>%
  group_by(RSTNAM) %>%
  mutate(area_sum = sum(area)) %>%
  mutate(area_share = area/area_sum) %>%
  mutate(total_share = sum(area_share)) %>%
  ungroup()

polys_wght <- poly_poly_wght %>%
  group_by(RSTNAM) %>%
  summarise(weightedIMDdecile = sum(IMDdecile*area_share))


trams_wght <- st_centroid(polys_wght)  

trams_wght <- trams_wght[order(trams_wght$weightedIMDdecile, decreasing = TRUE), ]

# Plot
tm_shape(trams_wght) +
  tm_dots(size = 0.4, fill = "darkred") +
  tm_shape(trams_wght[1:10,]) +
  tm_dots(size = 0.4, fill = "red") +
  tm_shape(trams_wght[89:99,]) +
  tm_dots(size = 0.4, fill = "blue")

# thanks to Patrick Schjølberg who shared this code on 2025-11-26. 

## a different approach again, processing the code in a different order. 

  tram_stations_buffer <- tram_stations_buffer %>% 
  mutate(bufferArea = as.integer(st_area(geometry)))



intersected_imd_tramstation <- st_intersection(manchester_lsoa, tram_stations_buffer)
plot(manchester_lsoa$geom, axes = TRUE)
plot(tram_stations_buffer$geometry, add = TRUE)
plot(intersected_imd_tramstation$geom, add = FALSE, col = 'red')

areaIMD <- intersected_imd_tramstation %>% 
  mutate(area = as.numeric(st_area(.)))

area_IMD_perTramStation <- areaIMD %>% 
  group_by(RSTNAM, IMDdecile, bufferArea) %>% 
  summarize(area = sum(area)) %>% 
  ungroup() %>% 
  mutate(percentageIMD = area/bufferArea) %>% 
  mutate(percentageIMD_2dp = round(percentageIMD, 2)) %>% 
  group_by(RSTNAM) %>% 
  summarise(FinalScore = weighted.mean(IMDdecile,percentageIMD ))



#reorder, most deprived at the top
area_IMD_perTramStation_ordered <- area_IMD_perTramStation[order(area_IMD_perTramStation$FinalScore,
                                                                 decreasing = TRUE), ]

area_IMD_perTramStation <- st_make_valid(area_IMD_perTramStation)

#plot map of weighted average IMD score by station (top 10 in Red, bottom 10 in Blue)
tm_shape(area_IMD_perTramStation) +
  tm_polygons(fill = "FinalScore",
              fill.scale = tm_scale_intervals(values = "brewer.blues", style = "jenks"),
              fill.legend = tm_legend(title = "Average IMD Decile")) +
  tm_shape(area_IMD_perTramStation_ordered) +
  tm_dots(size = 0.5, fill = "darkred") +
  tm_shape(area_IMD_perTramStation_ordered[1:10,]) +
  tm_dots(size = 0.5, fill = "red") +
  tm_shape(area_IMD_perTramStation_ordered[89:99,]) +
  tm_dots(size = 0.5, fill = "blue")

#the artefacts in the buffers are from the st_intersection - they are the LSOA boundaries

# Thanks to Dave who sahred this on 2025-11-26
