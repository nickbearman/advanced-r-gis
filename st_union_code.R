
#plot lsoa
qtm(manchester_lsoa)

#plot tram stations
qtm(tram_stations)

#plot tram stations buffer
qtm(tram_stations_1200_buffer)

tm_shape(manchester_lsoa) +
  tm_borders(col = "red") +
  tm_shape(tram_stations_1200_buffer) +
  tm_borders(col = "darkblue") 

tm_shape(tram_stations_1200_buffer) +
  tm_borders(col = "darkblue") +
tm_shape(manchester_lsoa) +
  tm_borders(col = "red")

tm_shape(tram_stations_1200_buffer[1,]) +
  tm_borders(col = "darkblue") +
  tm_shape(manchester_lsoa) +
  tm_borders(col = "red") 

head(manchester_lsoa)

union <- st_union(manchester_lsoa, tram_stations_1200_buffer)

head(union)

#aggregate
LSOA_crimes_aggregated <- aggregate(x = LSOA_crimes, by = list(LSOA_crimes$lsoa21cd), FUN = length)

#aggregate by tram station buffer
union_aggregated <- aggregate(x = union, by = list(union$Label_Text), FUN = sum)
#errors
union_aggregated <- aggregate(x = union, by = list(union$Label_Text), FUN = length)

#join
join <- st_join(manchester_lsoa, tram_stations_1200_buffer)
head(join)
View(join)
#list all lsoas by all stations,
#aggregtae, avergae?
join_aggregared <- aggregate(x = union, by = list(union$Label_Text), FUN = sum)


##From Tom Cunningham, happy to share 2023-05-04

# Calculate the weighted IMD per tram stop
wt_imd <- st_intersection(tram_stations_1200_buffer,
                          manchester_lsoa) %>% 
  mutate(area = st_area(.)) %>% 
  group_by(Label_Text) %>% 
  mutate(prop_area = as.numeric(area/sum(area)),
         sum_area = sum(area)) %>% 
  summarise(wt_mean_imd = sum(prop_area * IMDscore)) %>% 
  ungroup() %>% 
  as_tibble()

# Join weighted IMD with tram station pts
wt_imd_pt <- left_join(station_LSOA_IMD_pt, wt_imd, 
                       by = "Label_Text")

wt_imd_pt_reordered <- wt_imd_pt[order(wt_imd_pt$`mean(IMDscore)`, decreasing = TRUE), ]
head(wt_imd_pt_reordered)

# Plot weighted IMD
ggplot(wt_imd_pt) +
  ggspatial::annotation_map_tile(zoomin = 1) +
  geom_sf(aes(colour = wt_mean_imd), size = 4) +
  scale_colour_continuous("Weighted IMD", type = "viridis") +
  theme_void()

tm_shape(wt_imd_pt) +
  tm_dots(wt_imd_pt, size = 0.1, shape = 19, col = "darkred") +
  tm_shape(wt_imd_pt[1:10,]) +
  tm_dots(wt_imd_pt[1:10,], size = 0.1, shape = 19, col = "red") +
  tm_shape(wt_imd_pt[81:91,]) +
  tm_dots(wt_imd_pt[81:91,], size = 0.1, shape = 19, col = "blue") 



