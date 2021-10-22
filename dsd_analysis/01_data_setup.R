library(sf); library(tidyverse); library(mapview)
mapviewOptions(vector.palette = viridisLite::inferno, alpha=0.9, basemaps = c("CartoDB.Positron", "OpenStreetMap"), legend.opacity = 1 )
#' import dhaka and set zones 

load("dsd_data/dhaka_wards.rda"); mapview(dhaka_wards)

# validate polygons and set crs
dhaka_wards <- st_transform(st_make_valid(dhaka_wards), crs=32646)
# write.csv(as_tibble(as.data.frame(dhaka_wards)) %>% select(-geometry), "dsd_data/dhaka_wards.csv")

# master file file ward id and zone number
zone_convert <- read.csv("dsd_data/dhaka_zone_ward_convert.csv")
names(zone_convert)[1] <- "ID"

dhaka_wards <- left_join(dhaka_wards, zone_convert, by="ID")
dhaka_wards$zone <- paste0(stringr::str_sub(dhaka_wards$ID,1,1), "_", dhaka_wards$zone)

dhaka_zones <- dhaka_wards %>% group_by(zone) %>% 
                summarise(hhs = sum(households),
                          pop = sum(pop.total),
                          dhs = mean(DHS.mean),
                          usd = mean(USD.mean),
                          urban = mean(urban),
                          rural = mean(rural),
                          water = mean(water),
                          roads_lenght = sum(roads.lenght)/1000)

mapview(dhaka_zones)
dhaka_zones$area_km <- as.numeric(st_area(dhaka_zones)/1000000) 

# import dog data into zones
dog_data <- read_csv("dsd_data/dog_data.csv") %>% select(zone, human_pop, dog_pop, human_dog_ratio)
dhaka_zones <- left_join(dhaka_zones, dog_data, by="zone")

dhaka_zones$dog_dens_km_area <- dhaka_zones$dog_pop/dhaka_zones$area_km
dhaka_zones$dog_dens_km_road <- dhaka_zones$dog_pop/dhaka_zones$roads_lenght

ggplot(dhaka_zones, aes(fill=dog_dens_km_area)) + geom_sf() + theme_minimal() + scale_fill_viridis_c()
ggplot(dhaka_zones, aes(fill=dog_pop)) + geom_sf() + theme_minimal() + scale_fill_viridis_c()
ggplot(dhaka_zones, aes(fill=human_dog_ratio)) + geom_sf() + theme_minimal() + scale_fill_viridis_c()
ggplot(dhaka_zones, aes(fill=dog_dens_km_road)) + geom_sf() + theme_minimal() + scale_fill_viridis_c()
ggplot(dhaka_zones, aes(fill=urban)) + geom_sf() + theme_minimal() + scale_fill_gradient(low = 'white', high = 'darkgrey')
ggplot(dhaka_zones, aes(fill=rural)) + geom_sf() + theme_minimal() + scale_fill_gradient(low = 'white', high = 'forestgreen')
ggplot(dhaka_zones, aes(fill=water)) + geom_sf() + theme_minimal() + scale_fill_gradient(low = 'white', high = 'navy')

write_sf(dhaka_zones, "dsd_data/shps/dhaka_zones_data.shp")

#'--------------------------------------------------------------------------------------------
#' import and setup low income areas

dhaka_low <- read_sf("dsd_data/shps/dhaka_slums.shp")

# validate polygons, set crs  
dhaka_low <- st_transform(st_make_valid(dhaka_low), crs=32646); mapview(dhaka_low)

# add 50 meters buffer around slum polygons, to account for change from 2010
dhaka_low <- st_buffer(x=dhaka_low, dist=50); mapview(dhaka_low)

ggplot() + geom_sf(data=dhaka_zones, aes(fill=dog_dens_km_road)) + 
  geom_sf(data=dhaka_low, fill="coral", color="coral") + theme_minimal()

#'--------------------------------------------------------------------------------------------
#' import and setup osm building region

# file kills R RAM, processed in QGIS, keep building polygon within dhaka, setup buffer smooth them out

dhaka_building <- read_sf("dsd_data/shps/simplified_dhaka_large_buildings.shp")
dhaka_building$ID <- 1; dhaka_building <- summarise(group_by(dhaka_building,ID))

survey_points <- st_sample(dhaka_building, size=900) %>% st_transform(crs=32646)

# get distance to closest point
distance_data <- matrix(as.numeric(st_distance(survey_points)), 900, 900) # pairwise distance between each point
distance_data[distance_data == 0] <- 100000 # remove diagonal
closest_point_distance <- apply(distance_data, 2, min)

summary(closest_point_distance)
sd(closest_point_distance)

# number of minutes between points on average, assuming 1.3 meters per second
(mean(closest_point_distance)/1)/60 * 2

m <- mapview(dhaka_zones, col.regions="navy") + 
      mapview(dhaka_building, col.regions="coral") + 
        mapview(survey_points, col.regions="green") + 
          mapview(dhaka_low, col.regions="grey")
mapshot(m, url = "dsd_output/base_map.html")
setwd("dsd_output/") ; mapviewOptions(fgb = F)
htmlwidgets::saveWidget(m@map, file = "base_map.html", selfcontained = F)

