library(plyr)
library(tidyverse)
library(sf)
library(osmextract)
library(lubridate)

# Load Cologne Stadtteile shapefile
shapefile_cologne <- read_sf("/Users/sydney/Downloads/Stadtteil_4(1)/Stadtteil.shp") %>% st_transform(4326)

#Load Cologne OSM data
#osm_data_polygons <- oe_read("https://download.geofabrik.de/europe/germany/nordrhein-westfalen/koeln-regbez-latest.osm.pbf", layer = "multipolygons")
osm_data_polygons <- oe_read("/Users/sydney/Downloads/koeln-regbez-latest.osm.pbf", layer = "multipolygons")

osm_multipolygons_buildings <- osm_data_polygons %>%
  filter(!is.na(building)) %>%
    filter(!is.na(osm_way_id)) %>%
    mutate(osm_way_id = as.numeric(osm_way_id)) %>%
    select(osm_way_id, building)

#osm_data_points <- oe_read("https://download.geofabrik.de/europe/germany/nordrhein-westfalen/koeln-regbez-latest.osm.pbf", layer = "points", extra_tags = c("amenity", "shop", "leisure")) 
osm_data_points <- oe_read("/Users/sydney/Downloads/koeln-regbez-latest.osm.pbf", layer = "points", extra_tags = c("amenity", "shop", "leisure"))

osm_data_points <- osm_data_points %>% select(osm_id, name, amenity, shop, leisure, geometry) %>%
                    filter(!is.na(osm_id))

sf_use_s2(FALSE)

osm_data_points <- st_join(osm_multipolygons_buildings, osm_data_points)
osm_data_points <- osm_data_points %>% mutate(osm_way_id = as.character(osm_way_id))

#Load netcheck data
setwd("/Users/sydney/root/svn/shared-svn/projects/episim/data/netcheck/data")

years <- c(2020) #, 2021)
weeks <- seq(1, 20, 1)

netcheck_data <- data.frame()
for (year in years){
    for (week in weeks){
        file_name <- paste0("tuberlin_pings2osm_cologne_", year, "_", week, ".csv")
        if (file.exists(file_name)) {
            read_in_data <- read_csv(file_name)
            netcheck_data <- rbind(netcheck_data, read_in_data)
        }
    }
}

netcheck_data$osm_id <- as.character(netcheck_data$osm_id)

#Merge netcheck and osm_data
netcheck_data_osm_points <- inner_join(netcheck_data, osm_data_points, by=c("osm_id" = "osm_way_id")) %>% st_as_sf() 
netcheck_data_osm_polygons <- inner_join(netcheck_data, osm_data_polygons, by=c("osm_id" = "osm_way_id")) %>% st_as_sf()

#Merge Netcheck and Cologne shapefile
netcheck_data_final <- st_join(netcheck_data_osm_polygons, shapefile_cologne)
netcheck_data_final_points <- st_join(netcheck_data_osm_points, shapefile_cologne)
netcheck_data_final_points <- netcheck_data_final_points %>% select(-c("highway", "ref", "address", "is_in"))
netcheck_data_final <- netcheck_data_final %>% select(colnames(netcheck_data_final_points))
netcheck_data_final <- rbind(netcheck_data_final, netcheck_data_final_points)

#Supermarkets/Discounter
netcheck_data_supermarket <- netcheck_data_final %>% filter(shop == "supermarket" | building == "supermarket" | shop == "health_food") %>% 
            mutate(year = year(day)) %>%
            mutate(week = week(day))
netcheck_data_supermarket <- plyr::rename(netcheck_data_supermarket,  replace = c("name.y" = "Stadtteil"))
netcheck_data_supermarket <- plyr::rename(netcheck_data_supermarket, replace = c("name.x" = "Supermarket"))
netcheck_data_supermarket <- netcheck_data_supermarket %>%
            mutate(Supermarket = case_when(Supermarket == "Rewe" ~ "Rewe",
                                        Supermarket == "REWE" ~ "Rewe",
                                        Supermarket == "REWE City" ~ "Rewe",
                                        Supermarket == "Rewe Center" ~ "Rewe",
                                        Supermarket == "REWE Rahmati" ~ "Rewe",
                                        Supermarket == "Kaufland" ~ "Kaufland",
                                        Supermarket == "PENNY" ~ "Penny",
                                        Supermarket == "Penny" ~ "Penny",
                                        Supermarket == "ALDI Süd" ~ "Aldi Süd",
                                        Supermarket == "Aldi Süd" ~ "Aldi Süd",
                                        Supermarket == "EDEKA" ~ "Edeka",
                                        Supermarket == "Edeka E Center Engels" ~ "Edeka",
                                        Supermarket == "Edeka Kipping" ~ "Edeka",
                                        Supermarket == "Netto Marken-Discount" ~ "Netto",
                                        Supermarket == "Netto City" ~ "Netto",
                                        Supermarket == "Lidl" ~ "Lidl",
                                        Supermarket == "Norma" ~ "Norma",
                                        Supermarket == "Seng Heng Asia Supermarkt" ~ "Seng Heng Asia Supermarkt",
                                        Supermarket == "Ringeltaube" ~ "Ringeltaube",
                                        Supermarket == "Handelshof" ~ "Handelshof",
                                        Supermarket == "Der Kölner Biobauer" ~ "Der Kölner Biobauer",
                                        Supermarket == "Naturata" ~ "Naturata",
                                        Supermarket == "SuperBioMarkt" ~ "SuperBioMarkt",
                                        Supermarket == "BIOSAM" ~ "Biosam",
                                        Supermarket == "Alnatura Super Natur Markt" ~ "Alnatura",
                                        Supermarket == "Alnatura" ~ "Alnatura",
                                        Supermarket == "Kraut & Rüben" ~ "Kraut & Rüben")) %>%
            mutate(Supermarket_type = case_when(Supermarket == "Rewe" ~ "Supermarket",
                                                Supermarket == "Kaufland" ~ "Supermarket",
                                                Supermarket == "Penny" ~ "Discounter",
                                                Supermarket == "Edeka" ~ "Supermarket",
                                                Supermarket == "Netto" ~ "Discounter",
                                                Supermarket == "Norma" ~ "Discounter",
                                                Supermarket == "Lidl" ~ "Discounter",
                                                Supermarket == "Aldi Süd" ~ "Discounter",
                                                Supermarket == "Seng Heng Asia Supermarkt" ~ "Misc",
                                                Supermarket == "Ringeltaube" ~ "Misc",
                                                Supermarket == "Handelshof" ~ "Misc",
                                                Supermarket == "Der Kölner Biobauer" ~ "Organic Supermarket",
                                                Supermarket == "Naturata" ~ "Organic Supermarket",
                                                Supermarket == "SuperBioMarkt" ~ "Organic Supermarket",
                                                Supermarket == "Biosam" ~ "Organic Supermarket",
                                                Supermarket == "Alnatura" ~ "Organic Supermarket",
                                                Supermarket == "Kraut & Rüben" ~ "Organic Supermarket"))

netcheck_data_supermarket %>% mutate(week = week(day)) %>%
    group_by(week, Stadtteil) %>%
    dplyr::count(shop) %>%
    ggplot(aes(x = week, y = n, color = Stadtteil)) +
    geom_line() +
    theme_minimal() +
    xlab("Week no. in 2020") +
    ylab("No. of rows, multiple timestamps = 1 row") +
    ggtitle("Supermarkets") +
    theme(legend.position = "bottom", text = element_text(size = 13)) +
    theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))


netcheck_data_supermarket %>% mutate(week = week(day)) %>% drop_na(Supermarket_type) %>%
    group_by(week, Stadtteil) %>%
    dplyr::count(shop) %>%
    ggplot(aes(x = week, y = n)) +
    geom_line() +
    facet_wrap(~Stadtteil) +
    theme_minimal() +
    xlab("Week no. in 2020") +
    ylab("No. of rows, multiple timestamps = 1 row") +
    ggtitle("Supermarkets") +
    theme(legend.position = "bottom", text = element_text(size = 13)) +
    theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

netcheck_data_supermarket %>% mutate(week = week(day)) %>% drop_na(Supermarket_type) %>%
    group_by(week, Stadtteil, Supermarket_type) %>%
    dplyr::count(shop) %>%
    ggplot(aes(x = week, y = n, color = Supermarket_type )) +
    geom_line() +
    facet_wrap(~Stadtteil) +
    theme_minimal() +
    ylab("No. of rows, multiple timestamps = 1 row") +
    xlab("Week no. in 2020") +
    ggtitle("Supermarkets") +
    theme(legend.position = "bottom", text = element_text(size = 13)) +
    theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))



#Place of worship
netcheck_data_pow <- netcheck_data_final %>% filter(building %in% c("cathedral", "chapel", "mosque", "monastery", "place_of_worship") | amenity == "place_of_worship") %>%
            mutate(year = year(day)) %>%
            mutate(week = week(day))

netcheck_data_pow <- plyr::rename(netcheck_data_pow,  replace = c("name.y" = "Stadtteil"))

netcheck_data_pow %>% mutate(week = week(day)) %>%
    group_by(week, building) %>%
    dplyr::count(building) %>%
    ggplot(aes(x=week, y = n, color = building)) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    xlab("Week no. in 2020") +
    ylab("No. of rows, multiple timestamps = 1 row") +
    ggtitle("Place Of Worship")

netcheck_data_pow %>% mutate(week = week(day)) %>%
    group_by(week, Stadtteil, building) %>%
    dplyr::count(building) %>%
    ggplot(aes(x=week, y = n, color = building)) +
    geom_line() +
    facet_wrap(~Stadtteil)+
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    xlab("Week no. in 2020") +
    ylab("No. of rows, multiple timestamps = 1 row") +
    ggtitle("Place Of Worship")

#Sports hall/Centre
netcheck_data_sports <- netcheck_data_final %>% filter(building == "sports_hall" | leisure == "sports_hall") %>%
            mutate(year = year(day)) %>%
            mutate(week = week(day))

netcheck_data_sports <- plyr::rename(netcheck_data_sports,  replace = c("name.y" = "Stadtteil"))


netcheck_data_sports %>% mutate(week = week(day)) %>%
    group_by(week) %>%
    dplyr::count(building) %>%
    ggplot(aes(x=week, y = n)) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    xlab("Week no. in 2020") +
    ylab("No. of rows, multiple timestamps = 1 row") +
    ggtitle("Sports Halls")

  netcheck_data_sports %>% mutate(week = week(day)) %>%
    group_by(week,Stadtteil) %>%
    dplyr::count(building) %>%
    ggplot(aes(x=week, y = n)) +
    geom_line() +
    facet_wrap(~Stadtteil) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    xlab("Week no. in 2020") +
    ylab("No. of rows, multiple timestamps = 1 row") +
    ggtitle("Sports Halls")

#Park Playground
netcheck_data_park <- netcheck_data_final %>% filter(leisure %in% c("park", "playground")) %>%
            mutate(year = year(day)) %>%
            mutate(week = week(day))

netcheck_data_park <- plyr::rename(netcheck_data_park,  replace = c("name.y" = "Stadtteil"))
          
netcheck_data_park %>% mutate(week = week(day)) %>%
    group_by(week, leisure) %>%
    dplyr::count(leisure) %>%
    ggplot(aes(x=week, y = n, color = leisure)) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    xlab("Week no. in 2020") +
    ylab("No. of rows, multiple timestamps = 1 row") +
    ggtitle("Parks")

netcheck_data_park %>% mutate(week = week(day)) %>%
    group_by(week, leisure, Stadtteil) %>%
    dplyr::count(leisure) %>%
    ggplot(aes(x = week, y = n, color = leisure)) +
    facet_wrap(~Stadtteil) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    xlab("Week no. in 2020") +
    ylab("No. of rows, multiple timestamps = 1 row") +
    ggtitle("Parks")

#Restaurant, bar, cafe, pub, biergarten
netcheck_data_rest <- netcheck_data_final %>% filter(amenity %in% c("restaurant", "bar", "cafe", "pub", "biergarten")) %>%
            mutate(year = year(day)) %>%
            mutate(week = week(day))

netcheck_data_rest <- plyr::rename(netcheck_data_rest,  replace = c("name.y" = "Stadtteil"))
                  
netcheck_data_rest %>% mutate(week = week(day)) %>%
    group_by(week, amenity) %>%
    dplyr::count(amenity) %>%
    ggplot(aes(x = week, y = n, color = amenity)) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title=element_blank()) +
    xlab("Week no. in 2020") +
    ylab("No. of rows, multiple timestamps = 1 row") +
    ggtitle("Bars, Restaurants, Cafes, Pubs, Beergardens")

    netcheck_data_rest %>% mutate(week = week(day)) %>%
    group_by(week, amenity, Stadtteil) %>%
    dplyr::count(amenity) %>%
    ggplot(aes(x = week, y = n, color = amenity)) +
    geom_line() +
    facet_wrap(~Stadtteil) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    xlab("Week no. in 2020") +
    ylab("No. of rows, multiple timestamps = 1 row") +
    ggtitle("Bars, Restaurants, Cafes, Pubs, Beergardens")
