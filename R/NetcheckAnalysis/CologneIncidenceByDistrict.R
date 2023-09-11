library(tidyverse)
library(gghighlight)

library(raster)
library(readxl)
library(sf)
library(maps)
library(spData)
library(magick)
library(grid)
library(tmap)
library(viridis)

cologne_by_district <- read_csv("/Users/sydney/root/svn/shared-svn/projects/episim/data/cologneIncidencesByDistrict/incidences_20230315.csv")

# Clustering districts simply by the course of their incidence curve
# Following the example of this blogpost https://www.storybench.org/how-to-use-hierarchical-cluster-analysis-on-time-series-data/
ggplot(cologne_by_district, aes(date, y = incidence, color = district)) +
geom_line(stat = "identity") +
ylab("Incidence per 100,000") +
gghighlight(max(incidence) > 4500,
            max_highlight = 4,
            use_direct_label = TRUE) +
theme_minimal() +
theme(legend.position = 'none')

spread_cologne_by_district <- cologne_by_district %>%
    spread(district, incidence) %>%
    glimpse()

incidences <- t(spread_cologne_by_district[-1])
incidences_dist <- dist(incidences, method = "euclidean")
fit <- hclust(incidences_dist, method = "ward.D")

plot(fit, family = "Arial")
rect.hclust(fit, k = 5, border = "cadetblue")


clustered_data <- cutree(fit, k = 5)
clustered_data_tidy <- as.data.frame(as.table(clustered_data)) %>% glimpse()
colnames(clustered_data_tidy) <- c("district", "cluster")
clustered_data_tidy$district <- as.character(clustered_data_tidy$district)

joined_clusters <- cologne_by_district %>%
    inner_join(clustered_data_tidy, by = "district") %>%
    glimpse()

# Now: Plot data spatially
#The following code was created with the help of https://bookdown.org/nicohahn/making_maps_with_r5/docs/tmap.html

cologne_shp <- read_sf("/Users/sydney/Downloads/Stadtteil/Stadtteil.shp")

spatial_plot <- function(Date){
cologne_by_districtSep <- cologne_by_district %>% filter(date == Date)

cologne_shp <- inner_join(cologne_shp, cologne_by_districtSep, by = join_by(name == district))

tm_shape(cologne_shp) +
tm_borders() +
tm_polygons(col="incidence.x", breaks = c(0, 10, 30, 50, 100, 150, 200, +Inf), palette = "-viridis") +
tm_layout(legend.position = c("right", "top"), title= paste0("Incidence on ", as.character(Date)), title.position = c('right', 'top'))
}

cologne_animation <- tm_shape(cologne_shp) +
  tm_polygons(
    col = "incidence",
    breaks = c(0, 10, 30, 50, 100, 150, 200, +Inf),
    palette = "-viridis"
    ) +
  tm_facets(along = "year") +
  tm_layout(legend.position = c("left", "bottom"))
tmap_animation(
  honey_animation, filename = "honey.gif",
  delay = 50, width = 2400, height = 1200
  )

