#Spatial Analysis in R Final Project
#Vigo Bertolo 
#Title: FrogWatch Point Pattern Analysis

#Load packages:

library(tidyverse)
library(sf)
library(tmap)
library(tigris)
library(spatstat)
library(SpatialKDE)

#Load FrogWatch data: 
frogwatch.obs <- read_csv("./frogwatch_obs.csv")
glimpse(frogwatch.obs)

#Convert FrogWatch data to sf object:
fw.obs <- sf::st_as_sf(frogwatch.obs, 
                       coords = c("Longitude", "Latitude"), 
                       crs = 4269)
glimpse(fw.obs)

#Subset data to only include observations in Ohio:

fw.obs.oh <- filter(fw.obs, State == "Ohio")
glimpse(fw.obs.oh)

#Delete observation with inaccurate coordinates: 

fw.obs.oh <- fw.obs.oh[-3,]

#Load state of Ohio county data using tigris package: 

oh.counties <- tigris::counties(state = "OH", cb = TRUE)
glimpse(oh.counties)

#Check if CRS of FrogWatch observations matches Ohio counties:

st_crs(fw.obs.oh) == st_crs(oh.counties)

#EDA: Map observations on Ohio counties: 

tm_shape(oh.counties) +
  tm_polygons() +
  tm_shape(fw.obs.oh) +
  tm_dots(fill = "Species", size = 0.25)

tm_shape(oh.counties) +
  tm_polygons() +
  tm_shape(fw.obs.oh) +
  tm_dots(fill = "Site Habitat", size = 0.25)

tm_shape(oh.counties) +
  tm_polygons() +
  tm_shape(fw.obs.oh) +
  tm_dots(fill = "Characterize Land Use", size = 0.25)

#Subset observations to only include observations in northeast Ohio counties:

fw.obs.oh.ne <- fw.obs.oh %>%
  filter(County %in% c("Medina",
           "Summit",
           "Portage",
           "Cuyahoga",
           "Wayne",
           "Stark",
           "Lorain",
           "Geauga",
           "Carroll",
           "Ashtabula",
           "Tuscarawas",
           "Mahoning"))

#Subset Ohio counties to only counties in northeast Ohio:

ne.oh.counties <- oh.counties %>%
  filter(NAME %in% c("Medina",
                     "Summit",
                     "Portage",
                     "Cuyahoga",
                     "Wayne",
                     "Stark",
                     "Lorain",
                     "Geauga",
                     "Carroll",
                     "Ashtabula",
                     "Tuscarawas",
                     "Mahoning",
                     "Lake",
                     "Trumbull",
                     "Columbiana"))

#EDA: Map northeast Ohio observations on northeast Ohio counties:

tm_shape(ne.oh.counties) +
  tm_polygons() +
  tm_shape(fw.obs.oh.ne) +
  tm_dots(fill = "Species", size = 0.25)

tm_shape(ne.oh.counties) +
  tm_polygons() +
  tm_shape(fw.obs.oh.ne) +
  tm_dots(fill = "Site Habitat", size = 0.25)

tm_shape(ne.oh.counties) +
  tm_polygons() +
  tm_shape(fw.obs.oh.ne) +
  tm_dots(fill = "Characterize Land Use", size = 0.25)

#Clean northeast Ohio observations by species and habitat:

#Filtering species column to only include common species in northeast Ohio:
fw.obs.oh.ne.spp <- fw.obs.oh.ne %>%
  filter(Species %in% c("American Bullfrog",
                        "American Toad",
                        "Green Frog",
                        "Gray Treefrog",
                        "Northern Leopard Frog",
                        "Wood Frog",
                        "Spring Peeper",
                        "Western Chorus Frog"))

#Filtering habitat column to only include major wetland types: 
fw.obs.oh.ne.hab <- fw.obs.oh.ne.spp %>%
  filter(`Site Habitat` %in% c("Pond",
                               "Vernal Pool",
                               "Wet Meadow",
                               "Freshwater Marsh",
                               "Swamp or Woodland Swamp")) %>%
  rename("Wetland Habitat" = `Site Habitat`) #rename habitat column
  
#Map scrubbed northeast Ohio observations on northeast Ohio counties:

#Mapping observations by species:
species.map <- tm_shape(ne.oh.counties) +
  tm_polygons() +
  tm_shape(fw.obs.oh.ne.spp) +
  tm_dots(col = "Species", 
          palette = "viridis",
          size = 0.5, 
          alpha = 0.5) +
  tm_title("NE Ohio Frogwatch Observations by Species") +
  tm_layout(title.position = c("center","top"),
           title.fontface = "bold",
           title.size = 5) +
  tm_compass() + tm_scalebar()
species.map

##Mapping observations by wetland habitat: 
habitat.map <- tm_shape(ne.oh.counties) +
  tm_polygons() +
  tm_shape(fw.obs.oh.ne.hab) +
  tm_dots(col = "Wetland Habitat",
          palette = "viridis", 
          size = 0.5, 
          alpha = 0.5) +
  tm_title("NE Ohio Frogwatch Observations by Habitat") +
  tm_layout(title.position = c("center","top"),
            title.fontface = "bold",
            title.size = 5) +
  tm_compass() + tm_scalebar()
habitat.map

#Layout maps together:

spp.hab.map <- tmap_arrange(species.map, habitat.map, nrow = 1)
spp.hab.map

#EDA: Aspatial visualizations of species and habitat data: 

habitat.plot <- fw.obs.oh.ne.hab %>% 
  ggplot(., aes(x = `Wetland Habitat`)) + 
  geom_bar(fill = "darkblue") +
  labs(title = "Northeast Ohio FrogWatch Observations by Wetland Habitat",
       x = "Wetland Type",
       y = "Number of Observations") +
  theme_classic()
habitat.plot

species.plot <- fw.obs.oh.ne.spp %>% 
  ggplot(., aes(x = Species)) +
  geom_bar(fill = "darkolivegreen") +
  labs(title = "Northeast Ohio FrogWatch Observations by Species",
       x = "Species Heard",
       y = "Number of Observations") +
  theme_classic()
species.plot

#Point Pattern Analysis starts:

#Kernel Density Estimation for NE Ohio observations (heat map):
fw.obs.oh.ne.projected <- st_transform(fw.obs.oh.ne, crs = 3734) #reproject
kde.grid <- SpatialKDE :: create_grid_rectangular(fw.obs.oh.ne.projected,
                                                  cell_size = 5280, #square mile
                                                  side_offset = 10000)
fw.obs.kde <- SpatialKDE :: kde(fw.obs.oh.ne.projected, 
                                band_width = 10000, 
                                grid = kde.grid)
plot(fw.obs.kde,
     main = "Kernel Density Estimation for NE Ohio FrogWatch Observations")

#KDE map showing observation points and county borders for more context:
kde.map <- tm_shape(fw.obs.kde) +
  tm_polygons(
    col = "kde_value", 
    style = "jenks", 
    palette = "viridis", 
    title = "KDE") +
  tm_shape(fw.obs.oh.ne.projected) +
  tm_bubbles(size = 0.1, col = "white") +
  tm_title("Kernel Density Estimation for NE Ohio FrogWatch Observations") +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(ne.oh.counties) + tm_borders(col = "black", 
                                        lwd = 1.5)
kde.map

#Ripley's K tests for dispersion/clustering using spatstat package:

#Convert data to ppp object:
fw.obs.window <- owin(xrange = c(-83,-80), yrange = c(40, 42)) #NE Ohio window
fw.ppp <- ppp(x = frogwatch.obs$Longitude, 
              y = frogwatch.obs$Latitude,
              window = fw.obs.window)

#Run univariate Ripley's K function for all points: 
univ.k.test <- Kest(fw.ppp)
plot(univ.k.test,
     main = "Ripley's K Function for NE Ohio FrogWatch Observations")
#Observed K curve above expected line signifies clustering of points

#Marked Point Pattern Analysis starts:

#Mark species data according to family, a proxy for body size/nutrient export: 
fw.obs.marked <- frogwatch.obs %>%
  mutate(., Family = case_when(Species == "American Bullfrog" ~ 3,
                               Species == "Green Frog" ~ 3,
                               Species == "Northern Leopard Frog" ~ 3,
                               Species == "Pickerel Frog" ~ 3,
                               Species == "Wood Frog" ~ 3,
                               Species == "American Toad" ~ 2,
                               Species == "Fowler's Toad" ~ 2,
                               Species == "Gray Treefrog" ~ 1,
                               Species == "Spring Peeper" ~ 1, 
                               Species == "Western Chorus Frog" ~ 1,
                               Species == "Blanchard's Cricket Frog" ~ 1,
                               TRUE ~ 0))
#Ranidae or true frogs are largest on average and get a value of 3
#Bufonidae or toads are intermediate in size and get a value of 2
#Hylidae or tree frogs are smallest on average and get a value of 1

#Convert marked data to ppp object:
fw.marked.ppp <- ppp(fw.obs.marked$Longitude, 
                     y = fw.obs.marked$Latitude,
                     window = fw.obs.window,
                     marks = as.factor(fw.obs.marked$Family))

#Run Cross-L-Function to test for clustering/dispersion between marks: 
true.toad.test <- Lcross(fw.marked.ppp, i = 3, j = 2, 
                         correction = "border")
true.toad.plot <- plot(true.toad.test,
                       main = "Cross-L-Function between Ranidae and Bufonidae")
#Observed line above expected line signifies clustering of points

true.tree.test <- Lcross(fw.marked.ppp, i = 3, j = 1, 
                         correction = "border")
true.tree.plot <- plot(true.tree.test,
                       main = "Cross-L-Function between Ranidae and Hylidae")
#Observed line above expected line signifies clustering of points

toad.tree.test <- Lcross(fw.marked.ppp, i = 2, j = 1, 
                         correction = "border")
toad.tree.plot <- plot(toad.tree.test,
                       main = "Cross-L-Function between Bufonidae and Hylidae")
#Observed line above expected line signifies clustering of points
#All three plots indicate clustering likely due to sampling bias

#Map of observations by family: 
fw.obs.marked.pts <- st_as_sf(fw.obs.marked, 
                              coords = c("Longitude", "Latitude"), 
                              crs = 4269)

ranidae.obs <- fw.obs.marked.pts %>%
  filter(., Family == 3)
ranidae.map <- 
  tm_shape(ne.oh.counties) + tm_borders() +
  tm_shape(ranidae.obs) + tm_dots("blue") +
  tm_title("Ranidae FrogWatch Observations in NE Ohio") +
  tm_compass() + tm_scalebar()
ranidae.map

bufonidae.obs <- fw.obs.marked.pts %>%
  filter(., Family == 2)
bufonidae.map <-
  tm_shape(ne.oh.counties) + tm_borders() +
  tm_shape(bufonidae.obs) + tm_dots("brown") +
  tm_title("Bufonidae FrogWatch Observations in NE Ohio") +
  tm_compass() + tm_scalebar()
bufonidae.map

hylidae.obs <- fw.obs.marked.pts %>%
  filter(., Family == 1)
hylidae.map <-
  tm_shape(ne.oh.counties) + tm_borders() +
  tm_shape(hylidae.obs) + tm_dots("green") +
  tm_title("Hylidae FrogWatch Observations in NE Ohio") +
  tm_compass() + tm_scalebar()
hylidae.map

family.maps <-
  tmap_arrange(ranidae.map, bufonidae.map, hylidae.map, nrow = 1)
family.maps
#All three maps indicate clustering due to sampling bias confirmed
#Need to pivot to statistical rather than spatial analysis

#Perform Chi Square Test for correlations between species and habitat: 
fw.chisq.test <- chisq.test(fw.obs.oh.ne.hab$Species, 
                            fw.obs.oh.ne.hab$`Wetland Habitat`)
print(fw.chisq.test)
fw.chisq.test$observed
fw.chisq.test$expected
fw.chisq.test$residuals
fw.chisq.test$stdres

#Create a mosaic plot to visualize results of the Chi Square Test:
contingency.table <- table(fw.obs.oh.ne.hab$Species, 
                           fw.obs.oh.ne.hab$`Wetland Habitat`)
contingency.table
fw.mosaic <- mosaicplot(contingency.table, shade = TRUE, type = "pearson",
                        main = "Breeding Habitat Usage of NE Ohio Frog Species",
                        xlab = "Species", ylab = "Wetland Type", las = 2)
#Blue indicates positive association, red represents negative association