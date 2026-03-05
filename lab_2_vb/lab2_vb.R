# Spatial Analysis in R: Lab 2
# Name: Vigo Bertolo

# setup
library(tidyverse)
library(tmap)
library(sf)

# table of names
t.names <- tibble(key = c(1,2,3),
                  name = c("Huey", "Dewey", "Louis"))

# table of scores
t.scores <- tibble(name = c("Louis","Huey","Dewey"),
                   grade = c(99,45,33))

# join them together as a common variable
t.joined <- left_join(t.names, t.scores, by = "name")
t.joined

t.wonkyNames <- tibble(nombre = c("Dewey", "Louis", "Huey"),
                       x = rep(999),
                       favoriteFood = c("banana", "apple", "carrot"))
t.joined2 <- left_join(t.names, t.wonkyNames, by = c("name" = "nombre"))
t.joined2

# load data
bmps <- read_csv("./CBW/BMPreport2016_landbmps.csv")
glimpse(bmps)

# edit the bmps variable in place
bmps <- bmps %>%mutate(., FIPS.trimmed = stringr::str_sub(GeographyName, 1,5))

# calculate the total cost by BMP and then plot it
bmps %>% group_by(BMPType) %>% summarise(totalCost = sum(Cost)) %>%
  ggplot(., aes(x = BMPType, y = totalCost)) +
  geom_bar(stat = "identity") +
  theme_minimal()
summary(bmps$Cost)
bmps %>% group_by(BMPType) %>% summarise(totalCost = sum(Cost, na.rm = T)) %>%
  ggplot(., aes(x = BMPType, y = totalCost)) +
  geom_bar(stat = "identity") +
  theme_minimal()

# group by state and sector, sum total cost
twofactors <- bmps %>% group_by(StateAbbreviation, Sector) %>%
  summarise(totalCost = sum(Cost))
twofactors

# box plots
bmps %>% ggplot(., aes(x = StateAbbreviation, y = AmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation))

bmps %>%
  dplyr::filter(., AmountCredited > 1 & AmountCredited < 100) %>%
  ggplot(., aes(x = StateAbbreviation, y = AmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation))

bmps %>%
  dplyr::filter(., AmountCredited > 1 & AmountCredited < 100) %>%
  ggplot(., aes(x = StateAbbreviation, y = AmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation)) +
  facet_grid(Sector~.)

bmps %>% ggplot(., aes(x = StateAbbreviation, y = AmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation)) +
  scale_y_log10() +
  labs(y = "log10(AmountCredited)")

# demonstration vector
x <- c(1,2,3,4,5)
7 %in% x
2 %in% x
c(4,99,1) %in% x

# quick map
counties <- sf::read_sf("./CBW/County_Boundaries.shp")
counties %>% sf::st_is_valid()
counties <- counties %>% sf::st_make_valid()

tm_shape(counties) + tm_polygons(fill = "ALAND10")

# load spatial data
counties <- sf::read_sf("./CBW/County_Boundaries.shp") %>%
  sf::st_make_valid()
dams <- sf::read_sf("./CBW/Dam_or_Other_Blockage_Removed_2012_2017.shp") %>%
  sf::st_make_valid()
streams <- sf::read_sf("./CBW/Streams_Opened_by_Dam_Removal_2012_2017.shp") %>%
  sf::st_make_valid()

# load aspatial data
bmps <- read_csv("./CBW/BMPreport2016_landbmps.csv")

#Task 1: Aspatial operations
#1.1
bmps.cost <- bmps %>% group_by(StateAbbreviation) %>%
  summarise(mean(Cost, na.rm = TRUE),
            median(Cost, na.rm = TRUE),
            sd(Cost, na.rm = TRUE),
            min(Cost, na.rm = TRUE),
            max(Cost, na.rm = TRUE), 
            IQR(Cost, na.rm = TRUE))
glimpse(bmps.cost)

#1.2
bmps.acres <- bmps %>% filter(Unit == "Acres")
glimpse(bmps.acres)
bmps.acres %>% ggplot(., aes(x = Cost, y = TotalAmountCredited)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() + 
  labs(title = "Total Amount Credited for BMPs relative to Cost in the CWB",
       x = "log10(Cost)", 
       y = "log10(Total Amount Credited)") +
  theme_classic()

#1.3 complete
unique(bmps$BMP)
bmps.cc <- bmps %>% filter(str_detect(BMP, "Cover Crop"))
unique(bmps.cc$BMP)

bmps.cc %>% ggplot(., aes(x = StateAbbreviation, y = TotalAmountCredited)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Total Amount Credited for Cover Crop BMPs by State in the CWB",
       x = "State", 
       y = "log10(Total Amount Credited)") +
  theme_classic()

#1.4
glimpse(dams)
dams.year <- dams %>% filter(YEAR != 0)
glimpse(dams.year)

dams.year %>% ggplot(., aes(x = YEAR, y = STATE)) +
  geom_point() + 
  labs(title = "Timeline of Dam Construction in the CWB by State") +
  theme_classic() 

#1.5
bmps.geoid <- bmps %>%
  mutate(GEOID10 = str_remove_all(GeographyName, "\\(cbwsonly\\)"))

county.bmps <- left_join(counties, bmps.geoid, by = "GEOID10")
glimpse(county.bmps)

county.bmps %>% ggplot(., aes(x = ALAND10, y = AmountCredited)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Amount Credited for BMPs relative to Land Area of Counties in the CWB",
       x = "County Land Area",
       y = "Amount Credited") +
  theme_classic()

#Task 2: Spatial operations
#2.1
longest.streams <- streams %>%
  slice_max(., order_by = LengthKM, n = 5)
longest.streams$GNIS_Name

#2.2
st_crs(counties) == st_crs(streams)

streams.counties <- st_join(streams, counties, join = st_within)
glimpse(streams.counties)

county.stream.lengths <- streams.counties %>% 
  mutate(StreamLength = st_length(.))
glimpse(county.stream.lengths)

county.streams.total.length <- county.stream.lengths %>%
  group_by(NAME10) %>%
  summarise(TotalStreamLength = sum(StreamLength))
glimpse(county.streams.total.length)

top3.counties <- county.streams.total.length %>%
  slice_max(., order_by = TotalStreamLength, n = 3)
top3.counties

#2.3: need to check on more powerful computer
bmps.geoid <- bmps %>%
  mutate(GEOID10 = str_remove_all(GeographyName, "\\(cbwsonly\\)"))

county.bmps <- left_join(counties, bmps.geoid, by = "GEOID10")

county.bmps.cost <- county.bmps %>%
  group_by(GEOID10) %>%
  summarise(totalCost = sum(Cost, na.rm = TRUE))

tm_shape(county.bmps.cost) + 
  tm_polygons(fill = "totalCost")

#2.4
dams.streams <- st_nearest_feature(dams, streams)
closest.streams <- streams[dams.streams,]
closest.streams$GNIS_Name #answer

dam.stream.distance <- st_distance(dams, closest.streams, by_element = TRUE)
dam.stream.distance #verification of proximity between dams and streams

#2.5
dams %>%
  group_by(STATE) %>%
  count(.)
