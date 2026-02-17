#Step 1: Load packages and data

library(tidyverse)
library(ggplot2)
library(sf)

p.counties <- "./County_Boundaries.shp"
p.stations <- "./Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"

d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)

glimpse(d.counties)
glimpse(d.stations)

#check for validity

d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid()
d.counties <- d.counties %>% sf::st_make_valid()

#dplyr select verb

d.counties %>% dplyr::select(GEOID10, ALAND10) %>% head()
d.counties %>% dplyr::select(-NAME10) %>% head()
d.counties %>% dplyr::select(GEOID10:CLASSFP10) %>% head()
d.counties %>% dplyr::select(-(GEOID10:CLASSFP10)) %>% head()
d.counties %>% dplyr::select(starts_with("C"))

#Step 2: Grouping data

d.counties %>% group_by(STATEFP10) %>% mutate(stateLandArea = sum(ALAND10))
d.counties %>% 
  as_tibble() %>% dplyr::select(-geometry) %>%
  group_by(STATEFP10) %>%
  summarise(stateLandArea = sum(ALAND10))

#Step 3: Plotting data

d.counties %>%
  ggplot(., aes(x = as.factor(STATEFP10), y = ALAND10)) +
  geom_boxplot(aes(fill = STATEFP10))

d.counties %>%
  ggplot(., aes(x = ALAND10)) +
  geom_histogram(aes(fill = STATEFP10)) +
  labs(title = "not the most useful plot but you get the idea")

#Step 4: Spatial operations

d.counties %>% sf::st_crs()
d.stations %>% sf::st_crs()
d.counties %>% sf::st_crs() == d.stations %>% sf::st_crs()

del.counties <- d.counties %>% dplyr::filter(STATEFP10 == 10)
del.stations <- sf::st_intersection(d.stations, del.counties)
glimpse(del.stations)
plot(del.stations)

del.counties %>% st_area()

#Step 5: Tasks
#Task 1: Basic data manipulation
#1.1: For each county, calculate its land area as percentage of the total area (land + water) for that state.

glimpse(d.counties)
d.counties <- d.counties %>%
  group_by(STATEFP10) %>%
  mutate(stateTotalArea = sum(ALAND10 + AWATER10))
view(d.counties)
d.counties <- d.counties %>% 
  mutate(countyLandPerc = (ALAND10 / stateTotalArea))
view(d.counties)

#1.2: For each state, find the county that has the largest proportion of its land as water (water area / total area)

d.counties <- d.counties %>%
  mutate(countyWaterPerc = (AWATER10 / (AWATER10 + ALAND10)))
view(d.counties)       
d.counties %>%
  group_by(STATEFP10) %>% 
  slice_max(countyWaterPerc)

#1.3: Count the number of counties in each state

library(tidyverse)
library(sf)
d.counties %>%
  count(STATEFP10)

#1.4: Which station has the shortest name in the study area?

view(d.stations)
d.stations <- d.stations %>%
  mutate(char_count = nchar(STATION_NA))
d.stations %>%
  slice_min(char_count)

#Task 2: Plotting Attribute Data (for each plot, label axes and provide a title)
#2.1: Make a scatterplot showing the relationship between land area and water area for each county. Color each point using the state variable

library(ggplot2)
d.counties %>%
  ggplot(., aes(x = ALAND10, y = AWATER10, color = STATEFP10)) +
  geom_point() +
  theme_classic() +
  labs(title = "Land & Water Area Relationship in CBW Counties", x = "Land Area", y = "Water Area", color = "State")

#2.2: Make a histogram of drainage area (Drainage_A) for all monitoring stations

d.stations %>%
  ggplot(., aes(x = Drainage_A)) +
  geom_histogram(binwidth = 1000) +
  theme_classic() +
  labs(title = "Drainage Area of CWB Monitoring Stations", x = "Drainage Area", y = "Count")

#2.3: Make a similar histogram of drainage area (Drainage_A) for all monitoring stations. This time, shade/color each portion of the histogram’s bar(s) using the state variable

library(stringr)
d.stations <- d.stations %>%
  mutate(station_state = str_sub(STATION_NA,-2, -1))
view(d.stations)
d.stations[95, "station_state"] <- "PA"
d.stations %>%
  ggplot(., aes(x = Drainage_A, fill = station_state)) +
  geom_histogram(binwidth = 1000) +
  theme_classic() +
  labs(title = "Drainage Area of CWB Monitoring Stations", x = "Drainage Area", y = "Count", fill = "State")

#Task 3: Write a Function
#3.1.A: accepts a vector of arbitrary numbers, calculates the mean, median, maximum, and minimum of the vector

f.3.1 <- function(x) {
  vector <- sample(x)
  f.3.1.A <- summary(vector)
}

#3.1.B: Sorts the vector

f.3.1 <- function(x) {
  vector <- sample(x)
  f.3.1.A <- summary(vector)
  f.3.1.B <- sort(vector)
}

#3.1.C: returns a list of those values from A and the sorted vector from B

f.3.1 <- function(x) {
  vector <- sample(x)
  f.3.1.A <- summary(vector)
  f.3.1.B <- sort(vector)
  f.3.1.C <- cat(f.3.1.A, f.3.1.B)
}

#3.1.D: the function should only work with numeric values and print an error message if any other data type are found
#Test it with the following vectors

f.3.1 <- function(x) {
  vector <- sample(x)
  if(is.numeric(vector)){
    f.3.1.A <- summary(vector)
    f.3.1.B <- sort(vector)
    f.3.1.C <- cat(f.3.1.A, f.3.1.B)}
  else{
    stop()
  }
}

vector_1 <- c(1, 0, -1)
test_1 <- f.3.1(vector_1)

vector_2 <- c(10, 1000, 100)
test_2 <- f.3.1(vector_2)

vector_3 <- c(.1, .001, 1e8)
test_3 <- f.3.1(vector_3)

vector_4 <- c("a", "b", "c")
test_4 <- f.3.1(vector_4)

#Task 4: A (slightly) more complex spatial analysis.
#4.1: Calculate the number of monitoring stations in each state

d.stations %>%
  count(station_state)

#4.2: Calculate the average size of counties in New York (that are also in this study area)

d.counties %>%
  filter(STATEFP10 == 36) %>%
  summarise(avg_size_NY = mean(ALAND10))

#4.3: Calculate which state has monitoring stations with the greatest average drainage area (Drainage_A)

d.stations %>%
  group_by(station_state) %>%
  summarise(avg_drainage_a = mean(Drainage_A)) %>%
  slice_max(avg_drainage_a)

#Questions

#Q1: In using the intersection functions, are the following two statements equivalent?
#sf::st_intersection(d.stations, del.counties), sf::st_intersection(del.counties, d.stations)
#A1: This intersection is between a set of polygons (counties) and a set of points (stations) so the order of the variables within the function should not matter geometrically when performing the intersection. However, the variables contain different attributes and different geometries, so their order in the function will change the output of the intersection. For example, the attributes of the first argument will be referenced first, then the second argument will be overlaid and some of its attributes can be lost if they conflict with the first argument. Likewise, the geometry of the first argument determines the geometry of the output, so if the first variable has point geometry then the output will too (or vice versa). The same is true if you try to perform an intersection with other types of spatial data like lines and polygons.

#Q2: What did you find challenging in this lab? What was new?
#A2: I felt like this lab had a steep learning curve at the beginning due to my lack of programming experience. For instance, things that should have been easy like loading in the data took me awhile, but once I got familiar with the structure of the data and the syntax of the code, I was able to work my way through the tasks without too much difficulty. In the end, it was satisfying to solve a lot of these problems. 

#Q3: What types of activities would you like to see in labs this semester?
#A3: There is not any particular type of analysis I am looking to learn this semester because I am new to R and trying to become familiar with the different types of spatial analysis that I can potentially apply to my research. I want to gain experience working with big/complex data, especially related to water quality (which we have already started) and ideally data related to wildlife movement/habitat use as well. 
