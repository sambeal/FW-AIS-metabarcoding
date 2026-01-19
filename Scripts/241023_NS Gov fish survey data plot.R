# plot biodiversity results from NS Gov Fish Survey Excel Doc




# Load necessary libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)

# Step 1: Load the data
fishes_plotting <- read.csv("Data/Distribution of Nova Scotia Fishes_decimal_degree_cords_for_plotting.csv")

# Step 2: Convert the data to an sf object (ensure coordinates are in the right order: LONGITUDE first, LATITUDE second)
fishes_sf <- st_as_sf(fishes_plotting, coords = c("LONGITUDE_DEC","LATITUDE_DEC"), crs = 4326)  # WGS 84

# Get high-resolution map of Nova Scotia
ns_map <- ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name == "Nova Scotia")

# Optionally, transform the map projection to match your data's UTM Zone
ns_map_utm <- st_transform(ns_map, crs = 4326)

# Step 3: Plot the data
ggplot() +
  geom_sf(data = ns_map_utm, fill = "white", color = "black") +  # Nova Scotia background map
  geom_sf(data = fishes_sf, aes(color = COUNTY), size = 2) + # Plot the lake points
  labs(title = "Lakes surveyed in NS Gov excel sheet", 
       x = "Longitude", 
       y = "Latitude") + 
  theme_minimal()
