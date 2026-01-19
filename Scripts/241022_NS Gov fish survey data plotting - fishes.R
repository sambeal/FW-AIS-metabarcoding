# plot lakes from NS Lake Survey Program

# Load necessary libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)

# Step 1: Load the data
fishes <- read.csv("Data/Distribution of Nova Scotia Fishes_fishes_decimaldegree_for_plotting.csv")

# There are reused Lake names around the province. 
# Some lakes share the same watershed code.
# Some lakes have multiple site codes 

# Step 2: Count unique species for each lake
fishes_with_counts <- fishes %>%
  group_by(COORDINATES.LATITUDE..DEGREES..MINUTES. , COORDINATES.LONGITUDE..DEGREES..MINUTES. ) %>%
  summarise(Unique_Species_Count = n_distinct(Species.Detection, na.rm = TRUE), .groups = 'drop')

# Step 3: Join the count back to the original dataframe
fishes_plotting <- fishes %>%
  left_join(fishes_with_counts, 
            by = c("COORDINATES.LATITUDE..DEGREES..MINUTES." = "COORDINATES.LATITUDE..DEGREES..MINUTES.", 
                   "COORDINATES.LONGITUDE..DEGREES..MINUTES." = "COORDINATES.LONGITUDE..DEGREES..MINUTES."))


# View the updated dataframe
View(fishes_plotting)

#save output
write.csv(fishes_plotting, "Output/NS_gov_fish_survey_plotting_fishcounts_reformat.csv")

# Step 2: Convert the data to an sf object (ensure coordinates are in the right order: LONGITUDE first, LATITUDE second)
fishes_sf <- st_as_sf(fishes_plotting, coords = c("LONGITUDE_DEC","LATITUDE_DEC"), crs = 4326)  # WGS 84

# Get high-resolution map of Nova Scotia
ns_map <- ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name == "Nova Scotia")

# Optionally, transform the map projection to match your data's UTM Zone
ns_map_utm <- st_transform(ns_map, crs = 4326)


# Plot with ggplot
ggplot() +
  geom_sf(data = ns_map_utm, fill = "white", color = "black") +  # Nova Scotia background map
  geom_sf(data = fishes_sf, aes(color = Unique_Species_Count), size = 2) +  
  labs(title = "Lakes surveyed in NS Gov Excel Sheet (1967-91)", 
       x = "Longitude", 
       y = "Latitude",
       color = "Number of Fish Species Detected per Lake") + 
  scale_color_viridis_c() + # For a nice color scale
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)) 

