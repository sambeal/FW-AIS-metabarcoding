# Load Packages ----
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(rnaturalearth)
library(ggspatial)

# Turn off s2 to avoid geometry issues
sf_use_s2(FALSE)

# Load Your Data ----
# Sample coordinates (from your CSV)
coords <- read.csv("../NS fishes docs/Alexander et al. 1986_appendix_clean.csv", header = TRUE)
coords_sf <- st_as_sf(coords, coords = c("Longitude", "Latitude"), crs = 4326)

# Nova Scotia boundary
ns_map <- ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name == "Nova Scotia")
ns_map <- st_transform(ns_map, crs = 4326)  # Ensure same CRS as your points

# Load shapefiles from NS Mapping directory inside PhD directory
shp_wbody <- st_read("../../NS Mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/waterbody_2.shp") |> st_zm()
shp_wcourse <- st_read("../../NS Mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/watercourse_1.shp") |> st_zm()
shp_wshed <- st_read("../../NS Mapping/Data/NS_Watersheds_10k/PRIMARY_POLY_CSRS_UTM_Z20.shp") |> st_zm()

# Reproject everything to WGS84 (for compatibility with coords_sf and ns_map)
shp_wbody <- st_transform(shp_wbody, crs = 4326)
shp_wshed <- st_transform(shp_wshed, crs = 4326)

# Subset lakes only
shp_wbody_lakes <- subset(shp_wbody, definit_en == "Lake")

# Crop lakes and watersheds to NS boundary
shp_wbody_lakes_ns <- st_intersection(shp_wbody_lakes, ns_map)
shp_wshed_ns <- st_intersection(shp_wshed, ns_map)

# Make sure FLOW_DIR is factor
shp_wshed_ns$FLOW_DIR <- as.factor(shp_wshed_ns$FLOW_DIR)

# Final Plot ----
ggplot() +
  # NS map outline
  geom_sf(data = ns_map, fill = "white", color = "black") +
  
  # Watersheds by flow direction
  geom_sf(data = shp_wshed_ns, color = "black", alpha = 0.4, size = 0.3) +
  
  # Lakes
  geom_sf(data = shp_wbody_lakes_ns, fill = "deepskyblue3", color = "deepskyblue4", size = 0.25, alpha = 0.7) +
  
  #  sample sites
  geom_sf(data = coords_sf, size = 2) +
  
  # Axis labels and legend
  labs(fill = "Flow Direction", x = "Longitude", y = "Latitude") +
  
  # Theme
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA)
  )


#########

# Load Your Data ----
# Sample coordinates (from your CSV)
alex_locations <- read.csv("../NS fishes docs/Alexander et al. 1986_appendix_clean.csv", header = TRUE)

species_codes <- read.csv("../NS fishes docs/Alexander_1986_species_codes.csv", header = TRUE)

library(dplyr)
library(tidyr)
library(stringr)

# Step 1: Split the 'Fish_Species' column into individual codes
alex_locations_long <- alex_locations %>%
  mutate(Fish_Species = str_split(Fish_Species, ",")) %>%      # split comma-separated values
  unnest(Fish_Species) %>%
  mutate(Fish_Species = as.numeric(str_trim(Fish_Species)))    # trim spaces and convert to numeric

# Step 2: Join with species_codes to get common & scientific names
alex_fish <- alex_locations_long %>%
  left_join(species_codes, by = c("Fish_Species" = "Species_code")) %>%
  select(-Fish_Species)  # optionally remove code column

# Step 3: Clean up names
alex_fish <- alex_fish %>%
  mutate(Common_name = str_to_title(Common_name))
