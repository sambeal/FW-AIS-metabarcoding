# plot biodiversity results from NS Gov Fish Survey Excel Doc

# Load Packages ----
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(terra)
library(tidyterra)
library(geodata)
library(ggspatial)

# Load the fish data ----
fishes_plotting <- read.csv("Output/NS_gov_fish_survey_plotting_fishcounts_reformat.csv")

# Convert the data to an sf object (ensure coordinates are in the right order: LONGITUDE first, LATITUDE second)
fishes_sf <- st_as_sf(fishes_plotting, coords = c("LONGITUDE_DEC","LATITUDE_DEC"), crs = 4326)  # WGS 84


# Load Map Data ----
# NOTE: uses shape files downloaded from NRCAN
# http://ftp.maps.canada.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
# AND
# watershed polygon from Nova Scotia Data
# https://data.novascotia.ca/browse?tags=nswatersheds

shp_wbody <- sf::st_read("../../NS mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/waterbody_2.shp")
shp_wcourse <- sf::st_read("../../NS mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/watercourse_1.shp")
shp_wshed <- sf::st_read("../../NS mapping/Data/NS_Watersheds_10k/TERTIARY_POLY_CSRS_UTM_Z20_E.shp")


# --- Convert Data (to keep it 2D) ---
# Remove z and m dimensions
shp_wbody_nozm <- st_zm(shp_wbody)
shpgeo <- st_geometry(shp_wbody_nozm)

shp_wcourse_nozm <- st_zm(shp_wcourse)
shp_wcoursegeo <- st_geometry(shp_wcourse_nozm)

shp_wshed_nozm <- st_zm(shp_wshed)
shp_wshedgeo <- st_geometry(shp_wshed_nozm)


# Subset Lakes (not as interested in rivers or streams right now) ----
shp_wbody_lakes <- subset(shp_wbody_nozm, definit_en == "Lake")
shpgeo_lakes_nozm <- st_geometry(shp_wbody_lakes)


# Set World Object ----
world <- ne_countries(scale = "large", continent = 'north america', returnclass = "sf")

NorthAmerica <- gadm(country = country_codes("North America")$ISO3,
                     level = 0, resolution = 2,
                     path = getwd())


# Convert SpatVector (NorthAmerica) to sf object ----
NorthAmerica_sf <- sf::st_as_sf(NorthAmerica)


# Ensure both shapefiles have the same CRS ----
#north_america_crs <- st_crs(NorthAmerica_sf) # Check the CRS of NorthAmerica: World Geodetic System 1984
#lakes_crs <- st_crs(shpgeo_lakes_nozm)    # Check the CRS of your lakes data: GCS_North_American_1983_CSRS98


# Convert NA to match lakes ----
NorthAmerica_sf <- st_transform(NorthAmerica_sf, crs = lakes_crs)


# Plot with aligned CRS and zoom into Nova Scotia using coord_sf ----
# error in sf package
# turn off s2 to create map
sf_use_s2(FALSE)

ns_lakes_map <- ggplot() +
  # Plot North America background
  geom_sf(data = NorthAmerica_sf, fill = "gray85", color = "black") +
  # Adds lakes with outline
  geom_sf(data = shpgeo_lakes_nozm, 
          color = "deepskyblue4", fill = "deepskyblue3", size = 0.25, alpha = 0.7) +
  # Set lat and long limits using coord_sf() to zoom in
  coord_sf(xlim = c(-66.7, -59.5), ylim = c(43.2, 47)) +
  # Labels and adjustments
  labs(title = "Lakes of Nova Scotia", 
       x = "Longitude", 
       y = "Latitude") +
  # Format chart area
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)) +
  #add fish diversity data points
  geom_point(data = fishes_plotting, 
             aes(x = LONGITUDE_DEC, y = LATITUDE_DEC, 
                 fill = Unique_Species_Count),  # Fill points based on Unique_Species_Count
             color = "black",  # Outline color (black)
             size = 2, 
             alpha = 0.7,      # Adjust the transparency of the fill
             shape = 21) +     # Shape 21 supports both fill and outline
  scale_fill_gradient(low = "yellow", high = "red",
                      breaks = seq(0, max(fishes_plotting$Unique_Species_Count, na.rm = TRUE), by = 3)) +  # Legend breaks every 3 units
  labs(title = "Lakes surveyed in NS Gov Excel Sheet (1967-91)", 
       x = "Longitude", 
       y = "Latitude",
       color = "Number of Fish Species Detected per Lake") 

# Print the map
print(ns_lakes_map)



