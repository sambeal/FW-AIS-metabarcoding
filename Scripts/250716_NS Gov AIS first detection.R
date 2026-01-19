# plot biodiversity results from NS Gov Fish Survey Excel Doc

############ set up ############ 
# Load Packages ----
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(terra)
library(tidyterra)
library(geodata)
library(ggspatial)



############ Load the data ############ 
fishes <- read.csv("Output/NS_gov_fish_survey_plotting_fishcounts_reformat.csv", header = TRUE)

# remove column 1 -- should have saved with row.name but didn't
fishes <- fishes[-c(1)]

#subset into AIS
ais_plotting <- subset(fishes, fishes$Species.Detection %in% c("Smallmouth Bass", "Chain Pickerel"))



############ get assessment dates ############ 
# Convert the character date to a Date object
ais_plotting$Date <- as.Date(ais_plotting$ASSESSMENT.DATE, format = "%d-%b-%y")

# Extract year from the Date object
ais_plotting$Year <- as.numeric(format(ais_plotting$Date, "%Y"))

# Correct for two-digit years (if needed)
# This step ensures dates like '86' are interpreted as 1986, not 2086
ais_plotting$Year <- ifelse(ais_plotting$Year > 2025, ais_plotting$Year - 100, ais_plotting$Year)

# Now create the Decade column
ais_plotting$Decade <- floor(ais_plotting$Year / 10) * 10


# First detection per lake-species combo
first_detect <- ais_plotting %>%
  group_by(LAKE.NAME, SITE.CODE, Species.Detection) %>%
  summarise(First_Year = min(Year, na.rm = TRUE)) %>%
  mutate(Decade_of_First_Detection = floor(First_Year / 10) * 10)


# Join the decade info back to the main AIS plotting data
ais_plotting <- ais_plotting %>%
  left_join(first_detect, by = c("LAKE.NAME", "SITE.CODE", "Species.Detection"))

# Make sure decade is a factor
ais_plotting$Decade_of_First_Detection <- factor(ais_plotting$Decade_of_First_Detection)

# Convert the data to an sf object (ensure coordinates are in the right order: LONGITUDE first, LATITUDE second)
ais_sf <- st_as_sf(ais_plotting, coords = c("LONGITUDE_DEC","LATITUDE_DEC"), crs = 4326)  # WGS 84


############ Load Map Data ############ 
# NOTE: uses shape files downloaded from NRCAN
# http://ftp.maps.canada.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
# AND
# watershed polygon from Nova Scotia Data
# https://data.novascotia.ca/browse?tags=nswatersheds

shp_wbody <- sf::st_read("../../NS mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/waterbody_2.shp")
#shp_wcourse <- sf::st_read("../../NS mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/watercourse_1.shp")
shp_wshed <- sf::st_read("../../NS mapping/Data/NS_Watersheds_10k/TERTIARY_POLY_CSRS_UTM_Z20_E.shp")


# Subset Lakes (not as interested in rivers or streams right now) ----
shp_wbody_lakes <- subset(shp_wbody, definit_en == "Lake")
shpgeo_lakes <- st_geometry(shp_wbody_lakes)


# Set World Object ----
world <- ne_countries(scale = "large", continent = 'north america', returnclass = "sf")

NorthAmerica <- gadm(country = country_codes("North America")$ISO3,
                     level = 0, resolution = 2,
                     path = getwd())


# Convert SpatVector (NorthAmerica) to sf object ----
NorthAmerica_sf <- sf::st_as_sf(NorthAmerica)


# Ensure both shapefiles have the same CRS ----
north_america_crs <- st_crs(NorthAmerica_sf) # Check the CRS of NorthAmerica: World Geodetic System 1984
lakes_crs <- st_crs(shpgeo_lakes)    # Check the CRS of your lakes data: GCS_North_American_1983_CSRS98
wshed_crs <- st_geometry(shpgeo_wshed)

# Convert NA to match lakes ----
NorthAmerica_sf <- st_transform(NorthAmerica_sf, crs = lakes_crs)


# Plot with aligned CRS and zoom into Nova Scotia using coord_sf ----
# error in sf package
# turn off s2 to create map
sf_use_s2(FALSE)


############ Plot ############ 
# Plot 1 - points of first AIS invasion over lakes

ns_lakes_map <- ggplot() +
  # Plot North America background
  geom_sf(data = NorthAmerica_sf, fill = "gray85", color = "black") +
  # Adds lakes with outline
  geom_sf(data = shpgeo_lakes, color = "deepskyblue4", fill = "deepskyblue3", size = 0.25, alpha = 0.7) +
  # Set lat and long limits using coord_sf() to zoom in
  coord_sf(xlim = c(-66.7, -59.5), ylim = c(43.2, 47)) +
  # Format chart area
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)) +
  # Add fish diversity data points
  geom_point(data = ais_plotting,
             aes(x = LONGITUDE_DEC, y = LATITUDE_DEC,
                 fill = Decade_of_First_Detection,
                 #size = Unique_Species_Count
                 ),
             color = "black",
             shape = 21,
            # alpha = 0.8
            ) +
  # Colour by first detection of AIS (per decade)
  scale_fill_viridis_d() +
  # Size by species richness
  #scale_size_continuous(range = c(1, 5), name = "Unique Species Count") +
  # Facet by AIS
  facet_wrap(~Species.Detection) +
  labs(title = "First AIS Detection by Decade in Nova Scotia Lakes",
       subtitle = "Data from NS Gov Fish Database (1945–2011)",
       x = "Longitude", y = "Latitude")

# Print the map
print(ns_lakes_map)

######
# Plot 2 - points of first AIS invasion over watershed

shpgeo_shp_wshed <- st_geometry(shp_wshed)


######
ais_sf <- st_transform(ais_sf, crs = st_crs(shp_wbody_lakes))

ais_with_lake <- st_join(ais_sf, shp_wbody_lakes, join = st_intersects, left = FALSE)



