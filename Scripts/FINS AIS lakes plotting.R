################################################################################
# Exploration of FINS lake data set to choose lakes for Ch. 3 - FW survey of NS
# to assess the impacts of AIS on natural FW fish biodiversity

# have subsetted the data set into lakes with AIS detection

# will plot on NS map to show the distribution, coloured by year of detection

# firstly on a county basis, then a watershed basis
################################################################################

# Load Packages ----
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)


############ Load the data ############ 
ais <- read.csv("Analysis/AIS_lakes.csv", header = TRUE)


############ Preparing AIS data ############ 
# Convert AIS to sf using Easting/Northing
# Replace 26920 if your CRS is different (this is NAD83 / UTM zone 20N, common in NS)
ais_sf <- st_as_sf(ais, coords = c("Easting", "Northing"), crs = 26920)

# Transform to WGS84 (lat/long)
ais_latlong <- st_transform(ais_sf, crs = 4326)

# Extract numeric columns for ggplot
ais_latlong <- ais_latlong %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  )

# Create Decade column
ais_latlong <- ais_latlong %>%
  mutate(
    Decade_num = floor(Year / 10) * 10,                      # e.g., 1987 -> 1980
    Decade = factor(paste0(Decade_num, "s"),                 # e.g., 1980 -> "1980s"
                    levels = sort(unique(paste0(Decade_num, "s"))))
  )

# Ensure Decade is a factor with levels in chronological order
ais_latlong <- ais_latlong %>%
  mutate(Decade = factor(Decade, levels = sort(unique(Decade))))

# Split by species
bass_sf <- ais_latlong %>% filter(Species.Name == "Smallmouth Bass")
pickerel_sf <- ais_latlong %>% filter(Species.Name == "Chain Pickerel")



############ Load Map Data ############ 
# Lakes (subset from all waterbodies)
shp_wbody <- st_read("../../NS mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/waterbody_2.shp")
shp_lakes <- subset(shp_wbody, definit_en == "Lake")
# CRN: GCS_North_American_1983_CSRS98

# Primary watersheds
shp_wshed <- st_read("../../NS mapping/Data/NS_Watersheds_10k/PRIMARY_POLY_CSRS_UTM_Z20.shp")
# Projected CRS: NAD83(CSRS) / UTM zone 20N

# County boundaries
shp_county <- st_read("../../NS mapping/Data/Nova Scotia Topographic Database - County Boundaries/geo_export_c2cb38d4-df79-418a-99e4-b970bb96d87c.shp")
# Geodetic CRS:  WGS84(DD)

# Transform all to WGS84
crs_wgs <- 4326
shp_lakes <- st_transform(shp_lakes, crs = crs_wgs)
shp_wshed <- st_transform(shp_wshed, crs = crs_wgs)
shp_county <- st_transform(shp_county, crs = crs_wgs)

# Load North America & subset Nova Scotia 
NorthAmerica <- gadm(country = country_codes("Canada")$ISO3, level = 1, path = getwd())
NorthAmerica_sf <- st_as_sf(NorthAmerica)
NS_sf <- NorthAmerica_sf %>% filter(NAME_1 == "Nova Scotia")

# Transform NS map to match points CRS
NS_sf <- st_transform(NS_sf, crs = crs_wgs)

# Avoid geometry errors
sf_use_s2(FALSE)


############ Plot 1 - Lakes surveys ############
library(sf)
library(dplyr)

# Ensure both layers are in the same CRS
ais_latlong <- st_transform(ais_latlong, st_crs(shp_lakes))

# Perform a spatial join: attach lake polygons to AIS points
fins_lakes <- st_join(ais_latlong, shp_lakes, join = st_within)

# Now fins_lakes has all columns from AIS points plus the matching lake polygon columns
# For plotting, you can highlight only the lakes that have AIS
fins_lakes_with_ais <- shp_lakes[st_intersects(shp_lakes, ais_latlong, sparse = FALSE)[,1], ]

# Plotting 
ggplot() +
  geom_sf(data = NS_sf, fill = NA, color = "black") +
  # Optional: show all lakes as outlines
  geom_sf(data = shp_lakes, fill = NA, color = "darkgrey", size = 0.25, alpha = 0.3) +
  # Only colour the lakes in FINS dataset
  geom_sf(data = fins_lakes, fill = "deepskyblue3", color = "deepskyblue4", size = 0.25, alpha = 0.7) +
  # Add county lines
  geom_sf(data = shp_county, fill = NA, color = "black", size = 0.1) +
  # Add watershed lines
  geom_sf(data = shp_wshed, fill = NA, color = "lightgrey", size = 0.25, alpha = 0.3) +
  # Scale map
  coord_sf(xlim = c(-66.7, -59.5), 
           ylim = c(43.2, 47)) +
  theme_classic() +
  labs(title = "Lakes in FINS database",
       x = "Longitude",
       y = "Latitude")


ggsave("Lakes in FINS.pdf",
       plot = last_plot(),
       device = "pdf",
       path = "/Users/bethwatson/Documents/eDNA/maps/",
       scale = 1,
       width = 2000,
       height = 1600,
       units = c("px"),
       dpi = 300,
       limitsize = TRUE,
       bg = NULL)
############ Plot 1 - Smallmouth Bass ############
ggplot() +
  geom_sf(data = NS_sf, fill = NA, color = "black") +
  geom_sf(data = shp_lakes, color = "deepskyblue4", fill = "deepskyblue3", size = 0.25, alpha = 0.7) +
  geom_sf(data = shp_county, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = shp_wshed, fill = NA, color = "darkgreen", size = 0.25, alpha = 0.3) +
  geom_sf(data = bass_sf, aes(fill = Decade), shape = 21, color = "black", size = 2) +
  coord_sf(xlim = c(-66.7, -59.5), ylim = c(43.2, 47)) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  labs(title = "First Detection of Smallmouth Bass by Decade in NS Lakes",
       x = "Longitude", y = "Latitude",
       fill = "Decade")



############ Plot 2 - Chain Pickerel ############
ggplot() +
  geom_sf(data = NS_sf, fill = NA, color = "black") +
  geom_sf(data = shp_lakes, color = "deepskyblue4", fill = "deepskyblue3", size = 0.25, alpha = 0.7) +
  geom_sf(data = shp_county, fill = NA, color = "black", size = 0.5) +  # county boundaries
  geom_sf(data = shp_wshed, fill = NA, color = "darkgreen", size = 0.25, alpha = 0.3) + # optional watershed outlines
  coord_sf(xlim = c(-66.7, -59.5), ylim = c(43.2, 47)) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  geom_sf(data = pickerel_sf, aes(fill = Decade), color = "black", shape = 21, size = 2) +
  scale_fill_viridis_d() +
  labs(title = "First Detection of Chain Pickerel by Decade in NS Lakes",
       x = "Longitude", y = "Latitude",
       fill = "Decade")


