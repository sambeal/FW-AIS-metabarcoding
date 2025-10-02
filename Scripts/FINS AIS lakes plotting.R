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
library(rnaturalearth)
library(dplyr)
library(terra)
library(tidyterra)
library(geodata)
library(ggspatial)
library(viridis)


############ Load the data ############ 
ais <- read.csv("Analysis/AIS_lakes.csv", header = TRUE)


# Convert Capture date to proper Date class
ais <- ais %>%
  mutate(
    Date = as.Date(Captured.Date, format = "%d %b %Y"),
    Year = as.numeric(format(Date, "%Y"))
  )

# First detection per lake (minimum year)
ais_first <- ais %>%
  group_by(County, Site.Code, Name, Primary.Watershed, Secondary.Watershed,
           Watershed.Code, Easting, Northing, Species.Name) %>%
  summarise(
    First_Year = min(Year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Decade_num = floor(First_Year / 10) * 10,
    Decade = factor(paste0(Decade_num, "s"),
                    levels = sort(unique(paste0(Decade_num, "s"))))
  )


############ Plotting ############ 
# Step 1: Convert AIS to sf using Easting/Northing
# Replace 26920 if your CRS is different (this is NAD83 / UTM zone 20N, common in NS)
ais_sf <- st_as_sf(ais_first, coords = c("Easting", "Northing"), crs = 26920)

# Step 2: Transform to WGS84 (lat/long)
ais_latlong <- st_transform(ais_sf, crs = 4326)

# Step 3: Extract numeric columns for ggplot
ais_latlong <- ais_latlong %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  )

############ Load Map Data ############ 
# Lakes and PRIMARY watersheds
shp_wbody <- st_read("../../NS mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/waterbody_2.shp")
# CRN: GCS_North_American_1983_CSRS98

shp_wshed <- st_read("../../NS mapping/Data/NS_Watersheds_10k/PRIMARY_POLY_CSRS_UTM_Z20.shp")
# Projected CRS: NAD83(CSRS) / UTM zone 20N

# Subset to lakes only
shp_lakes <- subset(shp_wbody, definit_en == "Lake")

# County boundaries
shp_county <- st_read("../../NS mapping/Data/Nova Scotia Topographic Database - County Boundaries/geo_export_c2cb38d4-df79-418a-99e4-b970bb96d87c.shp")
# Geodetic CRS:  WGS84(DD)

# set CRS for all to use and transform
crs_wgs <- 4326

shp_lakes <- st_transform(shp_lakes, crs = crs_wgs)
shp_wshed <- st_transform(shp_wshed, crs = crs_wgs)
shp_county <- st_transform(shp_county, crs = crs_wgs)


# Load North America & subset Nova Scotia ----
NorthAmerica <- gadm(country = country_codes("Canada")$ISO3, level = 1, path = getwd())
NorthAmerica_sf <- st_as_sf(NorthAmerica)

# Subset for Nova Scotia only
NS_sf <- NorthAmerica_sf %>% filter(NAME_1 == "Nova Scotia")

# Transform NS map to match points CRS
NS_sf <- st_transform(NS_sf, crs = crs_wgs)

sf_use_s2(FALSE)

############ Plot ############ 
# Separate AIS data by species
bass_sf <- ais_latlong %>% filter(Species.Name == "Smallmouth Bass")
pickerel_sf <- ais_latlong %>% filter(Species.Name == "Chain Pickerel")


############ Plot 1 - Smallmouth Bass ############
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
  geom_sf(data = bass_sf, aes(fill = Decade), color = "black", shape = 21, size = 2) +
  scale_fill_viridis_d() +
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


