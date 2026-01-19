#################################################################################
# first look at the eDNA sample locations from the 
# NS Invasive Species Council's survey

# data provided by Sheri MacNeil at end of 2025

# goal of this exploration:
#   get a rough idea of where in the province eDNA samples have already
#   been collected to supplement the sampling I will do in Ch. 3
#   to assess the impacts of AIS on NS's freshwater fish diversity (richness)
#################################################################################

# Load Data ----
carp <-  read.csv("../NSISC_2025-11-13/NSISC_2025-11-13_CARP.csv", header = TRUE)
cmm <-  read.csv("../NSISC_2025-11-13/NSISC_2025-11-13_CMM.csv", header = TRUE)
coac <-  read.csv("../NSISC_2025-11-13/NSISC_2025-11-13_COAC.csv", header = TRUE)
hrm <-  read.csv("../NSISC_2025-11-13/NSISC_2025-11-13_HRM.csv", header = TRUE)
keji <-  read.csv("../NSISC_2025-11-13/NSISC_2025-11-13_KEJI.csv", header = TRUE)
mapc <-  read.csv("../NSISC_2025-11-13/NSISC_2025-11-13_MAPC.csv", header = TRUE)
moeh <-  read.csv("../NSISC_2025-11-13/NSISC_2025-11-13_MOEH.csv", header = TRUE)
pons <-  read.csv("../NSISC_2025-11-13/NSISC_2025-11-13_PONS.csv", header = TRUE)

#################################################################################

# need to rename/clean up these csvs so they can be merged together 
#carp----
colnames(carp)

library(dplyr)
carp_clean <- carp %>% select(c("Group":"Site","CODE","Location_Lat":"Date"))

# add in a column "RIVER" which will be empty but so can add to the other
# groups while distinguishing waterbody type
carp_clean$RIVER <- NA

# relocate to after LAKE
carp_clean <- carp_clean %>% relocate(RIVER, .after = LAKE)

#cmm----
colnames(cmm)
cmm_clean <- cmm %>% select(c("Group":"Site","CODE","Location_Lat":"Date"))

# add in a column "LAKE" which will be empty but so can add to the other
# groups while distinguishing waterbody type
cmm_clean$LAKE <- NA

# relocate to after LAKE
cmm_clean <- cmm_clean %>% relocate(LAKE, .before = RIVER)

#coac----
colnames(coac)
coac_clean <- coac %>% select(c("Group", "Lake.Name", "Site.Group","Sample.Code","Location_Lat":"Date"))

# rename to be consistent
coac_clean <- coac_clean %>% 
  rename(LAKE = Lake.Name, Site = Site.Group, CODE = Sample.Code)


# add in a column "RIVER" which will be empty but so can add to the other
# groups while distinguishing waterbody type
coac_clean$RIVER <- NA

# relocate to after LAKE
coac_clean <- coac_clean %>% relocate(RIVER, .after = LAKE)

#hrm----
colnames(hrm)
hrm_clean <- hrm %>% select(c("Group":"Date"))

# add in a column "RIVER" which will be empty but so can add to the other
# groups while distinguishing waterbody type
hrm_clean$RIVER <- NA

# relocate to after LAKE
hrm_clean <- hrm_clean %>% relocate(RIVER, .after = LAKE)

#keji----
colnames(keji)
keji_clean <- keji %>% select(c("Group":"CODE","Location_Lat":"Date"))

# add in a column "RIVER" which will be empty but so can add to the other
# groups while distinguishing waterbody type
keji_clean$RIVER <- NA

# relocate to after LAKE
keji_clean <- keji_clean %>% relocate(RIVER, .after = LAKE)

#mapc----
colnames(mapc)
mapc_clean <- mapc %>% select(c("Group":"CODE","Location_Lat":"Date"))

# add in a column "RIVER" which will be empty but so can add to the other
# groups while distinguishing waterbody type
mapc_clean$RIVER <- NA

# relocate to after LAKE
mapc_clean <- mapc_clean %>% relocate(RIVER, .after = LAKE)

#moeh----
colnames(moeh)
moeh_clean <- moeh %>% select(c("Group", "Lake.Name", "Site.Group","Sample.Code","Location_Lat":"Date"))

# rename to be consistent
moeh_clean <- moeh_clean %>% 
  rename(LAKE = Lake.Name, Site = Site.Group, CODE = Sample.Code)


# add in a column "RIVER" which will be empty but so can add to the other
# groups while distinguishing waterbody type
moeh_clean$RIVER <- NA

# relocate to after LAKE
moeh_clean <- moeh_clean %>% relocate(RIVER, .after = LAKE)

#pons----
colnames(pons)
pons_clean <- pons %>% select(c("Group", "Lake.Name", "Site.Group","Sample.Code","Location_Lat":"Date"))

# rename to be consistent
pons_clean <- pons_clean %>% 
  rename(LAKE = Lake.Name, Site = Site.Group, CODE = Sample.Code)

# add in a column "RIVER" which will be empty but so can add to the other
# groups while distinguishing waterbody type
pons_clean$RIVER <- NA

# relocate to after LAKE
pons_clean <- pons_clean %>% relocate(RIVER, .after = LAKE)

#################################################################################
# add together ----

coords_clean <- rbind(carp_clean,cmm_clean,coac_clean,
                      hrm_clean,keji_clean,mapc_clean,
                      moeh_clean,pons_clean)

library(tidyverse)
coords_clean_noNA <- coords_clean %>%
  drop_na(Location_Lat)

# rename Lat and Long columns

coords_clean_noNA <- coords_clean_noNA %>% 
  rename(Latitude = Location_Lat, Longitude = Location_Lon)

# make sure all Longitudes are negatives (W)
coords_clean_fixlong <- coords_clean_noNA %>%
  mutate(Longitude = if_else(Longitude > 0, -Longitude, Longitude))


# note: still not done, need to finish standardizing (e.g. Date column)
# but in a good enouhg place to plot right now

# save as .csv for easy re-using
write.csv(coords_clean_fixlong, "../NSISC_2025-11-13/NSISC_2025-11-13_allsites.csv", row.names = FALSE)

#################################################################################
# plot ----

# Load Packages ----
library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(ggspatial)

# Turn off s2 to avoid geometry issues
sf_use_s2(FALSE)

# Load Your Data ----
# Sample coordinates (from your CSV)
coords_sf <- st_as_sf(coords_clean_fixlong, coords = c("Longitude", "Latitude"), crs = 4326)


# Nova Scotia boundary
ns_map <- ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name == "Nova Scotia")

ns_map <- st_transform(ns_map, crs = 4326)  # Ensure same CRS as coords

# Load shapefiles from NS Mapping directory inside PhD directory
shp_wbody <- st_read("../../NS Mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/waterbody_2.shp") |> st_zm()
shp_wcourse <- st_read("../../NS Mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/watercourse_1.shp") |> st_zm()
shp_wshed <- st_read("../../NS Mapping/Data/NS_Watersheds_10k/PRIMARY_POLY_CSRS_UTM_Z20.shp") |> st_zm()

# Reproject everything to WGS84 (for compatibility with coords_sf and ns_map)
shp_wbody <- st_transform(shp_wbody, crs = 4326)
shp_wcourse <- st_transform(shp_wcourse, crs = 4326)
shp_wshed <- st_transform(shp_wshed, crs = 4326)

# Subset lakes only
#shp_wbody_lakes <- subset(shp_wbody, definit_en == "Lake")

# Crop lakes and watersheds to NS boundary
#shp_wbody_lakes_ns <- st_intersection(shp_wbody_lakes, ns_map)
shp_wbody_ns <- st_intersection(shp_wbody, ns_map)
shp_wshed_ns <- st_intersection(shp_wshed, ns_map)


# Final Plot ----
ggplot() +
  # NS map outline
  geom_sf(data = ns_map, fill = "white", color = "black") +
  
  # Watersheds by flow direction
  geom_sf(data = shp_wshed_ns, color = "black", alpha = 0.4, size = 0.3) +
  
  # Lakes
#  geom_sf(data = shp_wbody_lakes_ns, fill = "deepskyblue3", color = "deepskyblue4", size = 0.25, alpha = 0.7) +
  geom_sf(data = shp_wbody_ns, fill = "deepskyblue3", color = "deepskyblue4", size = 0.25, alpha = 0.7) +
  
  
  # set colour scale
  scale_fill_viridis_d(option="plasma") +
  
  #  sample sites
  geom_sf(data = coords_sf, size = 3, aes(fill = Group), shape = 21, colour = "black") +
  
  # Axis labels and legend
  labs(x = "Longitude", y = "Latitude") +
  
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
