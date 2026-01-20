################################################################################

# Plot: to map NSISC sample locations and lakes in FINS 
# onto a map of NS where the watersheds are coloured by AIS status (somewhat tranluscent shade)
# and the counties are outlined by AIS status 

################################################################################

############ Load Packages ############ 
library(sf)
# Avoid geometry errors
sf_use_s2(FALSE)

library(ggplot2)
library(dplyr)
library(viridis)
library(ggspatial)
library(rnaturalearth)


############ Load FINS-AIS plotting file (sf) ############ 
ais_plotting_sf <- st_read("Output/AIS_status_all_levels_260119.gpkg")

############ Load Map Data ############ 
# Waterbodies
wbody_sf <- st_read("../../NS mapping/Data/canvec_50K_NS_Hydro_shp/canvec_50K_NS_Hydro/waterbody_2.shp")

# Lakes (subset from all waterbodies) 
lakes_sf <- subset(wbody_sf, definit_en == "Lake")
# CRN: GCS_North_American_1983_CSRS98

# Primary watersheds
wshed_sf <- st_read("../../NS mapping/Data/NS_Watersheds_10k/PRIMARY_POLY_CSRS_UTM_Z20.shp")
# Projected CRS: NAD83(CSRS) / UTM zone 20N

# County boundaries
county_sf <- st_read("../../NS mapping/Data/Nova Scotia Topographic Database - County Boundaries/geo_export_c2cb38d4-df79-418a-99e4-b970bb96d87c.shp")
# Geodetic CRS:  WGS84(DD)

# Canada & subset Nova Scotia 
can_provinces_sf <- ne_states(
  country = "Canada",
  returnclass = "sf"
)

NS_sf <- can_provinces_sf %>%
  filter(name == "Nova Scotia")

############ Transform all to WGS84 ############ 
crs_wgs <- 4326
lakes_sf <- st_transform(lakes_sf, crs = crs_wgs)
wshed_sf <- st_transform(wshed_sf, crs = crs_wgs)
county_sf <- st_transform(county_sf, crs = crs_wgs)
NS_sf <- st_transform(NS_sf, crs = crs_wgs)
ais_plotting_sf <- st_transform(ais_plotting_sf, crs = crs_wgs)


############ Layers (sanity) check ############

# lakes_sf: lake polygons
# wshed_sf: (primary) watershed polygons
# county_sf: county outlines
# NS_sf: NS outline
# ais_plotting_sf: points, AIS status


############ Plot 1 - Lakes surveys ############


#Identify sampled lakes polygons
# For each lake polygon, check if it contains at least one sampled point
intersects_matrix <- st_intersects(lakes_sf, ais_plotting_sf, sparse = FALSE)
fins_lakes <- lakes_sf[rowSums(intersects_matrix) > 0, ]

my_colors <- c("Both" = "#440154FF", "Smallmouth Bass only" = "#2A788EFF", 
               "Chain Pickerel only" = "#FDE725FF", "No AIS" = "#7AD151FF",
               "Unknown / insufficient data" = "darkgrey")

# viridis(6) (remove # from following line to see colour options)
# "#440154FF" "#414487FF" "#2A788EFF" "#22A884FF" "#7AD151FF" "#FDE725FF"

# Merge WS_AIS_Status into wshed_sf

library(dplyr)
library(sf)

# Collapse AIS status to one row per watershed
ws_ais_status <- ais_plotting_sf %>%
  st_drop_geometry() %>%           # drop points, keep only data
  group_by(Primary.Watershed) %>%
  summarise(
    WS_AIS_Status = case_when(
      any(Lake_AIS_Status == "Both") ~ "Both",
      any(Lake_AIS_Status == "Smallmouth Bass only") &
        any(Lake_AIS_Status == "Chain Pickerel only") ~ "Both",
      any(Lake_AIS_Status == "Smallmouth Bass only") ~ "Smallmouth Bass only",
      any(Lake_AIS_Status == "Chain Pickerel only") ~ "Chain Pickerel only",
      any(Lake_AIS_Status == "Unknown / insufficient data") ~ "Unknown / insufficient data",
      TRUE ~ "No AIS"
    ),
    .groups = "drop"
  )

# convert ws code from number to name
# define watershed codenames from: https://www.dfo-mpo.gc.ca/publications/images/habitat/highlights-faitssaillants/nova-scotia-nouvelle-ecosse/overview-apercu/map-carte-3-eng.jpg


#   wshed_sf$RIVER = ais_plotting_sf$Primary.Watershed OMG
# with exception that wshed_sf$RIVER = "COSTAL ISLAND"  
library(stringr)

clean_name <- function(x) {
  x %>%
    str_to_lower() %>%                     # normalize first
    str_replace_all("&", "and") %>%        # & → and
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish() %>%
    str_to_title()                         # convert to Title Case
}

wshed_sf <- wshed_sf %>%
  mutate(watershed_renamed = clean_name(RIVER))


# from here, need to rename to be consistent with ais_plotting_sf
unique(ais_plotting_sf$Primary.Watershed) #Names i want
unique(wshed_sf$watershed_renamed) #Names i have and need to modify

setdiff(
  unique(wshed_sf$watershed_renamed),
  unique(ais_plotting_sf$Primary.Watershed)
)
# 27 names to in wshed_sf that are not in ais_plotting

setdiff(
  unique(ais_plotting_sf$Primary.Watershed),
  unique(wshed_sf$watershed_renamed)
)
# 26 names to in ais_plotting that are not in wshed_sf

# figure out which to modify: side-by-side
library(tibble)

have <- sort(unique(wshed_sf$watershed_renamed))
want <- sort(unique(ais_plotting_sf$Primary.Watershed))

diff <- tibble(
  Have = c(have, rep(NA, max(length(want) - length(have), 0))),
  Want = c(want, rep(NA, max(length(have) - length(want), 0)))
)

print(diff, n=47)

# manually rename wshed_sf so they match
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Barrington Clyde"] <- "Barrington and Clyde"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Cheticamp River"] <- "Cheticamp"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Clam Hrb St Francis"] <- "Clam Harbour/ St. Francis"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Costal Island"] <- "Coastal Island"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "East Indian River"] <- "East and Indian"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "East Middle West Pictou"] <- "East, Middle, West (Pictou)"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "East West Sheet Hbr"] <- "East (Sheet Harbour)"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Herring Cove Medway"] <- "Herring Cove and Medway"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Kelly Maccan Hebert"] <- "Kelley, Maccan and Hebert"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Lahave"] <- "LaHave"
ais_plotting_sf$Primary.Watershed[ais_plotting_sf$Primary.Watershed == "Missaguas"] <- "Missaguash" # note this one: wshed = correct spelling, not ais_plotting file!
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "New Hbr Salmon"] <- "New Hbr./ Salmon (Guys.)"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "North Baddeck Middle"] <- "Baddeck and Middle"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Philip Wallace"] <- "Philip and Wallace"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "River Denys Big"] <- "Denys and Big"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "River Inhabitants"] <- "Inhabitants"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "River John"] <- "John"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Roseway Sable Jordan"] <- "Roseway and Sable and Jordan"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Salmon Debert"] <- "Salmon and Debert"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Salmon Mira"] <- "Salmon and Mira"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Shubenacadie Stewiacke"] <- "Shubenacadie and Stewiacke"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Sissiboo Bear"] <- "Sissiboo and Bear"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "South West"] <- "South and West (Antigonish)"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "St Croix"] <- "St. Croix"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "St Marys"] <- "St. Mary's"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Tidnish Shinimicas"] <- "Tidnish and Shinimicas"
wshed_sf$watershed_renamed[wshed_sf$watershed_renamed == "Tusket River"] <- "Tusket"

    
        
wshed_sf_status_1 <- wshed_sf %>%
  st_join(
    ais_plotting_sf,
    by = c("watershed_renamed" = "Primary.Watershed")
  )


# Plotting 
ggplot() +
  # Base map: Nova Scotia outline
  geom_sf(data = NS_sf, fill = NA, color = "black") +
  # All lakes as subtle outlines
  geom_sf(data = lakes_sf, fill = "lightblue", color = "lightblue", size = 0.1, alpha = 0.2) +
  # Only colour the lakes in FINS dataset
  geom_sf(data = fins_lakes, fill = "deepskyblue4", color = "deepskyblue4", size = 0.25, alpha = 0.7) +
  # Add county lines
  geom_sf(data = county_sf, fill = NA, color = "black", size = 0.1) +
  # Add watershed lines
  geom_sf(data = wshed_sf_status_1, aes(fill = WS_AIS_Status) , color = "lightgrey", size = 0.25, alpha = 0.3) +
  scale_fill_manual(values = my_colors) + # Apply custom colors
# Scale map to NS
  coord_sf(xlim = c(-66.7, -59.5), 
           ylim = c(43.2, 47)) +
  theme_classic() +
  labs(title = "AIS in FINS database",
       x = "Longitude",
       y = "Latitude")
