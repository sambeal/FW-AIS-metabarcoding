################################################################################
# MASTER SCRIPT: Combine FINS + DEEHR (SMB & CP) for AIS assessment in NS lakes
################################################################################

library(dplyr)
library(stringr)
library(sf)
library(tidyr)
library(units)

################################################################################
# 1. LOAD FINS DATA
################################################################################

fins_lakes <- read.csv("../../3_FW_survey/NS fishes docs/FINS/FINS 2025-06-05_SBmodified_lakesize.csv")
fins_fish  <- read.csv("../../3_FW_survey/NS fishes docs/FINS/FINS 2025-06-05_SBmodified_lakefish.csv")

fins_lakes_sf <- st_as_sf(fins_lakes,
                          coords = c("Easting","Northing"),
                          crs = 26920) %>%
  st_transform(4326) %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  )

# transform easting/northing to lat/long
fins_fish_sf <- st_as_sf(fins_fish,
                         coords = c("Easting","Northing"),
                         crs = 26920) %>%
  st_transform(4326) %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  ) 

# and drop geometry to revert back to a df (probs easier way to do this)
fins_fish <- fins_fish_sf %>%
  st_drop_geometry() 

################################################################################
# 2. LOAD & CLEAN DEEHR DATA
################################################################################

deehr_esni <- read.csv("../../3_FW_survey/NS fishes docs/DEEHR/DEEHR_cp_points.csv")
deehr_mido <- read.csv("../../3_FW_survey/NS fishes docs/DEEHR/SMB_Sheet_Cleaned_Sharing.csv")

deehr_esni_clean <- deehr_esni %>%
  filter(Province == "NS") %>%
  transmute(
    Species.Name = "Chain Pickerel",
    Name = str_to_upper(Location),
    County = str_to_title(Country),
    Year = as.numeric(Year),
    Easting,
    Northing
  )

deehr_mido_clean <- deehr_mido %>%
  transmute(
    Species.Name = "Smallmouth Bass",
    Name = str_to_upper(system),
    County = str_to_title(county),
    Year = as.numeric(year),
    Latitude = lat,
    Longitude = lon
  )

deehr_esni_sf <- st_as_sf(deehr_esni_clean,
                          coords = c("Easting","Northing"),
                          crs = 26920) %>%
  st_transform(4326)

deehr_mido_sf <- st_as_sf(deehr_mido_clean,
                          coords = c("Longitude","Latitude"),
                          crs = 4326)

deehr_all_sf <- bind_rows(deehr_esni_sf, deehr_mido_sf) %>%
  mutate(DEEHR_ID = row_number())


################################################################################
# 3. SPATIAL MATCHING: DEEHR POINTS → FINS LAKES (DISTANCE + COUNTY CHECK)
################################################################################

# Work in projected CRS for accurate distance (meters)
fins_utm  <- st_transform(fins_lakes_sf, 26920)
deehr_utm <- st_transform(deehr_all_sf, 26920)

# Find nearest FINS lake for each DEEHR point
nearest_idx <- st_nearest_feature(deehr_utm, fins_utm)

# Calculate distances
distances <- st_distance(deehr_utm, fins_utm[nearest_idx,], by_element = TRUE)
distances_m <- as.numeric(set_units(distances, "m"))

# Combine DEEHR + nearest FINS info
deehr_matched <- bind_cols(
  
  deehr_utm %>%
    st_drop_geometry() %>%
    mutate(Distance_m = distances_m),
  
  fins_utm[nearest_idx,] %>%
    st_drop_geometry() %>%
    select(
      FINS_County = County,
      Site.Code,
      Name,
      Primary.Watershed,
      Watershed.Code
    )
)

# -------------------------------------------------------------------------
# CLASSIFY MATCH CONFIDENCE USING DISTANCE + COUNTY AGREEMENT
# -------------------------------------------------------------------------
deehr_matched$County[deehr_matched$County == ""] <- NA

deehr_matched <- deehr_matched %>%
  mutate(
    County_match = case_when(
      is.na(County) ~ "missing",
      County == FINS_County ~ "same",
      TRUE ~ "different"
    ),
    Match_Status = case_when(
      Distance_m <= 200 & County_match == "same" ~ "confident",
      Distance_m <= 300 & County_match == "missing" ~ "review_missing_county",
      Distance_m <= 200 & County_match == "different" ~ "review_county_mismatch",
      Distance_m <= 500 & County_match == "same" ~ "review_possible_match",
      TRUE ~ "unlikely"
    )
  )

deehr_matched_confident <- deehr_matched %>%
  filter(Match_Status == "confident")

deehr_matched_confident$Match_Decision <- "automatic"
deehr_matched_confident$Match_Status_Confirmed <- "match"

review_cases <- deehr_matched %>%
  filter(Match_Status %in% c("review_missing_county", "review_county_mismatch", "review_possible_match", "unlikely"))

#write.csv(review_cases, "Output/deehr_spatial_matches_review_distancecounty.csv", row.names = FALSE)

# -------------------------------------------------------------------------
# Combine manually and automatically matched data
# -------------------------------------------------------------------------
deehr_reviewed <- read.csv("Output/deehr_spatial_matches_review_distancecounty_manually_reviewed2.csv", header = TRUE)

deehr_final <- bind_rows(deehr_matched_confident, deehr_reviewed) %>%
  select(-any_of(c("DEEHR_Name", "DEEHR_County", "FINS_Name")))

if("Name...2" %in% colnames(deehr_final)) names(deehr_final)[names(deehr_final) == 'Name...2'] <- 'DEEHR_Name'
if("County" %in% colnames(deehr_final)) names(deehr_final)[names(deehr_final) == 'County'] <- 'DEEHR_County'
if("Name...9" %in% colnames(deehr_final)) names(deehr_final)[names(deehr_final) == 'Name...9'] <- 'FINS_Name'

deehr_final$Database <- "DEEHR"


################################################################################
# 4. PREPARE FINS FISH DATA
################################################################################

lake_fish <- fins_fish %>%
  left_join(
    fins_lakes_sf %>%
      st_drop_geometry() %>%
      select(County,Site.Code,Name,Primary.Watershed,
             Secondary.Watershed,Watershed.Code,
             Assessment.Date,Surface.Area..ha.),
    by=c("County","Site.Code","Name",
         "Primary.Watershed","Secondary.Watershed",
         "Watershed.Code",
         "Captured.Date"="Assessment.Date"),
    relationship="many-to-many") %>%
  mutate(
    Date = as.Date(Captured.Date,"%d %b %Y"),
    Year = as.numeric(format(Date,"%Y")),
    Database="FINS"
  )


################################################################################
# 5. CLEAN UP AND COMBINE DEEHR + FINS DETECTIONS
################################################################################

# --- ADDED FIX: SPATIAL CONSOLIDATION FOR DEEHR-ONLY LAKES ---
# This identifies DEEHR-only points within 200m and gives them identical names
deehr_coords_lookup <- deehr_all_sf %>%
  mutate(Lon_DEEHR = st_coordinates(.)[,1], Lat_DEEHR = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(DEEHR_ID, Lon_DEEHR, Lat_DEEHR)

deehr_only_fix <- deehr_final %>%
  filter(is.na(Site.Code) | Match_Status_Confirmed != "match") %>%
  left_join(deehr_coords_lookup, by = "DEEHR_ID") %>%
  st_as_sf(coords = c("Lon_DEEHR", "Lat_DEEHR"), crs = 4326) %>%
  st_transform(26920)

internal_clusters <- st_is_within_distance(deehr_only_fix, dist = 200)
deehr_only_fix$Group_ID <- sapply(internal_clusters, min)

deehr_consolidated_names <- deehr_only_fix %>%
  group_by(Group_ID) %>%
  mutate(
    Consolidated_Name = first(DEEHR_Name),
    Consolidated_County = first(DEEHR_County)
  ) %>%
  st_drop_geometry() %>%
  select(DEEHR_ID, Consolidated_Name, Consolidated_County)

# Now proceed with deehr_ready using the consolidated names
deehr_ready <- deehr_final %>%
  left_join(deehr_consolidated_names, by = "DEEHR_ID") %>%
  mutate(
    Final_Name   = case_when(
      Match_Status_Confirmed == "match" ~ FINS_Name,
      !is.na(Consolidated_Name) ~ Consolidated_Name,
      TRUE ~ DEEHR_Name
    ),
    Final_County = case_when(
      Match_Status_Confirmed == "match" ~ FINS_County,
      !is.na(Consolidated_County) ~ Consolidated_County,
      TRUE ~ DEEHR_County
    ),
    Database     = "DEEHR"
  ) %>%
  select(County = Final_County, Site.Code, Name = Final_Name, Primary.Watershed, Watershed.Code, Species.Name, Year, Database, DEEHR_ID)

lake_fish_combined <- bind_rows(
  lake_fish %>% select(County, Site.Code, Name, Primary.Watershed, Watershed.Code, Species.Name, Year, Database),
  deehr_ready %>% select(-DEEHR_ID)
)

################################################################################
# 6. CALCULATE AIS STATUS PER LAKE / WATERSHED
################################################################################

ais_watershed_intro <- lake_fish_combined %>%
  filter(Species.Name %in% c("Smallmouth Bass", "Chain Pickerel")) %>%
  group_by(Primary.Watershed, Species.Name) %>%
  summarise(WS_First_Year = min(Year, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(Primary.Watershed)) %>%
  pivot_wider(names_from = Species.Name, values_from = WS_First_Year) %>%
  rename(WS_First_SMB = `Smallmouth Bass`, WS_First_CP  = `Chain Pickerel`)

lake_ais_summary <- lake_fish_combined %>%
  group_by(County, Name, Site.Code, Primary.Watershed) %>%
  summarise(
    Lake_Last_Sampled_Year = max(Year, na.rm = TRUE),
    Lake_First_SMB = min(Year[Species.Name == "Smallmouth Bass"], na.rm = TRUE),
    Lake_First_CP  = min(Year[Species.Name == "Chain Pickerel"], na.rm = TRUE),
    Data_Source = case_when(
      any(Database == "FINS") & any(Database == "DEEHR") ~ "Both",
      any(Database == "FINS") ~ "FINS Only",
      any(Database == "DEEHR") ~ "DEEHR Only",
      TRUE ~ "Unknown"
    ),
    .groups = "drop"
  ) %>%
  mutate(across(c(Lake_First_SMB, Lake_First_CP, Lake_Last_Sampled_Year), 
                ~ifelse(is.infinite(.), NA, .))) %>%
  left_join(ais_watershed_intro, by = "Primary.Watershed")


################################################################################
# 7. FINAL LAKE STATUS (The "Believable Absence" Logic)
################################################################################
lake_ais_status <- lake_ais_summary %>%
  mutate(
    SMB_lake_absence_believable = case_when(
      !is.na(Lake_First_SMB) ~ NA,
      is.na(Lake_First_SMB) & is.na(WS_First_SMB) ~ TRUE,
      is.na(Lake_First_SMB) & Lake_Last_Sampled_Year >= WS_First_SMB ~ TRUE,
      TRUE ~ FALSE
    ),
    CP_lake_absence_believable = case_when(
      !is.na(Lake_First_CP) ~ NA,
      is.na(Lake_First_CP) & is.na(WS_First_CP) ~ TRUE,
      is.na(Lake_First_CP) & Lake_Last_Sampled_Year >= WS_First_CP ~ TRUE,
      TRUE ~ FALSE
    ),
    Lake_SMB_status = case_when(
      !is.na(Lake_First_SMB) ~ "Present",
      SMB_lake_absence_believable == TRUE ~ "Absent",
      TRUE ~ "Data insufficient"
    ),
    Lake_CP_status = case_when(
      !is.na(Lake_First_CP) ~ "Present",
      CP_lake_absence_believable == TRUE ~ "Absent",
      TRUE ~ "Data insufficient"
    ),
    
    Lake_AIS_Status = case_when(
      # Confirmed Both
      Lake_SMB_status=="Present" & Lake_CP_status=="Present" ~ "Both",
      
      # Confirmed One, Confirmed Absent Other
      Lake_SMB_status=="Present" & Lake_CP_status=="Absent"  ~ "Smallmouth Bass only",
      Lake_SMB_status=="Absent"  & Lake_CP_status=="Present" ~ "Chain Pickerel only",
      
      # Confirmed One, Other Unknown 
      Lake_SMB_status=="Present" & Lake_CP_status=="Data insufficient" ~ "SMB Present_CP unknown",
      Lake_SMB_status=="Data insufficient" & Lake_CP_status=="Present" ~ "CP Present_SMB unknown",
      
      # Confirmed No AIS
      Lake_SMB_status=="Absent" & Lake_CP_status=="Absent" ~ "No AIS",
      
      # One Absent, Other Unknown
      Lake_SMB_status=="Absent" & Lake_CP_status=="Data insufficient" ~ "No SMB_CP unknown",
      Lake_SMB_status=="Data insufficient" & Lake_CP_status=="Absent" ~ "No CP_SMB_unknown",
      
      # Total Mystery
      Lake_SMB_status=="Data insufficient" & Lake_CP_status=="Data insufficient" ~ "Unknown",
      TRUE ~ "Unknown"
    ))

################################################################################
# 8. Assign watershed and county AIS status
################################################################################
ws_ais_status <- lake_ais_status %>%
  group_by(Primary.Watershed) %>%
  summarise(WS_AIS_Status = case_when(
    any(Lake_AIS_Status == "Both", na.rm = TRUE) ~ "Both",
    any(Lake_AIS_Status == "Smallmouth Bass only", na.rm = TRUE) & any(Lake_AIS_Status == "Chain Pickerel only", na.rm = TRUE) ~ "Both",
    any(Lake_AIS_Status == "Chain Pickerel only", na.rm = TRUE) ~ "Chain Pickerel only",
    any(Lake_AIS_Status == "Smallmouth Bass only", na.rm = TRUE) ~ "Smallmouth Bass only",
    all(Lake_AIS_Status == "No AIS", na.rm = TRUE) ~ "No AIS",
    TRUE ~ "Unknown"
  ))

county_ais_status <- lake_ais_status %>%
  group_by(County) %>%
  summarise(County_AIS_Status = case_when(
    any(Lake_AIS_Status == "Both") ~ "Both",
    any(Lake_AIS_Status == "Smallmouth Bass only") & any(Lake_AIS_Status == "Chain Pickerel only") ~ "Both",
    any(Lake_AIS_Status == "Chain Pickerel only") ~ "Chain Pickerel only",
    any(Lake_AIS_Status == "Smallmouth Bass only") ~ "Smallmouth Bass only",
    all(Lake_AIS_Status == "No AIS") ~ "No AIS",
    TRUE ~ "Unknown"))


################################################################################
# 9. ATTACH SPATIAL DATA (ROBUST VERSION)
################################################################################

# -------------------------------------------------------------------------
# 1. FINS coordinate lookup (UPDATED: pulling from fish records too)
# -------------------------------------------------------------------------

# Get coords from the metadata file
fins_lakes_coords <- fins_lakes_sf %>%
  st_drop_geometry() %>%
  select(County, Site.Code, Name, Primary.Watershed, Longitude, Latitude) %>%
  distinct()

# Get coords from the fish record file (your newly transformed data)
fins_fish_coords <- lake_fish %>%
  select(County, Site.Code, Name, Primary.Watershed, Longitude, Latitude) %>%
  distinct()

# Combine them: prioritize metadata, but fill holes with fish records
fins_coords_lookup <- full_join(fins_lakes_coords, fins_fish_coords, 
                                by = c("County", "Site.Code", "Name", "Primary.Watershed"),
                                suffix = c(".meta", ".fish")) %>%
  mutate(
    Longitude = coalesce(Longitude.meta, Longitude.fish),
    Latitude  = coalesce(Latitude.meta, Latitude.fish)
  ) %>%
  select(-Longitude.meta, -Longitude.fish, -Latitude.meta, -Latitude.fish)


# -------------------------------------------------------------------------
# 2. DEEHR coordinate lookup
# -------------------------------------------------------------------------
deehr_coords_lookup <- deehr_all_sf %>%
  mutate(Lon_DEEHR = st_coordinates(.)[,1], Lat_DEEHR = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(DEEHR_ID, Lon_DEEHR, Lat_DEEHR)

# -------------------------------------------------------------------------
# 3. Attach DEEHR IDs to lakes
# -------------------------------------------------------------------------
deehr_id_lookup <- deehr_ready %>%
  select(County, Name, Site.Code, Primary.Watershed, DEEHR_ID) %>%
  filter(!is.na(DEEHR_ID)) %>%
  distinct()


# -------------------------------------------------------------------------
# 4. Join coordinates
# -------------------------------------------------------------------------

ais_with_coords <- lake_ais_status %>%
  # First attach FINS coordinates (Universal lookup handles fish-only records now)
  left_join(fins_coords_lookup, by = c("County", "Site.Code", "Name", "Primary.Watershed")) %>%
  
  # Attach DEEHR IDs
  left_join(deehr_id_lookup, by = c("County", "Name", "Site.Code", "Primary.Watershed")) %>%
  
  # Attach DEEHR coordinates
  left_join(deehr_coords_lookup, by = "DEEHR_ID") %>%
  
  # Final coalesce logic
  mutate(
    Longitude = coalesce(Longitude, Lon_DEEHR),
    Latitude  = coalesce(Latitude, Lat_DEEHR)
  ) %>%
  select(-Lon_DEEHR, -Lat_DEEHR, -DEEHR_ID) %>%
  left_join(ws_ais_status, by = "Primary.Watershed") %>%
  left_join(county_ais_status, by = "County")


################################################################################
# 10. ATTACH LAKE MORPHOLOGY (FINS ONLY)
################################################################################

lake_morphology <- fins_lakes %>%
  select(County, Name, Site.Code, Primary.Watershed, Volume..m.., Surface.Area..ha., 
         Maximum.Depth..m., Mean.Depth..m., Area.Less.6M.Deep..ha., Shoreline.Dev)

# --- ADDED FIX: Final distinct() to kill join-duplicates ---
ais_final_master <- ais_with_coords %>%
  left_join(lake_morphology, by = c("County", "Name", "Site.Code", "Primary.Watershed"), relationship = "many-to-many") %>%
  distinct()

################################################################################
# 11. EXPORT as csv and kml for google earth
################################################################################
write.csv(ais_final_master, "Output/AIS_status_all_levels_FINS-DEEHR_260315.csv", row.names=FALSE)

################################################################################
# 12. COMBINE AIS STATUS WITH FULL SPECIES LISTS
################################################################################

status_lookup <- ais_final_master %>%
  select(County, Name, Site.Code, Primary.Watershed, Data_Source, Lake_Last_Sampled_Year, 
         Lake_SMB_status, Lake_CP_status, Lake_AIS_Status, WS_First_SMB, WS_First_CP, WS_AIS_Status,
         Volume..m.., Surface.Area..ha., Maximum.Depth..m., Mean.Depth..m., Area.Less.6M.Deep..ha., Shoreline.Dev)

fish_community_with_ais <- lake_fish %>%
  left_join(status_lookup, by = c("County", "Name", "Site.Code", "Primary.Watershed"))

master_community_list <- bind_rows(
  fish_community_with_ais,
  deehr_ready %>% mutate(Date = as.Date(paste0(Year, "-01-01"))) 
)

################################################################################
# 13. EXPORT COMMUNITY DATA
################################################################################
write.csv(master_community_list, "Output/Community_composition_AIS_status_FINS-DEEHR_260315.csv", row.names = FALSE)

################################################################################
# 14. GOOGLE EARTH EXPORT (Separate Files for Easy Coloring)
################################################################################
library(sf)

# 1. Prep the data
kml_prep <- ais_final_master %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  mutate(Description = paste0("County: ", County, 
                              "<br>Site Code: ", Site.Code, 
                              "<br>Last Sampled: ", Lake_Last_Sampled_Year)) %>%
  select(Name, Description, Lake_AIS_Status, Longitude, Latitude)

# 2. Get unique statuses
statuses <- unique(kml_prep$Lake_AIS_Status)

# 3. Loop through and write a separate file for each status
for (s in statuses) {
  # Create a subset for this specific status
  subset_sf <- kml_prep %>% 
    filter(Lake_AIS_Status == s) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Clean the status name for a safe filename (replaces spaces/slashes with underscores)
  file_name <- str_replace_all(s, "[^[:alnum:]]", "_")
  kml_path <- paste0("Output/AIS_", file_name, ".kml")
  
  # Delete if it exists so we start fresh
  if(file.exists(kml_path)) file.remove(kml_path)
  
  # Write the file using the basic KML driver
  st_write(subset_sf, dsn = kml_path, driver = "kml", delete_dsn = TRUE)
}
