################################################################################
# MASTER SCRIPT: Combine FINS + DEEHR (SMB & CP) for AIS assessment in NS lakes ----
################################################################################

library(dplyr)
library(stringr)
library(sf)
library(tidyr)
library(units)

# =========================================================================
# Standardize Names 
# Strip punctuation, "LAKE", "RIVER", and extra spaces to force matches
# =========================================================================
clean_lake_names <- function(name_vector) {
  name_vector %>%
    str_to_upper() %>%
    str_replace_all("\\bLAKE\\b|\\bRIVER\\b", "") %>% # Drop common suffixes
    str_replace_all("[[:punct:]]", "") %>%            # Drop punctuation (e.g., St. vs St)
    str_squish()                                      # Remove double/trailing spaces
}

################################################################################
# 1. LOAD FINS DATA ----
################################################################################

fins_lakes <- read.csv("../../3_FW_survey/NS fishes docs/FINS/FINS 2025-06-05_SBmodified_lakesize.csv") %>%
  mutate(Name = clean_lake_names(Name)) # <-- NEW: Clean immediately

fins_fish  <- read.csv("../../3_FW_survey/NS fishes docs/FINS/FINS 2025-06-05_SBmodified_lakefish.csv") %>%
  mutate(Name = clean_lake_names(Name)) # <-- NEW: Clean immediately

fins_lakes_sf <- st_as_sf(fins_lakes,
                          coords = c("Easting","Northing"),
                          crs = 26920) %>%
  st_transform(4326) %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  )

fins_fish_sf <- st_as_sf(fins_fish,
                         coords = c("Easting","Northing"),
                         crs = 26920) %>%
  st_transform(4326) %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  ) 

fins_fish <- fins_fish_sf %>% st_drop_geometry() 

################################################################################
# 2. LOAD & CLEAN DEEHR DATA ----
################################################################################

deehr_esni <- read.csv("../../3_FW_survey/NS fishes docs/DEEHR/DEEHR_cp_points.csv")
deehr_mido <- read.csv("../../3_FW_survey/NS fishes docs/DEEHR/SMB_Sheet_Cleaned_Sharing.csv")

deehr_esni_clean <- deehr_esni %>%
  filter(Province == "NS") %>%
  transmute(
    Species.Name = "Chain Pickerel",
    Name = clean_lake_names(Location), # <-- NEW: Clean immediately
    County = str_to_title(Country),
    Year = as.numeric(Year),
    Easting,
    Northing
  )

deehr_mido_clean <- deehr_mido %>%
  transmute(
    Species.Name = "Smallmouth Bass",
    Name = clean_lake_names(system),   # <-- NEW: Clean immediately
    County = str_to_title(county),
    Year = as.numeric(year),
    Latitude = lat,
    Longitude = lon
  )

deehr_esni_sf <- st_as_sf(deehr_esni_clean, coords = c("Easting","Northing"), crs = 26920) %>%
  st_transform(4326)

deehr_mido_sf <- st_as_sf(deehr_mido_clean, coords = c("Longitude","Latitude"), crs = 4326)

deehr_all_sf <- bind_rows(deehr_esni_sf, deehr_mido_sf) %>%
  mutate(DEEHR_ID = row_number())

################################################################################
# 3. SPATIAL MATCHING: DEEHR POINTS → FINS LAKES (DISTANCE + COUNTY CHECK) ----
################################################################################

fins_utm  <- st_transform(fins_lakes_sf, 26920)
deehr_utm <- st_transform(deehr_all_sf, 26920)

nearest_idx <- st_nearest_feature(deehr_utm, fins_utm)
distances <- st_distance(deehr_utm, fins_utm[nearest_idx,], by_element = TRUE)
distances_m <- as.numeric(set_units(distances, "m"))

deehr_matched <- bind_cols(
  deehr_utm %>% st_drop_geometry() %>% mutate(Distance_m = distances_m),
  fins_utm[nearest_idx,] %>% st_drop_geometry() %>% select(FINS_County = County, Site.Code, Name, Primary.Watershed, Watershed.Code)
)

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

deehr_matched_confident <- deehr_matched %>% filter(Match_Status == "confident")
deehr_matched_confident$Match_Decision <- "automatic"
deehr_matched_confident$Match_Status_Confirmed <- "match"

review_cases <- deehr_matched %>%
  filter(Match_Status %in% c("review_missing_county", "review_county_mismatch",
                             "review_possible_match", "unlikely"))

# write out for manual review
write.csv(review_cases, "Output/deehr_spatial_matches_review_distancecounty_260319.csv", row.names = FALSE)

# -------------------------------------------------------------------------
# Check confident matches just to be safe, seeing some weird ones in final output 
# (19 mar)
# -------------------------------------------------------------------------
auto_match_review <- deehr_matched_confident[c("Name...2","County",
                                               "FINS_County","Name...9")]

print(auto_match_review)

# flag for fixing:631 BANOOK-MORRIS = not a match, 590 FLETCHERS-GOVERNOR = match,
# 590.1 FLETCHERS-GOVERNOR, 631.1 BANOOK-MORRIS, 
# 441 LITTLE CHEZETCOOK FIRE HALL-LITTLE = not a match

# -------------------------------------------------------------------------
# SURGICAL FIX: Override false positives in the automatic confident matches
# -------------------------------------------------------------------------
deehr_matched_confident <- deehr_matched_confident %>%
  mutate(
    # 1. Create a logical flag for the ones we know are wrong
    is_false_match = (Name...2 == "BANOOK" & Name...9 == "MORRIS") |
      (Name...2 == "LITTLE CHEZETCOOK FIRE HALL" & Name...9 == "LITTLE"),
    
    # 2. Downgrade their match status
    Match_Status = ifelse(is_false_match, "review_name_mismatch", Match_Status),
    Match_Decision = ifelse(is_false_match, "not_a_match", Match_Decision),
    Match_Status_Confirmed = ifelse(is_false_match, "not_a_match", Match_Status_Confirmed),
    
    # 3. Sever the ties to the incorrect FINS lake
    Site.Code = ifelse(is_false_match, NA, Site.Code),
    Name...9 = ifelse(is_false_match, NA_character_, Name...9),
    Primary.Watershed = ifelse(is_false_match, NA_character_, Primary.Watershed),
    FINS_County = ifelse(is_false_match, NA_character_, FINS_County)
  ) %>%
  # Clean up the temporary flag
  select(-is_false_match)

# -------------------------------------------------------------------------
# Combine manually and automatically matched data
# -------------------------------------------------------------------------
deehr_reviewed <- read.csv("Output/deehr_spatial_matches_review_distancecounty_manually_reviewed_260319.csv", header = TRUE) %>%
  mutate(Name...2 = clean_lake_names(Name...2), Name...9 = clean_lake_names(Name...9)) # Prevent manual review regressions

deehr_final <- bind_rows(deehr_matched_confident, deehr_reviewed) %>%
  select(-any_of(c("DEEHR_Name", "DEEHR_County", "FINS_Name")))

if("Name...2" %in% colnames(deehr_final)) names(deehr_final)[names(deehr_final) == 'Name...2'] <- 'DEEHR_Name'
if("County" %in% colnames(deehr_final)) names(deehr_final)[names(deehr_final) == 'County'] <- 'DEEHR_County'
if("Name...9" %in% colnames(deehr_final)) names(deehr_final)[names(deehr_final) == 'Name...9'] <- 'FINS_Name'

deehr_final$Database <- "DEEHR"

# =========================================================================
# NEW: DEDUPLICATE DEEHR RECORDS (Name-Based Only)
# =========================================================================
deehr_final_clean <- deehr_final %>%
  mutate(
    True_Lake_Identity = case_when(
      Match_Status_Confirmed == "match" ~ FINS_Name,
      TRUE ~ paste(DEEHR_Name, DEEHR_County, sep = "_")
    )
  ) %>%
  group_by(True_Lake_Identity, Species.Name) %>%
  arrange(desc(Year)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-True_Lake_Identity)


################################################################################
# 4. PREPARE FINS FISH DATA
################################################################################
lake_fish <- fins_fish %>%
  left_join(
    fins_lakes_sf %>%
      st_drop_geometry() %>%
      select(County,Site.Code,Name,Primary.Watershed, Secondary.Watershed,Watershed.Code, Assessment.Date,Surface.Area..ha.),
    by=c("County","Site.Code","Name","Primary.Watershed","Secondary.Watershed","Watershed.Code","Captured.Date"="Assessment.Date"),
    relationship="many-to-many") %>%
  mutate(
    Date = as.Date(Captured.Date,"%d %b %Y"),
    Year = as.numeric(format(Date,"%Y")),
    Database="FINS"
  )

################################################################################
# 5. CLEAN UP AND COMBINE DEEHR + FINS DETECTIONS ----
################################################################################

deehr_ready <- deehr_final_clean %>%
  mutate(
    Final_Name   = case_when(
      Match_Status_Confirmed == "match" ~ FINS_Name,
      TRUE ~ DEEHR_Name
    ),
    Final_County = case_when(
      Match_Status_Confirmed == "match" ~ FINS_County,
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

# =========================================================================
# HELPER FUNCTIONS: Prevent -Inf warnings on lakes missing sample years
# =========================================================================
safe_max <- function(x) { if(all(is.na(x))) NA_real_ else max(x, na.rm = TRUE) }
safe_min <- function(x) { if(all(is.na(x))) NA_real_ else min(x, na.rm = TRUE) }

ais_watershed_intro <- lake_fish_combined %>%
  filter(Species.Name %in% c("Smallmouth Bass", "Chain Pickerel")) %>%
  group_by(Primary.Watershed, Species.Name) %>%
  summarise(WS_First_Year = safe_min(Year), .groups = "drop") %>%
  filter(!is.na(Primary.Watershed)) %>%
  pivot_wider(names_from = Species.Name, values_from = WS_First_Year) %>%
  rename(WS_First_SMB = `Smallmouth Bass`, WS_First_CP  = `Chain Pickerel`)

lake_ais_summary <- lake_fish_combined %>%
  # Fill NA values in grouping variables to prevent them from splitting identical lakes
  mutate(
    Site.Code = replace_na(as.character(Site.Code), "UNKNOWN"),
    Primary.Watershed = replace_na(Primary.Watershed, "UNKNOWN")
  ) %>%
  group_by(County, Name, Site.Code, Primary.Watershed) %>%
  summarise(
    Lake_Last_Sampled_Year = safe_max(Year),
    Lake_First_SMB = safe_min(Year[Species.Name == "Smallmouth Bass"]),
    Lake_First_CP  = safe_min(Year[Species.Name == "Chain Pickerel"]),
    Data_Source = case_when(
      any(Database == "FINS") & any(Database == "DEEHR") ~ "Both",
      any(Database == "FINS") ~ "FINS Only",
      any(Database == "DEEHR") ~ "DEEHR Only",
      TRUE ~ "Unknown"
    ),
    .groups = "drop"
  ) %>%
  # Restore the NAs for later joins
  mutate(
    Site.Code = na_if(Site.Code, "UNKNOWN"),
    Primary.Watershed = na_if(Primary.Watershed, "UNKNOWN")
  ) %>%
  left_join(ais_watershed_intro, by = "Primary.Watershed")

################################################################################
# 7 & 8. FINAL LAKE STATUS & ASSIGNMENTS
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
      Lake_SMB_status=="Present" & Lake_CP_status=="Present" ~ "Both",
      Lake_SMB_status=="Present" & Lake_CP_status=="Absent"  ~ "Smallmouth Bass only",
      Lake_SMB_status=="Absent"  & Lake_CP_status=="Present" ~ "Chain Pickerel only",
      Lake_SMB_status=="Present" & Lake_CP_status=="Data insufficient" ~ "SMB Present_CP unknown",
      Lake_SMB_status=="Data insufficient" & Lake_CP_status=="Present" ~ "CP Present_SMB unknown",
      Lake_SMB_status=="Absent" & Lake_CP_status=="Absent" ~ "No AIS",
      Lake_SMB_status=="Absent" & Lake_CP_status=="Data insufficient" ~ "No SMB_CP unknown",
      Lake_SMB_status=="Data insufficient" & Lake_CP_status=="Absent" ~ "No CP_SMB_unknown",
      TRUE ~ "Unknown"
    ))

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

fins_lakes_coords <- fins_lakes_sf %>% st_drop_geometry() %>% select(County, Site.Code, Name, Primary.Watershed, Longitude, Latitude) %>% distinct()
fins_fish_coords <- lake_fish %>% select(County, Site.Code, Name, Primary.Watershed, Longitude, Latitude) %>% distinct()

# NEW: force exactly ONE coordinate per lake by calculating the mean.
# If a lake was sampled at 3 spots, distinct() keeps all 3. Coalescing later causes duplicates.
fins_coords_lookup <- full_join(fins_lakes_coords, fins_fish_coords, 
                                by = c("County", "Site.Code", "Name", "Primary.Watershed"),
                                suffix = c(".meta", ".fish")) %>%
  mutate(
    Longitude = coalesce(Longitude.meta, Longitude.fish),
    Latitude  = coalesce(Latitude.meta, Latitude.fish)
  ) %>%
  group_by(County, Site.Code, Name, Primary.Watershed) %>% 
  summarise(
    Longitude = mean(Longitude, na.rm = TRUE), # Takes centroid of all sampling points
    Latitude = mean(Latitude, na.rm = TRUE),
    .groups = "drop"
  )

deehr_coords_lookup <- deehr_all_sf %>% mutate(Lon_DEEHR = st_coordinates(.)[,1], Lat_DEEHR = st_coordinates(.)[,2]) %>% st_drop_geometry() %>% select(DEEHR_ID, Lon_DEEHR, Lat_DEEHR)

# NEW: Same logic as above. Force 1 DEEHR ID per lake to prevent row expansion
deehr_id_lookup <- deehr_ready %>%
  select(County, Name, Site.Code, Primary.Watershed, DEEHR_ID) %>%
  filter(!is.na(DEEHR_ID)) %>%
  group_by(County, Name, Site.Code, Primary.Watershed) %>%
  slice(1) %>% # Keep only the first matched ID if there are multiples
  ungroup()

ais_with_coords <- lake_ais_status %>%
  # NEW FIX: Revert Site.Code back to integer so it matches the lookup tables
  mutate(Site.Code = as.integer(Site.Code)) %>%
  
  # Now the joins will proceed flawlessly
  left_join(fins_coords_lookup, by = c("County", "Site.Code", "Name", "Primary.Watershed")) %>%
  left_join(deehr_id_lookup, by = c("County", "Name", "Site.Code", "Primary.Watershed")) %>%
  left_join(deehr_coords_lookup, by = "DEEHR_ID") %>%
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

# NEW: Force 1 row per lake for morphology. If there are slight variations in depth records, take the first non-NA.
lake_morphology <- fins_lakes %>%
  select(County, Name, Site.Code, Primary.Watershed, Volume..m.., Surface.Area..ha., 
         Maximum.Depth..m., Mean.Depth..m., Area.Less.6M.Deep..ha., Shoreline.Dev) %>%
  group_by(County, Name, Site.Code, Primary.Watershed) %>%
  summarise(across(everything(), ~first(na.omit(.))), .groups = "drop")

ais_final_master <- ais_with_coords %>%
  left_join(lake_morphology, by = c("County", "Name", "Site.Code", "Primary.Watershed")) %>%
  distinct() 

################################################################################
# 11. EXPORT as csv 
################################################################################
write.csv(ais_final_master, "Output/AIS_status_all_levels_FINS-DEEHR_260319.csv", row.names=FALSE)


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
write.csv(master_community_list, "Output/Community_composition_AIS_status_FINS-DEEHR_260319.csv", row.names = FALSE)

################################################################################
# 14. GOOGLE EARTH EXPORT (Single Master Nested KML)
################################################################################

# 1. Prep data (ensure clean coordinates and fill NAs)
kml_prep <- ais_final_master %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  mutate(
    County = replace_na(County, "Unknown_County"), 
    Description = paste0(
      "County: ", County, 
      "<br>Site Code: ", Site.Code, 
      "<br>Last Sampled: ", Lake_Last_Sampled_Year
    )
  ) %>%
  select(County, Name, Description, Lake_AIS_Status, Longitude, Latitude)

# 2. Setup output path
kml_path <- "Output/AIS_Status_Master_Nested.kml"

# 3. Open a direct file connection (Bypassing sf/GDAL completely)
file_conn <- file(kml_path, "w")

# Write KML Headers
writeLines('<?xml version="1.0" encoding="UTF-8"?>', file_conn)
writeLines('<kml xmlns="http://www.opengis.net/kml/2.2">', file_conn)
writeLines('  <Document>', file_conn)
writeLines('    <name>NS Lake AIS Status</name>', file_conn) # <-- FIXED

# 4. Nested Loop: Write XML tags for County -> Status -> Lake
counties <- unique(kml_prep$County)

for (c in counties) {
  writeLines('    <Folder>', file_conn)
  writeLines(paste0('      <name>', c, '</name>'), file_conn) # <-- FIXED
  
  county_data <- kml_prep %>% filter(County == c)
  statuses <- unique(county_data$Lake_AIS_Status)
  
  for (s in statuses) {
    writeLines('      <Folder>', file_conn)
    writeLines(paste0('        <name>', s, '</name>'), file_conn) # <-- FIXED
    
    status_data <- county_data %>% filter(Lake_AIS_Status == s)
    
    for (i in 1:nrow(status_data)) {
      writeLines('        <Placemark>', file_conn)
      # CDATA tags prevent Google Earth from breaking when it sees <br> tags
      writeLines(paste0('          <name><![CDATA[', status_data$Name[i], ']]></name>'), file_conn)
      writeLines(paste0('          <description><![CDATA[', status_data$Description[i], ']]></description>'), file_conn)
      writeLines('          <Point>', file_conn)
      writeLines(paste0('            <coordinates>', status_data$Longitude[i], ',', status_data$Latitude[i], ',0</coordinates>'), file_conn)
      writeLines('          </Point>', file_conn)
      writeLines('        </Placemark>', file_conn)
    }
    
    writeLines('      </Folder>', file_conn) # Close Status Folder
  }
  writeLines('    </Folder>', file_conn) # Close County Folder
}

# Write Footers and close connection
writeLines('  </Document>', file_conn)
writeLines('</kml>', file_conn)
close(file_conn)