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
fins_lakes <- read.csv("../../3_FW_survey/NS fishes docs/FINS/FINS 2025-06-05_SBmodified_lakesize.csv", header = TRUE)
fins_fish <- read.csv("../../3_FW_survey/NS fishes docs/FINS/FINS 2025-06-05_SBmodified_lakefish.csv", header = TRUE)

# Make FINS lakes spatial (Easting/Northing available)
fins_lakes_sf <- st_as_sf(fins_lakes, coords = c("Easting", "Northing"), crs = 26920) %>%
  st_transform(crs = 4326) %>%  # WGS84 (what all the shape files are in; needed for plotting)
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  )

################################################################################
# 2. LOAD DEEHR DATA
################################################################################
deehr_esni <- read.csv("../../3_FW_survey/NS fishes docs/DEEHR/DEEHR_cp_points.csv", header = TRUE)
deehr_mido <- read.csv("../../3_FW_survey/NS fishes docs/DEEHR/SMB_Sheet_Cleaned_Sharing.csv", header = TRUE)

# CLEAN & COMBINE DEEHR
deehr_esni_clean <- deehr_esni %>%
  filter(Province == "NS") %>%
  transmute(
    Species.Name = "Chain Pickerel",
    Name = str_to_upper(Location),
    County = str_to_title(Country),
    Year = as.numeric(Year),
    Easting,
    Northing,
    UTM.Zone
  )

deehr_mido_clean <- deehr_mido %>%
  transmute(
    Species.Name = "Smallmouth Bass",
    Name = str_to_upper(system),
    County = str_to_title(county),
    Year = as.numeric(year),
    UTM.Zone = zone,
    Latitude = lat,
    Longitude = lon
  )

# Convert both to sf (crs 4326 to be consistent)
deehr_esni_sf <- st_as_sf(deehr_esni_clean, coords = c("Easting", "Northing"), crs = 26920) %>% st_transform(4326)
deehr_mido_sf <- st_as_sf(deehr_mido_clean, coords = c("Longitude","Latitude"), crs = 4326)
deehr_all_sf <- bind_rows(deehr_esni_sf, deehr_mido_sf)

################################################################################
# 3. PARTIAL NAME MATCHING WITH FINS (same county)
################################################################################
fins_df <- fins_lakes_sf %>% st_drop_geometry() %>%
  select(County, Name, Site.Code, Primary.Watershed, Watershed.Code)

# Add a Temporary ID to DEEHR to track unique points
deehr_df <- deehr_all_sf %>% 
  st_drop_geometry() %>%
  mutate(Year = as.numeric(Year),
         DEEHR_ID = row_number()) # Unique identifier for each DEEHR record

matched_list <- lapply(1:nrow(deehr_df), function(i){
  county    <- deehr_df$County[i]
  name_orig <- deehr_df$Name[i]
  
  # Filter FINS by county first
  fins_sub <- fins_df %>% filter(County == county)
  
  # 1. Literal match check
  matched <- fins_sub %>%
    filter(str_detect(Name, fixed(name_orig, ignore_case = TRUE)) |
             str_detect(fixed(name_orig, ignore_case = TRUE), Name))
  
  if(nrow(matched) == 0) return(NULL)
  
  # --- THE FIX: TIE-BREAKER LOGIC ---
  if(nrow(matched) > 1){
    # Check for an exact match first
    exact <- matched %>% filter(toupper(Name) == toupper(name_orig))
    
    if(nrow(exact) == 1){
      matched <- exact
    } else {
      # Fallback: Pick the match with the most similar character count
      # (e.g., if DEEHR is "Grand", "Grand Lake" is closer than "Grand Lake Meadows")
      matched <- matched %>%
        mutate(len_diff = abs(nchar(Name) - nchar(name_orig))) %>%
        slice_min(len_diff, n = 1, with_ties = FALSE)
    }
  }
  
  # Keep DEEHR metadata for your manual review
  deehr_info <- deehr_df[i, ] %>% 
    select(DEEHR_ID, DEEHR_Name = Name, Species.Name, Year)
  
  deehr_expanded <- deehr_info[rep(1, nrow(matched)), ]
  
  bind_cols(matched, deehr_expanded)
})

deehr_matched_name <- bind_rows(matched_list)
#305/559

# Show DEEHR name side-by-side with the FINS match
# some are off by many letters! double check why not seeing it
check_remaining_duplicates <- deehr_matched_name %>%
  group_by(DEEHR_ID) %>%
  filter(n() > 1) %>%
  select(DEEHR_ID, DEEHR_Name, FINS_Name = Name, County, Site.Code)

print(check_remaining_duplicates)
#no duplicates remaining because this is taking only the first match and ignoring others


################################################################################
# 4. SPATIAL ASSIGNMENT FOR UNMATCHED POINTS (Updated for DEEHR_ID)
################################################################################

# Identify IDs that were NOT matched in the name loop
matched_ids <- unique(deehr_matched_name$DEEHR_ID)
deehr_unmatched <- deehr_all_sf %>%
  mutate(DEEHR_ID = row_number()) %>% # Ensure ID matches the one from Section 3
  filter(!(DEEHR_ID %in% matched_ids))

if(nrow(deehr_unmatched) > 0){
  # Transform to projected CRS for accurate distance measurement (meters)
  deehr_unmatched_utm <- st_transform(deehr_unmatched, 26920)
  fins_sf_utm <- st_transform(fins_lakes_sf, 26920)
  
  # Find nearest FINS lake centroid
  nearest_idx <- st_nearest_feature(deehr_unmatched_utm, fins_sf_utm)
  distances <- st_distance(deehr_unmatched_utm, fins_sf_utm[nearest_idx,], by_element = TRUE)
  
  # Set 200m tolerance
  tolerance <- set_units(200, "m")
  valid_matches <- distances <= tolerance
  
  # Filter to only those within the 200m buffer
  deehr_nearest_sf <- deehr_unmatched_utm[valid_matches, ]
  matched_lakes_sf <- fins_sf_utm[nearest_idx[valid_matches], ]
  
  # Extract metadata from the matched FINS lakes
  deehr_matched_spatial <- bind_cols(
    matched_lakes_sf %>% st_drop_geometry() %>% 
      select(County, Name, Site.Code, Primary.Watershed, Watershed.Code),
    deehr_nearest_sf %>% st_drop_geometry() %>% 
      select(Species.Name, Year)
  )
} else {
  deehr_matched_spatial <- NULL
  message("All DEEHR records were matched by name; skipping spatial assignment.")
}

################################################################################
# 5. COMBINE DEEHR POINTS
################################################################################

deehr_final <- bind_rows(
  deehr_matched_name %>% select(County, Name, Site.Code, Primary.Watershed, Watershed.Code, Species.Name, Year),
  deehr_matched_spatial
)
deehr_final$Database <- "DEEHR"

################################################################################
# 6. MERGE WITH FINS DATA FOR AIS ANALYSIS
################################################################################

# Merge FINS fish + lake info
lake_fish <- fins_fish %>%
  left_join(
    fins_lakes_sf %>% st_drop_geometry() %>% 
      select(County, Site.Code, Name, Primary.Watershed, Secondary.Watershed,
             Watershed.Code, Assessment.Date, Surface.Area..ha.),
    by = c("County", "Site.Code", "Name",
           "Primary.Watershed", "Secondary.Watershed", "Watershed.Code",
           "Captured.Date" = "Assessment.Date"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    Date = as.Date(Captured.Date, format="%d %b %Y"),
    Year = as.numeric(format(Date,"%Y"))
  )

lake_fish$Database <- "FINS"


# Combine DEEHR final detections with FINS
lake_fish_combined <- bind_rows(
  lake_fish %>% select(County, Site.Code, Name, Primary.Watershed, Watershed.Code, Species.Name, Year, Database),
  deehr_final
)

################################################################################
# 7. CALCULATE AIS STATUS PER LAKE / WATERSHED / COUNTY
################################################################################
#STEP 7A — Watershed First Detection
ais_watershed_intro <- lake_fish_combined %>%
  filter(Species.Name %in% c("Smallmouth Bass","Chain Pickerel")) %>%
  group_by(Primary.Watershed, Species.Name) %>%
  summarise(First_Year = min(Year, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(
    names_from = Species.Name,
    values_from = First_Year,
    names_prefix="WS_First_"
  ) %>%
  rename(
    WS_First_SMB = `WS_First_Smallmouth Bass`,
    WS_First_CP  = `WS_First_Chain Pickerel`
  )


#STEP 7B — Lake Summary (one row per lake)
lake_ais_summary <- lake_fish_combined %>%
  group_by(County, Site.Code, Name, Primary.Watershed) %>%
  summarise(
    Lake_First_SMB = min(Year[Species.Name=="Smallmouth Bass"], na.rm=TRUE),
    Lake_First_CP  = min(Year[Species.Name=="Chain Pickerel"], na.rm=TRUE),
    Lake_Last_Sampled_Year = max(Year, na.rm=TRUE),
    .groups="drop"
  ) %>%
  mutate(
    Lake_First_SMB = ifelse(is.infinite(Lake_First_SMB), NA, Lake_First_SMB),
    Lake_First_CP  = ifelse(is.infinite(Lake_First_CP),  NA, Lake_First_CP)
  ) %>%
  left_join(ais_watershed_intro, by="Primary.Watershed")


#STEP 7C — Believable Absence + Final Lake Status
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
      Lake_SMB_status=="Present" & Lake_CP_status!="Present" ~ "Smallmouth Bass only",
      Lake_SMB_status!="Present" & Lake_CP_status=="Present" ~ "Chain Pickerel only",
      Lake_SMB_status=="Absent" & Lake_CP_status=="Absent" ~ "No AIS",
      Lake_SMB_status=="Absent" & Lake_CP_status=="Data insufficient" ~ "No SMB_CP unknown",
      Lake_SMB_status=="Data insufficient" & Lake_CP_status=="Absent" ~ "No CP_SMB_unknown",
      Lake_SMB_status=="Data insufficient" & Lake_CP_status=="Data insufficient" ~ "Unknown",
      TRUE ~ "Data insufficient"
    )
  )

#STEP 7D — combine with fish species detections!
lake_ais_status_with_fish <- lake_fish_combined %>%
  left_join(
    lake_ais_status %>%
      select(County, Site.Code, Name, Primary.Watershed,
             Lake_SMB_status, Lake_CP_status, Lake_AIS_Status),
    by = c("County","Site.Code","Name","Primary.Watershed")
  )


################################################################################
# 8. Summarize per watershed and county
################################################################################
ws_ais_status <- lake_ais_status %>%
  group_by(Primary.Watershed) %>%
  summarise(
    WS_AIS_Status = case_when(
      # Any lake has both
      any(Lake_AIS_Status == "Both", na.rm = TRUE) ~ "Both",
      
      # SMB-only + CP-only somewhere in watershed
      any(Lake_AIS_Status == "Smallmouth Bass only", na.rm = TRUE) &
        any(Lake_AIS_Status == "Chain Pickerel only", na.rm = TRUE) ~ "Both",
      
      # CP only
      any(Lake_AIS_Status == "Chain Pickerel only", na.rm = TRUE) ~ "Chain Pickerel only",
      
      # SMB only
      any(Lake_AIS_Status == "Smallmouth Bass only", na.rm = TRUE) ~ "Smallmouth Bass only",
      
      # All lakes confidently no AIS
      all(Lake_AIS_Status == "No AIS", na.rm = TRUE) ~ "No AIS",
      
      # Otherwise (mixed with uncertainty)
      TRUE ~ "Unknown"
    )
  )


county_ais_status <- lake_ais_status %>%
  group_by(County) %>%
  summarise(
    County_AIS_Status = case_when(
      
      # Any lake with both → county is Both
      any(Lake_AIS_Status == "Both") ~ "Both",
      
      # Both species somewhere in county (SMB-only + CP-only lakes) → Both
      any(Lake_AIS_Status == "Smallmouth Bass only") & any(Lake_AIS_Status == "Chain Pickerel only") ~ "Both",
      
      # Only CP detected anywhere → Chain Pickerel only
      any(Lake_AIS_Status == "Chain Pickerel only") ~ "Chain Pickerel only",
      
      # Only SMB detected anywhere → Smallmouth Bass only
      any(Lake_AIS_Status == "Smallmouth Bass only") ~ "Smallmouth Bass only",
      
      # All lakes No AIS → No AIS
      all(Lake_AIS_Status == "No AIS") ~ "No AIS",
      
      # Any remaining mixture of partial absence/uncertain → Unknown
      TRUE ~ "Unknown"))


################################################################################
# 9. OUTPUT MASTER FILE (WITH SPATIAL DATA RESTORED)
################################################################################

### --- 9.1. EXTRACT COORDS FROM FINS --- ###

fins_coords <- fins_lakes_sf %>%
  st_drop_geometry() %>%
  select(County, Site.Code, Name, Primary.Watershed,
         Longitude, Latitude)


### --- 9.2. EXTRACT COORDS FROM DEEHR --- ###

deehr_coords <- deehr_all_sf %>%
  st_transform(4326) %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(County, Name, Species.Name, Year,
         Longitude, Latitude) %>%
  mutate(Database = "DEEHR")


### --- 9.3. STACK SAMPLING DATA (NO GEOMETRY YET) --- ###

lake_fish_clean <- lake_fish %>%
  mutate(Database = "FINS")

all_sampling <- bind_rows(
  lake_fish_clean,
  deehr_final
)


### --- 9.4. ATTACH AIS STATUS --- ###

ais_with_all_status <- all_sampling %>%
  left_join(
    lake_ais_status_with_fish,
    by = c("County","Site.Code","Name",
           "Primary.Watershed","Watershed.Code",
           "Species.Name","Year","Database"),
    relationship = "many-to-many"
  ) %>%
  left_join(ws_ais_status, by="Primary.Watershed") %>%
  left_join(county_ais_status, by="County")


### --- 9.5. REATTACH SPATIAL DATA --- ###

# First attach FINS coords (lake-level)
ais_with_all_status <- ais_with_all_status %>%
  left_join(
    fins_coords,
    by = c("County","Site.Code","Name","Primary.Watershed"),
    relationship = "many-to-many"
  )

# Then overwrite with DEEHR coords where available
ais_with_all_status <- ais_with_all_status %>%
  mutate(
    Longitude = if_else(Database == "DEEHR", NA_real_, Longitude),
    Latitude  = if_else(Database == "DEEHR", NA_real_, Latitude)
  ) %>%
  left_join(
    deehr_coords %>%
      select(County, Name, Species.Name, Year, Longitude, Latitude),
    by = c("County","Name","Species.Name","Year"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    Longitude = coalesce(Longitude.x, Longitude.y),
    Latitude  = coalesce(Latitude.x,  Latitude.y)
  ) %>%
  select(-Longitude.x, -Longitude.y, -Latitude.x, -Latitude.y)

### --- 9.6. CLEAN DUPLICATES BUT KEEP COORDS --- ###

ais_with_all_status_clean <- ais_with_all_status %>%
  group_by(
    County, Site.Code, Name,
    Primary.Watershed, Species.Name, Year
  ) %>%
  summarise(
    Database = paste(sort(unique(Database)), collapse = "+"),
    Longitude = first(Longitude),
    Latitude  = first(Latitude),
    across(
      -c(Database, Longitude, Latitude),
      first
    ),
    .groups = "drop"
  )

# omit unneeded columns
ais_with_all_status_clean <- ais_with_all_status_clean[, !(names(ais_with_all_status_clean) 
                                                           %in% c("Captured.By",
                                                                  "Notes", 
                                                                  "Origin",
                                                                  "Effort",
                                                                  "Effort.Unit",
                                                                  "Number.Caught",
                                                                  "Catch.Unit.Effort",
                                                                  "Average.Length..cm.",
                                                                  "Average.Weight..g.",
                                                                  "Historical"))]

### --- 9.7. Reorganize --- ###
ais_with_all_status_clean <- ais_with_all_status_clean %>% relocate(Year, .after = Date)
ais_with_all_status_clean <- ais_with_all_status_clean %>% relocate(Species.Name, .after = Captured.Date)
ais_with_all_status_clean <- ais_with_all_status_clean %>% relocate(Category, .after = Name)
ais_with_all_status_clean <- ais_with_all_status_clean %>% relocate(Alias, .after = Category)
ais_with_all_status_clean <- ais_with_all_status_clean %>% relocate(Latitude, Longitude, .after = Northing)
ais_with_all_status_clean <- ais_with_all_status_clean %>% relocate(Database, .after = County_AIS_Status)



################################################################################
# 10 WRITE OUTPUT
################################################################################

write.csv(
  ais_with_all_status_clean,
  "Output/AIS_status_all_levels_FINS-DEEHR_260302.csv",
  row.names=FALSE
)
