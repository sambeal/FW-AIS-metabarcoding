################################################################################
# Exploration of FINS lake data set to choose lakes for Ch. 3 - FW survey of NS
# to assess the impacts of AIS on natural FW fish biodiversity

# will use the same lake selection parameters as EC Honours

# am assessing lakes firstly on a county basis, then will look per watershed 
# per county

# FINS data set is an excel file and I have saved the two sheets of interest
# as separate csvs

# will firstly create a short list of lakes in the appropriate range (≤ 100 ha)
# then will comb through those to see what species were detected and when

# final output will be a list of lakes with AIS invasion status
################################################################################

# read in lake data ----
#NOTE this path will need to be updated once I finish cleaning up my files
lakes <- read.csv("../../3_FW_survey/NS fishes docs/FINS/FINS 2025-06-05_SBmodified_lakesize.csv", header = TRUE)

# explore number of unique lakes
library(dplyr)
unique_lake_count <- lakes %>%
  distinct(County, Site.Code, Name, Watershed.Code) %>%  # keep unique combinations
  nrow()  # count them
# 1180

# number of unique lakes <-≤ 100 ha
unique_lakes_under_100 <- lakes %>%
  filter(Surface.Area..ha. < 100) %>%
  distinct(County, Site.Code, Name, Watershed.Code)

# 880

# read in fish data - NOTE this path will need to be updated once I finish cleaning up my files
fish <- read.csv("../../3_FW_survey/NS fishes docs/FINS/FINS 2025-06-05_SBmodified_lakefish.csv", header = TRUE)


################################################################################
# Add lake size and species info into a new df
library(dplyr)

# Invasion status first, then down the line filter by lake size
# still need to combine the different sheets of the FINS file
# some lakes have multiple entries from resampling -- keep all of these
lake_fish <- fish %>%
  left_join(
    lakes %>%
      select(County, Site.Code, Name, Primary.Watershed, Secondary.Watershed,
             Watershed.Code, Assessment.Date, Surface.Area..ha.),
    by = c("County", "Site.Code", "Name",
           "Primary.Watershed", "Secondary.Watershed", "Watershed.Code",
           "Captured.Date" = "Assessment.Date"),
    relationship = "many-to-many"
  )


# reformat capture date into individual components
lake_fish <- lake_fish %>%
  mutate(
    Date = as.Date(Captured.Date, format = "%d %b %Y"),
    Year = as.numeric(format(Date, "%Y"))
  )

# Rearrange
lake_fish <- lake_fish %>%
  relocate(Date, Year, .after = Captured.Date)

# Summarize first detection per watershed, not per lake
library(tidyr)

# 1. Calculate first detection per watershed
ais_watershed_intro <- lake_fish %>%
  filter(Species.Name %in% c("Smallmouth Bass", "Chain Pickerel")) %>%
  group_by(Primary.Watershed, Species.Name) %>%
  summarise(First_Year = min(Year, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Species.Name,
              values_from = First_Year,
              names_prefix = "First_Year_in_WS_")

ais_watershed_intro <- ais_watershed_intro %>%
  rename(
    WS_First_SMB = `First_Year_in_WS_Smallmouth Bass`,
    WS_First_CP  = `First_Year_in_WS_Chain Pickerel`
  )

# Join this back lake_fish dataset
watersheds_invaded <- lake_fish %>%
  left_join(ais_watershed_intro, by = "Primary.Watershed")

# omit unneeded columns
watersheds_invaded_clean <- watersheds_invaded[, !(names(watersheds_invaded) %in% c("Captured.By","Notes", "Origin",
                                                                                    "Effort","Effort.Unit","Number.Caught",
                                                                                    "Catch.Unit.Effort","Average.Length..cm.",
                                                                                    "Average.Weight..g.","Historical"))]
# determine when AIS were detected in each LAKE
lake_summary <- watersheds_invaded_clean %>%
  group_by(County, Site.Code, Name, Primary.Watershed) %>%
  summarise(
    Lake_First_SMB = min(Year[Species.Name == "Smallmouth Bass"], na.rm = TRUE),
    Lake_First_CP  = min(Year[Species.Name == "Chain Pickerel"], na.rm = TRUE),
    Last_Sampled_Year = max(Year, na.rm = TRUE),
    WS_First_SMB = first(WS_First_SMB),
    WS_First_CP  = first(WS_First_CP),
    .groups = "drop"
  )

# turn INFs into NAs
lake_summary <- lake_summary %>%
  mutate(
    Lake_First_SMB = ifelse(is.infinite(Lake_First_SMB), NA, Lake_First_SMB),
    Lake_First_CP  = ifelse(is.infinite(Lake_First_CP),  NA, Lake_First_CP)
  )

# Define when AIS was possible in each lake
# (From what year onward could this lake reasonably contain AIS?)
lake_summary <- lake_summary %>%
  mutate(
    WS_First_AIS = pmin(WS_First_SMB, WS_First_CP, na.rm = TRUE)
  )

# Flag whether absence of AIS is trustworthy
lake_summary <- lake_summary %>%
  mutate(
    Sampled_Post_Invasion = Last_Sampled_Year >= WS_First_AIS
  )

lake_AIS_summary <- lake_summary %>%
  mutate(
    Lake_AIS_Status = case_when(
      !is.na(Lake_First_SMB) & !is.na(Lake_First_CP) ~ "Both",
      !is.na(Lake_First_SMB) &  is.na(Lake_First_CP) ~ "Smallmouth Bass only",
      is.na(Lake_First_SMB)  & !is.na(Lake_First_CP) ~ "Chain Pickerel only",
      is.na(Lake_First_SMB)  & is.na(Lake_First_CP) & Sampled_Post_Invasion ~ "No AIS",
      TRUE ~ "Unknown / insufficient data"
    )
  )

# quick geography check
lake_AIS_summary_table <- as.data.frame.matrix(table(lake_AIS_summary$County, lake_AIS_summary$Lake_AIS_Status))
WS_AIS_summary_table <- as.data.frame.matrix(table(lake_AIS_summary$Primary.Watershed, lake_AIS_summary$Lake_AIS_Status))



# add AIS status back to watersehd 
watersheds_invaded_with_status <- watersheds_invaded_clean %>%
  left_join(
    lake_AIS_summary %>%
      select(
        County, Site.Code, Name, Primary.Watershed,
        Lake_First_SMB, Lake_First_CP,
        Last_Sampled_Year,
        WS_First_AIS,
        Sampled_Post_Invasion,
        Lake_AIS_Status
      ),
    by = c("County", "Site.Code", "Name", "Primary.Watershed")
  )


# save
write.csv(watersheds_invaded_with_status, "Analysis/AIS_lakes_260116.csv", row.names = FALSE)

