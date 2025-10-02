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


# subset lakes that are ≤ 100 ha
smalllakes <- subset(lakes, Surface.Area..ha.<= 100)


# read in fish data - NOTE this path will need to be updated once I finish cleaning up my files
fish <- read.csv("../../3_FW_survey/NS fishes docs/FINS/FINS 2025-06-05_SBmodified_lakefish.csv", header = TRUE)


################################################################################
# Add lake size and species info into a new df
library(dplyr)

# some lakes have multiple entries from resampling -- keep all of these
smalllake_fish <- fish %>%
  left_join(
    smalllakes %>%
      select(County, Site.Code, Name, Primary.Watershed, Secondary.Watershed,
             Watershed.Code, Assessment.Date, Surface.Area..ha.),
    by = c("County", "Site.Code", "Name",
           "Primary.Watershed", "Secondary.Watershed", "Watershed.Code",
           "Captured.Date" = "Assessment.Date"),
    relationship = "many-to-many"
  )

# save as .csv
write.csv(smalllake_fish, "Analysis/Small_lakes_fish_detected.csv", row.names = FALSE)


################################################################################
# Oct 1, 2025 -- will assess lakes in opposite order as before

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

ais_lakes <- subset(lake_fish, Species.Name == "Smallmouth Bass" | Species.Name == "Chain Pickerel")

# save as .csv
write.csv(ais_lakes, "Analysis/AIS_lakes.csv", row.names = FALSE)


