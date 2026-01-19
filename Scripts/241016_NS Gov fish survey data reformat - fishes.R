# Explore the NS Gov fish data set
fishes <- read.csv("Data/Distribution of Nova Scotia Fishes-1.csv")

# Step 1: Data manipulation
# Change data types for later plotting
fishes <- fishes %>%
  mutate(across(1:5, as.factor))

# Reshape data
library(dplyr)
library(tidyr)

# Reshape the species columns into a single column
fishes_long <- fishes %>%
  pivot_longer(cols = starts_with("Species"), 
               names_to = "Species_Column", 
               values_to = "Species.Detection") %>%
  filter(Species.Detection != "")  # Remove rows with missing species detections

# Check the resulting data
head(fishes_long)


# Step 2: Convert LONGITUDE to numeric, turning non-numeric values into NA
fishes_long$COORDINATES.LONGITUDE..DEGREES..MINUTES. <- as.numeric(fishes_long$COORDINATES.LONGITUDE..DEGREES..MINUTES.)

# Check for NA values before proceeding
summary(fishes_long$COORDINATES.LONGITUDE..DEGREES..MINUTES.)

# Remove rows where either LATITUDE or LONGITUDE is NA
fishes_long <- fishes_long[complete.cases(fishes_long[, c("COORDINATES.LATITUDE..DEGREES..MINUTES.", "COORDINATES.LONGITUDE..DEGREES..MINUTES.")]), ]

# Step 3: Convert coordinates from degrees and minutes to decimal degrees
convert_deg_min_to_dec <- function(coord) {
  degrees <- floor(coord / 100) # Extract degrees
  minutes <- coord %% 100       # Extract minutes
  dec_deg <- degrees + (minutes / 60) # Convert to decimal
  return(dec_deg)
}

# Apply the conversion function to both LATITUDE and LONGITUDE
fishes_long$LATITUDE_DEC <- sapply(fishes_long$COORDINATES.LATITUDE..DEGREES..MINUTES., convert_deg_min_to_dec)
fishes_long$LONGITUDE_DEC <- sapply(fishes_long$COORDINATES.LONGITUDE..DEGREES..MINUTES., convert_deg_min_to_dec)

# Check the resulting data
head(fishes_long)

# Step 4: Make all LONGITUDE_DEC values negative
fishes_long$LONGITUDE_DEC <- fishes_long$LONGITUDE_DEC * -1

# Step 5: Save fishes_decimal_degree as new df for later plotting
write.csv(fishes_long, "Data/Distribution of Nova Scotia Fishes_fishes_decimaldegree_for_plotting.csv", row.names = FALSE)
