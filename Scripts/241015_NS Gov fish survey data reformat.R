# reformat NS Lake Survey Program lat and long for plotting
# LAT and LONG are in coordinate degree and not decimal degree format - need to 
# reformat for plotting 

# Load necessary libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)

# Step 1: Load and clean the data

# Load the data
fishes <- read.csv("Data/Distribution of Nova Scotia Fishes- edited.csv")
fishes <- fishes[c(2:10)]

# Step 2: Convert LONGITUDE to numeric (non-numeric values will be turned to NA)
fishes$LONGITUDE <- as.numeric(fishes$LONGITUDE)

# Remove empty or unnecessary columns/rows (if necessary)
fishes <- fishes[complete.cases(fishes[, c("LATITUDE", "LONGITUDE")]), ]

# Step 3: Convert coordinates from degrees and minutes to decimal degrees
convert_deg_min_to_dec <- function(coord) {
  degrees <- floor(coord / 100) # Extract degrees
  minutes <- coord %% 100       # Extract minutes
  dec_deg <- degrees + (minutes / 60) # Convert to decimal
  return(dec_deg)
}

# Apply the conversion function to both latitude and longitude columns
fishes$LATITUDE_DEC <- sapply(fishes$LATITUDE, convert_deg_min_to_dec)
fishes$LONGITUDE_DEC <- sapply(fishes$LONGITUDE, convert_deg_min_to_dec)

# Step 4: Make all LONGITUDE_DEC values negative
fishes$LONGITUDE_DEC <- fishes$LONGITUDE_DEC * -1

# Step 5: Save fishes_decimal_degree as new df for later plotting
write.csv(fishes, "Data/Distribution of Nova Scotia Fishes_decimal_degree_cords_for_plotting.csv", row.names = FALSE)

