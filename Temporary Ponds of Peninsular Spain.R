
#Temporary ponds of peninsular Spain dataset

#Data harmonisation and Figure elaboration 

# PURPOSE: This script documents the step-by-step logic used to integrate 
# multiple data sources into the final dataset.

# STATUS: INFORMATIONAL ONLY. This script is not executable as it relies 
# on data files from third-party sources that are not redistributed here.

# The resulting dataset is available both in 
# Zenodo: 
# GitHub: https://github.com/christianarnanz/Temporary_Ponds_Dataset

library(forcats)
library(spdep)
library(psych)
library(corrplot)
library(eulerr)
library(purrr)
library(lwgeom)
library(XML)
library(remotes)
library(ggplot2)
library(stringi)
library(rio)
library(readr)
library(data.table)
library(stringr)
library(readxl)
library(openxlsx)
library(fuzzyjoin)
library(stringdist)
library(sf)
library(dplyr)
library(tidyr)
library(mgrs)


# INITIALIZATION: Loading the baseline dataset

# The "Integrated dataset" serves as the starting baseline (and integrates
# sources from "Category A" and "Category B". 
# It consists of manually processed and aggregated records, 
# where each row represents a unique validated pond.

# Input format (Wide): Single-row integration of multiple data sources. 
# Columns follow the structure: [Variable]_SOURCE_1...[Variable]_SOURCE_20 
# (e.g., Name, ID, SOURCE, X_4326, Y_4326, ID_POLYGON, REFERENCE), in order to add all information
# in further steps (see the available final dataset in the repositorie). Also, 
# each row has a temporary synthetic initial ID (INITIAL_ID), and name (NAME_MATCH)
# in order to perform all operations.

path <- "your_path"
integrated_dataset <- read.csv(paste0(path, "integrated_dataset.csv")) 

# Citations should have information about the short name of the source ( citations$cita e.g. short citation of 
# an article, such as "Alonso, 1998"), and a complete reference (citations$reference e.g. "Alonso, M. (1998). Las Lagunas de la España peninsular. Limnetica, 15, 1–176.")

citations <- read.csv(paste0(path, "citations.csv")) 

# ITERATIVE INTEGRATION: Implementing the multi-stage protocol
# We proceed with the sequential addition of records from Categories C, D, and E, 
# following the spatial and fuzzy string matching protocols described in the article.
# This ensures that new information is harmonized with the existing baseline
# while avoiding redundancy. Note that previously to the integration described here, 
# all entries from each source, have been manually revised to ensure that we retain 
# only temporary ponds, as we describe in the manuscript, relying our decisions on either 
# manual inspection using Google Earth Pro imagery, the available editions of 
# the Aerial Orthophotography National Plan (PNOA), the hydrological information
# provided by the original source, or direct expert consultation.

# 1 - "Category C" sources ####

## Define the directories

#Here are the csv files with the original information. File names should have
# as names, the short citation used for the source, and the original EPSG for 
# coordinates (e.g. Blanco_2013_4326.csv). Columns must have, if available, 
# information about name, identifier, longitude, and latitude: "NAME", "ID", "X", "y" respectively.

input_dir <- "SOURCE_DATA/cat_c_csv/"

#Here are the gpkg files generated with the csv files
output_dir <- "SOURCE_DATA/cat_c_gpkg/"

# Modify the file names in input_dir
file.rename(from = list.files(input_dir, full.names = TRUE), 
            to = file.path(input_dir, gsub(" ", "_", list.files(input_dir))))


# Function to convert degrees, minutes and seconds to decimals 
convert_gms_to_decimal <- function(gms) {
  parts <- str_match(gms, "([0-9]+)[°º]?\\s*([0-9]+)?[′']?\\s*([0-9]+(?:\\.[0-9]+)?)?[″\"]?\\s*([NSEW])?")
  if (is.na(parts[1])) {
    stop("Invalid DMS format: ", gms)
  }
  degrees <- as.numeric(parts[2])
  minutes <- ifelse(is.na(parts[3]), 0, as.numeric(parts[3]))
  seconds <- ifelse(is.na(parts[4]), 0, as.numeric(parts[4]))
  direction <- parts[5]
  
  # Calculate the decimal value
  decimal <- degrees + minutes / 60 + seconds / 3600
  if (direction %in% c("S", "W")) {
    decimal <- -decimal
  }
  return(decimal)
}

# Get the list of CSV files that end with a number
csv_files <- list.files(path = input_dir, pattern = "_[0-9]+\\.csv$", full.names = TRUE)


# 1.1 - Process the CSV from each source, to generate a SF object ####

#   1.1.1 - CSV files (not Military Grid Reference System, MGRS) #### 
for (file in csv_files) {
  tryCatch({
    # Extract the file name without the path or extension
    file_name <- tools::file_path_sans_ext(basename(file))
    
    # Clean the file name (replace spaces and invalid characters)
    clean_file_name <- gsub("[^a-zA-Z0-9_]", "_", file_name)  # Replace invalid characters with _
    clean_file_name <- gsub("_+", "_", clean_file_name)       # Remove multiple _
    
    # Extract the EPSG from the file name
    epsg <- as.numeric(str_extract(file_name, "[0-9]+$"))
    cat("Processing file:", file_name, "with EPSG:", epsg, "\n")
    
    # Read the CSV file
    data <- read.csv2(file)
    
    # Check the column names
    cat("File columns:", names(data), "\n")
    
    # Find the columns closest to "x" and "y"
    x_col <- names(data)[which.min(adist("x", names(data)))]
    y_col <- names(data)[which.min(adist("y", names(data)))]
    cat("Identified coordinate columns:", x_col, "and", y_col, "\n")
    
    # Convert coordinates to numeric (just in case)
    data[[x_col]] <- as.numeric(data[[x_col]])
    data[[y_col]] <- as.numeric(data[[y_col]])
    
    # Remove rows with NA or Inf values in the coordinate columns
    data <- data[!is.na(data[[x_col]]) & !is.na(data[[y_col]]) & 
                   !is.infinite(data[[x_col]]) & !is.infinite(data[[y_col]]), ]
    cat("Rows after removing NA and Inf in coordinates:", nrow(data), "\n")
    
    # Check that there is at least one valid row
    if (nrow(data) == 0) {
      stop("No valid rows after removing NA and Inf.")
    }
    
    # Create an sf object with the coordinates
    cat("Creating sf object...\n")
    sf_data <- st_as_sf(data, coords = c(x_col, y_col), crs = epsg)
    
    # Check that the sf object was created correctly
    if (is.null(sf_data)) {
      stop("Could not create sf object.")
    }
    
    # Transform to EPSG 4326
    sf_data <- st_transform(sf_data, crs = 4326)
    
    # Generate the GPKG file (only if it doesn’t already exist)
    gpkg_file <- file.path(output_dir, paste0(clean_file_name, ".gpkg"))
    
    if (!file.exists(gpkg_file)) {
      cat("Saving GPKG file at:", gpkg_file, "\n")
      st_write(sf_data, gpkg_file, driver = "GPKG", delete_dsn = FALSE)
      cat("GPKG file generated successfully:", gpkg_file, "\n")
    } else {
      cat("File already exists, it will not be overwritten:", gpkg_file, "\n")
    }
    
  }, error = function(e) {
    cat("Error processing file:", file, "\n")
    cat("Error message:", e$message, "\n")
    
    # Print more error details
    cat("Additional details:\n")
    print(e)
  })
}

#Now we need to carefully review the output, and for those that weren’t generated correctly,
#go to the original csv, add it to QGIS, generate the gpkg, and paste it into the output_dir


#   1.1.2 - MGRS files ####

#Get the list of CSV files that contain "_MGRS" in the name
csv_files_mgrs <- list.files(path = input_dir, pattern = "_MGRS\\.csv$", full.names = TRUE)

# Process each CSV file
for (file in csv_files_mgrs) {
  tryCatch({
    # Extract the file name without the path or extension
    file_name <- tools::file_path_sans_ext(basename(file))
    
    # Clean the file name (replace spaces and invalid characters)
    clean_file_name <- gsub("[^a-zA-Z0-9_]", "_", file_name)  # Replace invalid characters with _
    clean_file_name <- gsub("_+", "_", clean_file_name)       # Remove multiple _
    
    # Read the CSV file
    cat("Processing file:", file_name, "\n")
    data <- read.csv2(file)
    
    # Check the column names
    cat("File columns:", names(data), "\n")
    
    # Find the column closest to "UTM" (MGRS coordinates)
    mgrs_col <- names(data)[which.min(adist("UTM", names(data)))]
    cat("Identified column for MGRS coordinates:", mgrs_col, "\n")
    
    # Convert MGRS coordinates to latitude and longitude
    cat("Converting MGRS coordinates to latitude/longitude...\n")
    coords <- mgrs_to_latlng(data[[mgrs_col]])
    
    # Add the converted coordinates to the data frame
    data$lat <- coords$lat
    data$lon <- coords$lng
    
    # Remove rows with NA or Inf values in the coordinate columns
    data <- data[!is.na(data$lat) & !is.na(data$lon) & 
                   !is.infinite(data$lat) & !is.infinite(data$lon), ]
    cat("Rows after removing NA and Inf in coordinates:", nrow(data), "\n")
    
    # Check that there is at least one valid row
    if (nrow(data) == 0) {
      cat("No valid rows after removing NA and Inf. Skipping this file.\n")
      next  # Skip to the next file
    }
    
    # Create an sf object with the coordinates
    cat("Creating sf object...\n")
    sf_data <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
    
    # Check that the sf object was created correctly
    if (is.null(sf_data)) {
      stop("Could not create sf object.")
    }
    
    # Generate the GPKG file
    gpkg_file <- file.path(output_dir, paste0(clean_file_name, ".gpkg"))
    cat("Saving GPKG file at:", gpkg_file, "\n")
    st_write(sf_data, gpkg_file, driver = "GPKG", delete_dsn = F)
    
    cat("GPKG file generated successfully:", gpkg_file, "\n")
  }, error = function(e) {
    cat("Error processing file:", file, "\n")
    cat("Error message:", e$message, "\n")
    
    # Print more error details
    cat("Additional details:\n")
    print(e)
  })
}

# 1.2 - Start the match ####

#   1.2.1 - Prepare the data ####


# Create the column NAME_MATCH
integrated_dataset <- integrated_dataset %>%
  mutate(NAME_MATCH = coalesce(NAME_SOURCE_1, NAME_SOURCE_2))

# Check that there are no NAs
summary(is.na(integrated_dataset$NAME_MATCH) | integrated_dataset$NAME_MATCH == "")

# Generate a spatial file with integrated_dataset
integrated_dataset_sf <- st_as_sf(integrated_dataset, coords = c("X_4326", "Y_4326"), crs = 4326)

# Reproject to EPSG 25830
integrated_dataset_sf <- st_transform(integrated_dataset_sf, crs = 25830)


## Generate a dictionary that associates each csv with each citation 

# Get the list of GPKG files in the directory
gpkg_files <- list.files(path = output_dir, pattern = "\\.gpkg$", full.names = TRUE)

# Function to remove the part from the last "_" before ".csv"
clean_name <- function(NAME) {
  NAME <- sub("_[^_]+$", "", NAME)  # Remove from the last "_" to the end
  return(NAME)
}

# Create a dictionary using fuzzy distance (JW method), in order to match observations of our citation 
# object (which has all sources names used and full references), and source names of each processed files.
# Manually check to ensure there are no errors.

diccionary <- data.frame(
  gpkg_file = gpkg_files,  # Full name of the GPKG file
  cita = sapply(basename(gpkg_files), function(file) {
    # Clean the file name
    file_clean <- clean_name(file)
    
    # Find the closest citation using fuzzy distance
    citations$cita[which.min(stringdist::stringdist(file_clean, citations$cita, method = "jw"))]
  })
)


# Now we are going to reproject each gpkg to 25830 and generate the corresponding columns

# List to store the reprojected GPKG objects
gpkg_reproyectados <- list()

gpkg_files

# Loop to process each GPKG file
for (gpkg_file in gpkg_files) {
  # Read the GPKG file
  gpkg_data <- st_read(gpkg_file)
  
  # Reproject to EPSG 4326
  gpkg_data <- st_transform(gpkg_data, crs = 4326)
  
  # Generate columns x_ref_4326 and y_ref_4326
  gpkg_data$x_ref_4326 <- st_coordinates(gpkg_data)[, 1]
  gpkg_data$y_ref_4326 <- st_coordinates(gpkg_data)[, 2]
  
  # Reproject to EPSG 25830
  gpkg_data <- st_transform(gpkg_data, crs = 25830)
  
  # Save the reprojected object in the list
  gpkg_reproyectados[[gpkg_file]] <- gpkg_data
}





#Now we have the spatial files generated in the output dir, we proceed with the match 

#   1.2.2 - Proceed with the match  ####

# Threshold for fuzzy distance (only applicable in the second round)
threshold_fuzzy <- 0.2

# List to store the final results
results <- list()

# Loop to process each GPKG file
for (gpkg_file in gpkg_files) {
  # Get the reprojected GPKG object
  gpkg_data <- gpkg_reproyectados[[gpkg_file]]
  
  # List to store the matches of this GPKG file
  matches_gpkg <- list()
  
  # Vector to store the rows of integrated_dataset_sf that already matched in the first round
 rows_with_match_100m <- c()
  
  # First round: Match at 150 meters
  for (i in 1:nrow(gpkg_data)) {
    # Get the current row of the GPKG
   row_gpkg <- gpkg_data[i, ]
    
    # Perform the spatial match (150 meters) with integrated_dataset_sf
    matches <- st_join(rows_gpkg, integrated_dataset_sf %>% select(NAME_MATCH, INITIAL_ID), 
                       join = st_is_within_distance, dist = 150, left = FALSE)
    
    # If there are matches, select the best one based on fuzzy distance
    if (nrow(matches) > 0) {
      # Check the column name that contains the name in the GPKG
      NAME_col_gpkg <- names(rows_gpkg)[which.min(adist("NAME", names(rows_gpkg)))]
      
      # Calculate fuzzy distance and select the best match
      matches <- matches %>%
        mutate(
          fuzzy_dist = stringdist::stringdist(as.character(rows_gpkg[[NAME_col_gpkg]]), 
                                              as.character(NAME_MATCH), method = "jw")
        ) %>%
        arrange(fuzzy_dist) %>%
        slice(1)  # Keep only the best match
      
      # Keep only the desired columns
      matches <- matches %>% 
        select(all_of(names(rows_gpkg)), NAME_MATCH, INITIAL_ID)
      
      # Add the row of integrated_dataset_sf that matched to the vector rows_with_match_100m
     rows_with_match_100m <- c(rows_with_match_100m, matches$INITIAL_ID)
    } else {
      # If there are no matches, keep the original GPKG row and add empty columns
      matches <-row_gpkg %>%
        mutate(NAME_MATCH = NA, INITIAL_ID = NA)
    }
    
    # Add the result to the matches list of this GPKG file
    matches_gpkg[[i]] <- matches
  }
  
  # Second round: Match at 1000 meters (excluding rows that already matched in the first round)
  for (i in 1:nrow(gpkg_data)) {
    # Get the current row of the GPKG
   row_gpkg <- gpkg_data[i, ]
    
    # If it already matched in the first round, skip this row
    if (!is.na(matches_gpkg[[i]]$INITIAL_ID)) next
    
    # Perform the spatial match (1000 meters) with integrated_dataset_sf, excluding previously matched rows
    matches <- st_join(rows_gpkg, 
                       integrated_dataset_sf %>% filter(!INITIAL_ID %in%rows_with_match_100m) %>% select(NAME_MATCH, INITIAL_ID), 
                       join = st_is_within_distance, dist = 1000, left = FALSE)
    
    # If there are matches, select the best one based on fuzzy distance
    if (nrow(matches) > 0) {
      # Check the column name that contains the name in the GPKG
      NAME_col_gpkg <- names(rows_gpkg)[which.min(adist("NAME", names(rows_gpkg)))]
      
      # Calculate fuzzy distance and select the best match
      matches <- matches %>%
        mutate(
          fuzzy_dist = stringdist::stringdist(as.character(rows_gpkg[[NAME_col_gpkg]]), 
                                              as.character(NAME_MATCH), method = "jw")
        ) %>%
        arrange(fuzzy_dist) %>%
        slice(1)  # Keep only the best match
      
      # Apply fuzzy threshold only in the second round
      if (matches$fuzzy_dist <= threshold_fuzzy) {
        # Keep only the desired columns
        matches <- matches %>% 
          select(all_of(names(rows_gpkg)), NAME_MATCH, INITIAL_ID)
      } else {
        # If fuzzy distance is greater than the threshold, discard the match
        matches <-row_gpkg %>%
          mutate(NAME_MATCH = NA, INITIAL_ID = NA)
      }
    } else {
      # If there are no matches, keep the original GPKG row and add empty columns
      matches <-row_gpkg %>%
        mutate(NAME_MATCH = NA, INITIAL_ID = NA)
    }
    
    # Update the matches list of this GPKG file
    matches_gpkg[[i]] <- matches
  }
  
  # Combine all matches of this GPKG file into a single data frame
  matches_gpkg <- do.call(rbind, matches_gpkg)
  
  # Save the final result
  results[[gpkg_file]] <- matches_gpkg
}


#   1.2.3 - Process the results

#Initialize a list to store the processed data frames
df_list <- list()

# Loop to process each element of the results list
for (gpkg_file in names(results)) {
  # Get the corresponding data frame
  df <- results[[gpkg_file]]
  
  # Add the reference column using the dictionary
  df$citation <- diccionary$cita[diccionary$gpkg_file == gpkg_file]
  

  # Define the desired columns
  selected_columns <- c("NAME", "CODE", "x_ref_4326", "y_ref_4326", 
                         "NAME_MATCH", "INITIAL_ID")
  
  # Check and add missing columns with NA values
  for (col in selected_columns) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  # Select only the desired columns, and short citation  
  df <- df %>% 
    select(all_of(selected_columns), citation )
  
   # Add the processed data frame to the list
  df_list[[gpkg_file]] <- df
}

# Combine all data frames into a single one
df_results <- do.call(rbind, df_list)

df_results <- as.data.frame(df_results)

#Now check if there are any NA values in the columns NAME, x_ref_4326, citation
summary(is.na(df_results[,c("NAME", "x_ref_4326", "citation")]))

#We now have the results in a synthetic object. Next, we will write a KML to explore

# Ensure that df_results is an sf object and is in WGS84 (EPSG:4326)
if (!inherits(df_results, "sf")) {
  # If it is not spatial, create the sf object using coordinates x_ref_4326 and y_ref_4326
  df_results <- st_as_sf(
    df_results,
    coords = c("x_ref_4326", "y_ref_4326"),
    crs = 4326  # Assign WGS84 system directly
  )
} else {
  # If it is already spatial, reproject to WGS84 (just in case)
  df_results <- st_transform(df_results, crs = 4326)
}


#   1.2.3.1 - Incorporate information



# From the df with the match results, we will do it in two parts: first
# those that have matched, and then we will add the new observations

df_results_filtered <- df_results %>% filter(!is.na(INITIAL_ID))

for (i in 1:nrow(df_results_filtered)) {
  # 1. Get current row data
  row <- df_results_filtered[i, ]
  
  # 2. Find the match in the integrated dataset using the ID
  idx <- which(integrated_dataset$INITIAL_ID == row$INITIAL_ID)
  
  # If no match is found, skip to the next iteration
  if (length(idx) == 0) next
  
  # 3. Find the first available (NA) slot among the 26 reference columns
  for (j in 1:20) {
    ref_col <- paste0("SOURCE_", j)
    
    # Check if the slot is empty
    if (is.na(integrated_dataset[[ref_col]][idx])) {
      
      # 4. Populate the data into the first available slot (j)
      integrated_dataset[[ref_col]][idx] <- row$citation
      integrated_dataset[[paste0("NAME_", j)]][idx] <- row$NAME
      integrated_dataset[[paste0("ID_", j)]][idx] <- row$CODE
      integrated_dataset[[paste0("X_4326_", j)]][idx] <- row$x_ref_4326
      integrated_dataset[[paste0("Y_4326_", j)]][idx] <- row$y_ref_4326
      
      # 5. Break the inner loop once the info is added
      break
    }
  }
}

# 1. Filter rows that do not have an INITIAL_ID (new observations)
df_results_new <- df_results %>% filter(is.na(INITIAL_ID))

# 2. Set the starting counter for the new INITIAL_IDs
new_id <- 6000  

# 3. Loop through each row of the new observations
for (i in 1:nrow(df_results_new)) {
  
  # Get the current row data from the filtered results
  row <- df_results_new[i, ]
  
  # 4. Create a new empty row using the integrated_dataset structure as a template
  # We take the first row and set all values to NA
  new_row <- integrated_dataset[1, ]
  new_row[1, ] <- NA 
  
  # 5. Assign the unique INITIAL_ID and increment the counter
  new_row$INITIAL_ID <- new_id
  new_id <- new_id + 1
  
  # 6. Populate the first set of reference columns (Position 1)
  # Ensure column names match your actual dataset 
  new_row$SOURCE_1     <- row$SOURCE
  new_row$NAME_SOURCE_1       <- row$NAME
  new_row$ID_SOURCE_1         <- row$CODE
  new_row$X_4326_SOURCE_1     <- row$x_ref_4326
  new_row$Y_4326_SOURCE_1     <- row$y_ref_4326
  
  # 7. Append the new row to the main integrated dataset
  integrated_dataset <- rbind(integrated_dataset, new_row)
}

rows_seleccionadas <- integrated_dataset %>%
  mutate(INITIAL_ID = as.numeric(INITIAL_ID)) %>%
  filter(INITIAL_ID > 6000)

# Update synthetic coordinates and NAME value for new observations

# Longitude
integrated_dataset$X_4326 <- apply(integrated_dataset[, c("X_4326", paste0("X_4326_", 1:20))], 1, function(row) {
  val <- row[1]  # First value (X_4326)
  if (is.na(val) || val == "") {
    val <- row[which(!is.na(row[-1]) & row[-1] != "")[1] + 1]  # First available reference value
  }
  return(val)
})

# Latitude
integrated_dataset$Y_4326 <- apply(integrated_dataset[, c("Y_4326", paste0("Y_4326_", 1:20))], 1, function(row) {
  val <- row[1]  # First value (Y_4326)
  if (is.na(val) || val == "") {
    val <- row[which(!is.na(row[-1]) & row[-1] != "")[1] + 1]  # First available reference value
  }
  return(val)
})

# Update the NAME column using the first available reference from NAME_SOURCE_1 to 26
integrated_dataset$Name <- apply(integrated_dataset[, c("Name", paste0("NAME_SOURCE_", 1:26))], 1, function(row) {
  
  val <- row[1]  # Check the primary Name column first
  
  # If the primary Name is missing or empty, search the reference columns
  if (is.na(val) || val == "") {
    val <- row[which(!is.na(row[-1]) & row[-1] != "")[1] + 1]
    
  }
  
  return(val)
})

# 2 - Sources from "Category D": CSV (points) and GPKG (points); and "Category E" (polygons) ####
#   2.1 - Process the files ####            

# Directories for Categories "D" and "E"

# For sources with information in csv format that requires converting to gpkg
cat_d_csv_dir <- "SOURCE_DATA/cat_d_csv/"

# For sources with information in GPKG originally or after processing
cat_d_gpkg_dir <- "SOURCE_DATA/cat_d_csv/"

# All Category E obejts, are originally SF objects
cat_e_dir <- "SOURCE_DATA/cat_e/"

csv_files_cat_d <- list.files(path = cat_d_csv_dir, pattern = "_[0-9]+\\.csv$", full.names = TRUE)



# Create SF objects for CSV files

for (file in csv_files_cat_d) {
  tryCatch({
    # Extract the file name without the path or extension
    file_name <- tools::file_path_sans_ext(basename(file))
    
    # Clean the file name (replace spaces and invalid characters)
    clean_file_name <- gsub("[^a-zA-Z0-9_]", "_", file_name)  # Replace invalid characters with _
    clean_file_name <- gsub("_+", "_", clean_file_name)       # Remove multiple _
    
    # Extract the EPSG from the file name
    epsg <- as.numeric(str_extract(file_name, "[0-9]+$"))
    cat("Processing file:", file_name, "with EPSG:", epsg, "\n")
    
    # Read the CSV file
    data <- read.csv2(file)
    
    # Check the column names
    cat("File columns:", names(data), "\n")
    
    # Find the columns closest to "x" and "y"
    x_col <- names(data)[which.min(adist("x", names(data)))]
    y_col <- names(data)[which.min(adist("y", names(data)))]
    cat("Identified coordinate columns:", x_col, "and", y_col, "\n")
    
    # Convert coordinates to numeric (just in case)
    data[[x_col]] <- as.numeric(data[[x_col]])
    data[[y_col]] <- as.numeric(data[[y_col]])
    
    # Remove rows with NA or Inf values in the coordinate columns
    data <- data[!is.na(data[[x_col]]) & !is.na(data[[y_col]]) & 
                   !is.infinite(data[[x_col]]) & !is.infinite(data[[y_col]]), ]
    cat("Rows after removing NA and Inf in coordinates:", nrow(data), "\n")
    
    # Check that there is at least one valid row
    if (nrow(data) == 0) {
      stop("No valid rows after removing NA and Inf.")
    }
    
    # Create an sf object with the coordinates
    cat("Creating sf object...\n")
    sf_data <- st_as_sf(data, coords = c(x_col, y_col), crs = epsg)
    
    # Check that the sf object was created correctly
    if (is.null(sf_data)) {
      stop("Could not create sf object.")
    }
    
    # Transform to EPSG 4326
    sf_data <- st_transform(sf_data, crs = 4326)
    
    # Generate the GPKG file (only if it doesn’t already exist)
    gpkg_file <- file.path(cat_d_gpkg_dir, paste0(clean_file_name, ".gpkg"))
    
    if (!file.exists(gpkg_file)) {
      cat("Saving GPKG file at:", gpkg_file, "\n")
      st_write(sf_data, gpkg_file, driver = "GPKG", delete_dsn = FALSE)
      cat("GPKG file generated successfully:", gpkg_file, "\n")
    } else {
      cat("File already exists, it will not be overwritten:", gpkg_file, "\n")
    }
    
  }, error = function(e) {
    cat("Error processing file:", file, "\n")
    cat("Error message:", e$message, "\n")
    
    # Print more error details
    cat("Additional details:\n")
    print(e)
  })
}

#Now we need to carefully review the output, and for those that weren’t generated correctly,
#go to the original csv, add it to QGIS, generate the gpkg, and paste it into the cat_d_gpkg_dir

# Initialize empty lists to store spatial files
points_list <- list()
polygons_list <- list()

# 1. Get a list of all spatial files in the directories
# We have either GeoPackages (.gpkg) or Shapefiles (.shp)
point_files <- list.files(cat_d_gpkg_dir, pattern = "\\.(gpkg|shp)$", full.names = TRUE)
poly_files  <- list.files(cat_e_dir,      pattern = "\\.(gpkg|shp)$", full.names = TRUE)

# 2. Process Point Files
for (file in point_files) {
  # Read the file
  data <- st_read(file, quiet = TRUE)
  
  # Use the filename (without extension) as the list name
  name_source <- tools::file_path_sans_ext(basename(file))
  points_list[[name_source]] <- data
}

# 3. Process Polygon Files
for (file in poly_files) {
  # Read the file
  data <- st_read(file, quiet = TRUE)
  
  # Use the filename (without extension) as the list name
  name_source <- tools::file_path_sans_ext(basename(file))
  polygons_list[[name_source]] <- data
}

# Combine all file paths into one vector
all_files_paths <- c(point_files, poly_files)

# Create the dictionary for these specific blocks (manually check to identify errors)
diccionary <- data.frame(
  gpkg_file = tools::file_path_sans_ext(basename(all_files_paths)), # Clean names (no ext)
  cita = sapply(all_files_paths, function(path) {
    # Clean the filename for better matching
    file_clean <- clean_name(basename(path))
    
    # Fuzzy match with your citations master list
    citations$cita[which.min(stringdist::stringdist(file_clean, citations$cita, method = "jw"))]
  }),
  stringsAsFactors = FALSE
)
    
#   2.2 - "Category D" sources, proceed with the match and results processing ####

# List to store processed point objects
processed_points <- list()

# Process each object in the list
for (i in seq_along(points_list)) {
  # Get spatial object
  point <- points_list[[i]]
  
  # Get file name (if the list has names)
  name_file <- names(points_list)[i]
  
  # If no name, use a generic one
  if (is.null(name_file)) {
    name_file <- paste0("file_", i)
  }
  
  # Look for the citation in the dictionary based on the file name
  actual_citation <- diccionary$cita[diccionary$gpkg_file == name_file]
  
  if (length(actual_citation) > 0 && !is.na(actual_citation[1])) {
    point$SOURCE <- actual_citation[1]
  } else {
    point$SOURCE <- name_file # Fallback to file name if not found
    warning(paste("Citation not found in dictionary for:", name_file))
  }
  
  # Reproject to EPSG:4326 (WGS84)
  point <- st_transform(point, crs = 4326)
  
  # Fix invalid geometries (though points are rarely invalid)
  point <- st_make_valid(point)
  
  # Check that all geometries are points
  if (!all(st_geometry_type(point) %in% c("POINT", "MULTIPOINT"))) {
    warning(paste("The file", name_file, "contains geometries that are not points."))
  }
  
  # Store processed object in the list
  processed_points[[i]] <- point
}

# Define expected columns
expected_columns <- c("SOURCE", "NAME", "CODE", "x_4326", "y_4326", "geometry")

# Function to standardize columns
standarize_colums <- function(point, expected_columns) {
  # Ensure all expected columns exist
  for (col in expected_columns) {
    if (!col %in% colnames(point)) {
      point[[col]] <- NA  # Add column with NA values
    }
  }
  
  # Select only expected columns
  point <- point[, expected_columns]
  return(point)
}

# Apply function to each sf object
processed_points <- lapply(processed_points, standarize_colums, expected_columns)

# Combine all objects into a single sf
joined_points <- do.call(rbind, processed_points)

# Transform CRS to match integrated_dataset_sf
if (st_crs(joined_points) != crs_integrated_dataset_sf) {
  joined_points <- st_transform(joined_points, crs_integrated_dataset_sf)
}

# Generate coordinates
coordenadas <- st_coordinates(joined_points)

# Add coordinate columns
joined_points <- joined_points %>%
  mutate(
    x_4326 = coordenadas[, "X"],  # X coordinate column
    y_4326 = coordenadas[, "Y"]   # Y coordinate column
  )


# Check for invalid geometries
if (any(!st_is_valid(joined_points))) {
  warning("The file joined_points contains invalid geometries. Attempting to fix...")
  joined_points <- st_make_valid(joined_points)  # Corrected: apply to joined_points
}

# Transform CRS again to match integrated_dataset_sf
if (st_crs(joined_points) != crs_integrated_dataset_sf) {
  joined_points <- st_transform(joined_points, crs_integrated_dataset_sf)
}

View(joined_points)

# Verify that there are no NA values in the "NAME" column, since it will be used for matching
summary(is.na(joined_points$NAME))

# List to store final results
results_points <- list()

# Vector to store rows from integrated_dataset_sf that already matched in the first round
rows_con_match_20m <- c()

# Reproject to ETRS89 (EPSG:25830) to avoid unit issues (distances in meters)
integrated_dataset_sf <- st_transform(integrated_dataset_sf, 25830)

# Reproject joined_points to EPSG:25830
joined_points <- st_transform(joined_points, 25830)

# First round: Match within 20 meters
for (i in 1:nrow(joined_points)) {
  # Get the current row from joined_points
  row_point <- joined_points[i, ]
  
  # Find the nearest feature in integrated_dataset_sf
  index_closer <- st_nearest_feature(row_point, integrated_dataset_sf)
  closer_match <- integrated_dataset_sf[index_closer, ]
  
  # Calculate the distance
  distancia <- st_distance(row_point, closer_match)
  
  # If distance <= 20 meters, record the match
  if (as.numeric(distancia) <= 20) {
    rows_with_match_20m <- c(rows_con_match_20m, closer_match$INITIAL_ID)
    matches <- row_point %>% mutate(INITIAL_ID = closer_match$INITIAL_ID)
  } else {
    matches <- row_point %>% mutate(INITIAL_ID = NA)
  }
  
  results_points[[i]] <- matches
}

# Second round: Match within 200 meters (excluding first-round matches)
for (i in 1:nrow(joined_points)) {
  row_point <- joined_points[i, ]
  
  # Skip if already matched
  if (!is.na(results_points[[i]]$INITIAL_ID)) next
  
  # Spatial join within 200 meters, excluding previous matches
  matches <- st_join(
    row_point,
    integrated_dataset_sf %>% 
      filter(!INITIAL_ID %in% rows_with_match_20m) %>% 
      mutate(NAME_pond = coalesce(!!!syms(paste0("NAME_", 1:20)))) %>% 
      select(INITIAL_ID, NAME_pond),
    join = st_is_within_distance,
    dist = 200,
    left = FALSE
  )
  
  # If matches exist, select the closest one
  if (nrow(matches) > 0) {
    distances <- st_distance(row_point, matches)
    matches <- matches %>%
      mutate(distancia_geom = as.numeric(distances)) %>%
      arrange(distancia_geom) %>%
      slice(1) %>%
      select(all_of(names(row_point)), INITIAL_ID)
  } else {
    matches <- row_point %>% mutate(INITIAL_ID = NA)
  }
  
  results_points[[i]] <- matches
}

# Combine all results into a single data frame
results_points <- do.call(rbind, results_points)

# Check for any NA values in NAME, and SOURCE columns
summary(is.na(df_results[,c("NAME", "SOURCE")]))

# Drop geometry for further processing
integrated_dataset <- st_drop_geometry(integrated_dataset_sf)

# Filter only matched results
results_points_filtered <- results_points %>%
  filter(!is.na(INITIAL_ID)) %>%
  st_drop_geometry()

# Update existing points with match info
for (i in 1:nrow(results_points_filtered)) {
  row <- results_points_filtered[i, ]
  idx <- which(integrated_dataset$INITIAL_ID == row$INITIAL_ID)
  if (length(idx) == 0) next
  found_source <- FALSE
  
  for (j in 1:20) {
    ref_col <- paste0("SOURCE_", j)
    if (!is.na(row$SOURCE) && row$SOURCE != "") {
      if (!is.na(integrated_dataset[[ref_col]][idx]) && integrated_dataset[[ref_col]][idx] != "") {
        if (row$SOURCE == integrated_dataset[[ref_col]][idx]) {
          integrated_dataset[[paste0("NAME_", j)]][idx] <- row$NAME
          integrated_dataset[[paste0("ID_", j)]][idx] <- row$CODE
          integrated_dataset[[paste0("X_4326_", j)]][idx] <- row$x_4326
          integrated_dataset[[paste0("Y_4326_", j)]][idx] <- row$y_4326
          found_source <- TRUE
          break
        }
      }
    }
  }
  
  if (!found_source) {
    for (j in 1:20) {
      ref_col <- paste0("SOURCE_", j)
      if (is.na(integrated_dataset[[ref_col]][idx]) || integrated_dataset[[ref_col]][idx] == "") {
        integrated_dataset[[ref_col]][idx] <- row$SOURCE
        integrated_dataset[[paste0("NAME_", j)]][idx] <- row$NAME
        integrated_dataset[[paste0("ID_", j)]][idx] <- row$CODE
        integrated_dataset[[paste0("X_4326_", j)]][idx] <- row$x_4326
        integrated_dataset[[paste0("Y_4326_", j)]][idx] <- row$y_4326
        break
      }
    }
  }
}

# Add new observations
# 1. Filter for rows that do not have an INITIAL_ID (new observations)
results_points_new <- results_points %>%
  filter(is.na(INITIAL_ID)) %>%
  st_drop_geometry()

# 2. Determine the starting ID for new records
# We add 1 to the current max to ensure all new IDs are unique
max_existing_id <- max(integrated_dataset$INITIAL_ID, na.rm = TRUE)
new_id <- max_existing_id + 1

# 3. Loop through each new observation and append it
for (i in 1:nrow(results_points_new)) {
  
  # Get the current row data
  row <- results_points_new[i, ]
  
  # 4. Create a new empty row using the integrated_dataset structure as a template
  new_row <- integrated_dataset[1, ]
  new_row[1, ] <- NA 
  
  # 5. Assign the new unique INITIAL_ID and increment the counter
  new_row$INITIAL_ID <- new_id
  new_id <- new_id + 1
  
  # 6. Populate the information into the first reference slots (Position 1)
  new_row$SOURCE_1 <- row$SOURCE
  new_row$NAME_SOURCE_1   <- row$NAME
  new_row$ID_SOURCE_1     <- row$CODE
  new_row$X_4326_SOURCE_1 <- row$x_4326
  new_row$Y_4326_SOURCE_1 <- row$y_4326
  
  # 7. Append the new row to the main dataset
  integrated_dataset <- rbind(integrated_dataset, new_row)
}

# Update coordinates
integrated_dataset$X_4326 <- apply(integrated_dataset[, c("X_4326", paste0("X_4326_", 1:20))], 1, function(row) {
  val <- row[1]
  if (is.na(val) || val == "") {
    val <- row[which(!is.na(row[-1]) & row[-1] != "")[1] + 1]
  }
  val
})

integrated_dataset$Y_4326 <- apply(integrated_dataset[, c("Y_4326", paste0("Y_4326_", 1:20))], 1, function(row) {
  val <- row[1]
  if (is.na(val) || val == "") {
    val <- row[which(!is.na(row[-1]) & row[-1] != "")[1] + 1]
  }
  val
})

# Update the Name column using the first available reference from NAME_SOURCE_1 to 26
integrated_dataset$Name <- apply(integrated_dataset[, c("Name", paste0("NAME_SOURCE_", 1:26))], 1, function(row) {
  
  val <- row[1]  # Check the primary Name column first
  
  # If the primary Name is missing or empty, search the reference columns
  if (is.na(val) || val == "") {
    val <- row[which(!is.na(row[-1]) & row[-1] != "")[1] + 1]
    
  }
  
  return(val)
})

# Convert back to sf
integrated_dataset_sf <- st_as_sf(integrated_dataset, coords = c("X_4326", "Y_4326"), crs = st_crs(4326), remove = FALSE)


#   2.3 - "Category E" sources, proceed with the match and results processing ####

id_polygon_counter <- 1  # Counter to generate unique id_polygon


# List to store the processed objects
processed_polygons <- list()

# Process each object in the list
for (i in seq_along(polygons_list)) {
  # Get the spatial object
  polygon <- polygons_list[[i]]
  
  # Get the file name (if the list has names)
  name_file <- names(polygons_list)[i]
  
  # If the list has no names, use a generic name
  if (is.null(name_file)) {
    name_file <- paste0("file_", i)
  }
  
  # Look for the citation in the dictionary based on the file name
  actual_citation <- diccionary$cita[diccionary$gpkg_file == name_file]
  
  if (length(actual_citation) > 0 && !is.na(actual_citation[1])) {
    polygon$SOURCE <- actual_citation[1]
  } else {
    polygon$SOURCE <- name_file # Fallback to file name if not found
    warning(paste("Citation not found in dictionary for polygon file:", name_file))
  }
  
  # Reproject to EPSG:4326 (WGS84)
  polygon <- st_transform(polygon, crs = 4326)
  
  # Fix invalid geometries
  polygon <- st_make_valid(polygon)
  

  # Verify that each geometry is a polygon
  if (!all(st_geometry_type(polygon) %in% c("POLYGON", "MULTIPOLYGON"))) {
    warning(paste("The file", name_file, "contains geometries that are not polygons."))
  }
  
  # Store the processed object in the list
  processed_polygons[[i]] <- polygon
}


# Define the expected columns
expected_columns <- c("SOURCE", "ID_POLYGON", "NAME", "CODE", "x_4326", "y_4326", "geometry")

# Function to standardize the columns of an sf object
standarize_colums <- function(polygon, expected_columns) {
  # Create an empty data.frame with the expected columns
  df_vacio <- st_sf(
    SOURCE = character(),
    ID_POLYGON = integer(),
    NAME = character(),
    CODE = character(),
    x_4326 = numeric(),
    y_4326 = numeric(),
    geom = st_sfc()
  )
  
  # Check and add missing columns
  for (col in expected_columns) {
    if (!col %in% colnames(polygon)) {
      polygon[[col]] <- NA  # Add the column with NA values
    }
  }
  
  # Select only the expected columns
  polygon <- polygon[, expected_columns]
  
  return(polygon)
}

# Apply the function to each object in the list
processed_polygons <- lapply(processed_polygons, standarize_colums, expected_columns)

# Merge all objects into a single sf object
joined_polygons <- do.call(rbind, processed_polygons)

# Assign a code to each polygon
joined_polygons$ID_POLYGON <- paste0(seq_len(nrow(joined_polygons)))

# Temporarily reproject to a projected coordinate system (e.g., UTM)
joined_polygons_proj <- st_transform(joined_polygons, crs = 32630)  # UTM zone 30N (adjust according to your location)

# Calculate the representative point (st_point_on_surface) in projected coordinates
joined_polygons_proj <- joined_polygons_proj %>%
  mutate(
    representative_point = st_point_on_surface(geom)
  )

# Reproject the representative point coordinates back to EPSG:4326
representative_points_4326 <- st_transform(joined_polygons_proj$representative_point, crs = 4326)

# Add the representative point coordinates to the original object
joined_polygons <- joined_polygons %>%
  mutate(
    x_4326_ref = st_coordinates(representative_points_4326)[, 1],
    y_4326_ref = st_coordinates(representative_points_4326)[, 2]
  )

colnames(joined_polygons)


# Check if the object is valid
if (any(!st_is_valid(joined_polygons))) {
  warning("The file joined_polygons contains invalid geometries. An attempt will be made to fix them.")
  joined_polygons_corrected <- st_make_valid(joined_polygons)  # Fix invalid geometries
}

# Generate the sf object. From here, remove redundant objects
integrated_dataset

integrated_dataset_sf <- st_as_sf(
  integrated_dataset %>% filter(is.na(descartar) | descartar != 1),  # Include NAs and exclude descartar == 1
  coords = c("X_4326", "Y_4326"), 
  crs = 4326, 
  remove = FALSE
)

max(integrated_dataset_sf$INITIAL_ID)
max(integrated_dataset$INITIAL_ID)
summary(is.na(integrated_dataset$X_4326))

# Get the CRS from integrated_dataset_sf
crs_integrated_dataset_sf <- st_crs(integrated_dataset_sf)

# Transform the CRS of joined_polygons to match integrated_dataset_sf
if (st_crs(joined_polygons_corrected) != crs_integrated_dataset_sf) {
  joined_polygons_corrected <- st_transform(joined_polygons_corrected, crs_integrated_dataset_sf)
}

str(joined_polygons_corrected)
unique(joined_polygons_corrected$SOURCE)


# Let's sort the polygons by size
joined_polygons_corrected$area <- st_area(joined_polygons_corrected)

# Sort the data frame by the 'area' column in ascending order
joined_polygons_corrected <- joined_polygons_corrected[order(joined_polygons_corrected$area), ]

INITIAL_ID_max <- max(integrated_dataset_sf$INITIAL_ID, na.rm = TRUE)

# Iterate over each individual polygon in joined_polygons_corrected
for (k in 1:nrow(joined_polygons_corrected)) {
  polygon <- joined_polygons_corrected[k, ]  # Extract a single polygon
  
  # Progress message
  cat("Processing polygon", k, "of", nrow(joined_polygons_corrected), "\n")
  
  # Check if the geometry is valid
  if (!st_is_valid(polygon)) {
    warning(paste("Polygon", k, "has an invalid geometry. It will be skipped."))
    next  # Skip this polygon
  }
  
  # Extract information from the polygon
  SOURCE <- if ("SOURCE" %in% colnames(polygon)) polygon$SOURCE[1] else NA
  NAME <- if ("NAME" %in% colnames(polygon)) polygon$NAME[1] else NA
  CODE <- if ("CODE" %in% colnames(polygon)) polygon$CODE[1] else NA
  x_4326_ref <- if ("x_4326_ref" %in% colnames(polygon)) polygon$x_4326_ref[1] else NA
  y_4326_ref <- if ("y_4326_ref" %in% colnames(polygon)) polygon$y_4326_ref[1] else NA
  ID_POLYGON <- if ("ID_POLYGON" %in% colnames(polygon)) polygon$ID_POLYGON[1] else NA
  
  # Perform spatial match with integrated_dataset_sf
  match <- st_intersects(integrated_dataset_sf, polygon, sparse = FALSE)
  indices_match <- which(match)  # Get indices of points intersecting the polygon
  
  if (length(indices_match) > 0) {
    # If a match exists, add information to all intersecting points in integrated_dataset_sf
    for (j in indices_match) {
      # Find the first available SOURCE_X column
      ref_col <- which(is.na(integrated_dataset_sf[j, grepl("^SOURCE_", colnames(integrated_dataset_sf))]))[1]
      
      if (!is.na(ref_col)) {
        ref_col_name <- paste0("SOURCE_", ref_col)
        integrated_dataset_sf[j, ref_col_name] <- SOURCE  # Assign reference
        
        # Add information to the corresponding columns
        if (!is.na(NAME)) {
          integrated_dataset_sf[j, paste0("NAME_", ref_col)] <- NAME
        }
        if (!is.na(CODE)) {
          integrated_dataset_sf[j, paste0("ID_", ref_col)] <- CODE
        }
        
        integrated_dataset_sf[j, paste0("ID_POLYGON_ref_", ref_col)] <- ID_POLYGON
      }
    }
  } else if (sum(match) == 0) {
    # Only if there is NO match
    # If no match, convert integrated_dataset_sf to a temporary data.frame
    integrated_dataset_df <- as.data.frame(integrated_dataset_sf)
    
    # Create a new row in integrated_dataset_df
    new_row <- integrated_dataset_df[1, ]  # Create an empty row with the same structure
    new_row <- new_row %>% mutate_all(~ NA)  # Assign NA to all columns
    
    # Add information to the first available reference
    new_row$SOURCE_1 <- SOURCE
    if (!is.na(NAME)) {
      new_row$NAME_SOURCE_1 <- NAME
    }
    if (!is.na(CODE)) {
      new_row$ID_SOURCE_1 <- CODE
    }
    
    
    # Add centroid coordinates
    new_row$ID_POLYGON_SOURCE_1 <- ID_POLYGON
    new_row$INITIAL_ID <- INITIAL_ID_max
    INITIAL_ID_max <- INITIAL_ID_max + 1
    
    # Update X_4326 and Y_4326 columns with new coordinates, and sinthetic name
    new_row$X_4326 <- x_4326_ref
    new_row$Y_4326 <- y_4326_ref
    
    if (!is.na(Name)) {
      new_row$Name <- NAME_SOURCE_1
    }
    
    # Add the new row to integrated_dataset_df
    integrated_dataset_df <- rbind(integrated_dataset_df, new_row)
    
    # Ensure X_4326 and Y_4326 columns are numeric
    integrated_dataset_df$X_4326 <- as.numeric(integrated_dataset_df$X_4326)
    integrated_dataset_df$Y_4326 <- as.numeric(integrated_dataset_df$Y_4326)
    
    # Convert integrated_dataset_df back to an sf object
    integrated_dataset_sf <- st_as_sf(integrated_dataset_df, coords = c("X_4326", "Y_4326"), crs = 4326, remove = FALSE)
    
    # Validate that integrated_dataset_sf is a valid sf object
    if (!inherits(integrated_dataset_sf, "sf")) {
      stop("Error: integrated_dataset_sf is not a valid sf object after conversion.")
    }
  }
}

#Finally, complete the "REFERENCE_SOURCE_X" columns with the citations object
# Loop to process the 20 SOURCE and REFERENCE_SOURCE columns
for (i in 1:20) {
  # Dynamically construct the column names
  col_source <- paste0("SOURCE_", i)
  col_ref    <- paste0("REFERENCE_SOURCE_", i)
  
  # 1. Ensure the reference column exists in the dataset
  if (!(col_ref %in% colnames(integrated_dataset_sf))) {
    integrated_dataset_sf[[col_ref]] <- NA_character_
  }
  
  # 2. Identify rows where SOURCE_X is not NA
  rows_to_update <- !is.na(integrated_dataset_sf[[col_source]])
  
  if (any(rows_to_update)) {
    # Extract the source citation strings for these rows
    current_sources <- integrated_dataset_sf[[col_source]][rows_to_update]
    
    # 3. Match current_sources against citations$cita
    # match() finds the index of each current_source inside the citations$cita vector
    match_indices <- match(current_sources, citations$cita)
    
    # 4. Assign the corresponding reference value to the REFERENCE_SOURCE_X column
    # If a match is not found, it will naturally assign NA
    integrated_dataset_sf[[col_ref]][rows_to_update] <- citations$reference[match_indices]
  }
}



# 3 - Add information about Natura 2000 and other Protected Areas ####

# Load Protected Natural Areas (PAs) and Natura 2000 Network 

# Link to World Database on Protected Areas:
# "https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA"

# Link to Natura 2000 Network:
# "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/rednatura_2000_desc.html"

# Load Protected Areas (excluding Natura 2000 sites). 
# Note: N2K sites were manually removed from this layer to avoid overlap 
# with the specific Spanish N2K cartography, which is more detailed.
enp <- st_read("protected_areas_without_N2K.gpkg")

# Load the official Natura 2000 Network layer
n2k <- st_read("natura_2000_network.shp")

# Data Cleaning: IUCN Categories 

# Key field: enp$IUCN_CAT
# This contains IUCN protection categories (Roman numerals and characters: Ia, Ib, II, III...)

# Identify strings that represent a lack of specific criteria
no_criteria <- c("Not Applicable", "Not Assigned", "Not Reported")

# Standardize these values to R's NA_character_
enp <- enp %>%
  mutate(
    IUCN_CAT = case_when(
      IUCN_CAT %in% no_criteria ~ NA_character_,
      TRUE ~ IUCN_CAT
    )
  )

# Data Cleaning: Designations

# Key field: enp$DESIG_ENG 
# This contains the type of protected area in English. 
# We correct a common typo ("Nacional" to "National").

enp <- enp %>%
  mutate(DESIG_ENG = str_replace(DESIG_ENG, "Nacional Park", "National Park"))
# Correct "Nacional Park" to "National Park" in the DESIG_ENG column
enp <- enp %>%
  mutate(DESIG_ENG = str_replace(DESIG_ENG, "Nacional Park", "National Park"))

# Define the priority order for IUCN categories (strictness of protection)
orden_IUCN <- c("Ia", "Ib", "II", "III", "IV", "V", "VI")

# Perform a Spatial Join
# We add a row_id to keep track of original geometries
joined <- st_join(integrated_dataset_sf %>% mutate(row_id = row_number()),
                  enp %>% select(DESIG_ENG, IUCN_CAT, NAME),
                  join = st_intersects, left = TRUE)

# Consolidate Attributes (non-spatial processing)
# We group by row_id to handle cases where one feature intersects multiple protected areas
attrs <- joined %>%
  st_drop_geometry() %>%
  group_by(row_id) %>%
  summarise(
    # Combine all unique designations into a single string
    protected_areas = if(all(is.na(DESIG_ENG))) "Unprotected" else paste(na.omit(unique(DESIG_ENG)), collapse = "; "),
    
    # Combine all unique protected area names into a single string
    protected_area_name = if(all(is.na(NAME))) "Unprotected" else paste(na.omit(unique(NAME)), collapse = "; "),
    
    # Determine the maximum protection level for each site
    max_protection = if(all(is.na(DESIG_ENG))) {
      "Unprotected"
    } else if(all(is.na(IUCN_CAT))) {
      # Fallback priority logic based on designation if IUCN category is missing
      prio <- case_when(
        DESIG_ENG == "Ramsar Site" ~ 1,
        DESIG_ENG == "Natural Park" ~ 2,
        DESIG_ENG == "Protected Wetland" ~ 3,
        DESIG_ENG == "Periurban Protected Area" ~ 4,
        TRUE ~ 5
      )
      DESIG_ENG[which.min(prio)][1]
    } else {
      # Select the designation corresponding to the highest IUCN protection level (Ia > Ib > II...)
      DESIG_ENG[which.min(as.numeric(factor(IUCN_CAT, levels = c("Ia","Ib","II","III","IV","V","VI"), ordered = TRUE)))][1]
    },
    .groups = "drop"
  )

# Re-join attributes with the original geometries
# We use row_id to map the summarized data back to the spatial features
integrated_dataset_sf_PA <- integrated_dataset_sf %>%
  mutate(row_id = row_number()) %>%
  left_join(attrs, by = "row_id") %>%
  select(-row_id)

# Harmonize Coordinate Reference Systems (CRS)
n2k <- st_transform(n2k, st_crs(integrated_dataset_sf_PA))

# Repair geometries in both layers to avoid errors during spatial operations
integrated_dataset_sf_PA <- st_make_valid(integrated_dataset_sf_PA)
n2k <- st_make_valid(n2k)

# Natura 2000 Processing 

# Create a temporary ID for grouping
integrated_dataset_sf_PA <- integrated_dataset_sf_PA %>%
  mutate(row_id = row_number())

# Perform Spatial Join with Natura 2000 Network (including the "TIPO" field)
joined_natura <- st_join(
  integrated_dataset_sf_PA,
  n2k %>% select(SITE_CODE, SITE_NAME, TIPO),
  join = st_intersects,
  left = TRUE
)

# Summarize attributes (dropping geometry for faster processing)
attrs_natura <- joined_natura %>%
  st_drop_geometry() %>%
  group_by(row_id) %>%
  summarise(
    # Concatenate unique Site Codes if they exist
    natura2000_code = if (all(is.na(SITE_CODE)))
      NA_character_
    else
      paste(na.omit(unique(SITE_CODE)), collapse = "; "),
    
    # Concatenate unique Site Names if they exist
    natura2000_name = if (all(is.na(SITE_NAME)))
      NA_character_
    else
      paste(na.omit(unique(SITE_NAME)), collapse = "; "),
    
    # Concatenate unique Site Types (A, B, or C)
    natura2000_tipo = if (all(is.na(TIPO)))
      NA_character_
    else
      paste(sort(unique(na.omit(TIPO))), collapse = "; "),
    
    # Create a descriptive designation variable based on Natura 2000 types
    # A = Special Protection Areas (SPA/ZEPA)
    # B = Sites of Community Importance/Special Areas of Conservation (SCI/ZEC)
    # C = Sites where both designations overlap
    N2K_site_designation = case_when(
      is.na(natura2000_tipo) ~ "Unprotected",
      natura2000_tipo == "A" ~ "ZEPA",
      natura2000_tipo == "B" ~ "ZEC/LIC",
      natura2000_tipo == "C" ~ "ZEC/LIC & ZEPA",
      natura2000_tipo %in% c("A;B", "B;A", "A; B", "B; A") ~ "ZEC/LIC & ZEPA",
      TRUE ~ natura2000_tipo
    ),
    
    .groups = "drop"
  )
# Re-join the summarized columns with the original geometries
integrated_dataset_sf_PA_N2K <- integrated_dataset_sf_PA %>%
  left_join(attrs_natura, by = "row_id") %>%
  select(-row_id)

# --- Renaming Section ---
# Rename the final variables according to your specific nomenclature
integrated_dataset_sf_PA_N2K <- integrated_dataset_sf_PA_N2K %>%
  rename(
    N2K_CODE                   = natura2000_code,
    N2K_NAME                   = natura2000_name,
    OTHER_PAs_max_protection   = max_protection,
    OTHER_PAs_site_designation = protected_areas,
    OTHER_PAs_site_name        = protected_area_name
  )


# 4 - Add Final ID based on administrative locations (provinces) #### 

# Link to official cartography for Spanish provincial boundaries:
# "https://centrodedescargas.cnig.es/CentroDescargas/limites-municipales-provinciales-autonomicos"

# Load the provincial boundary shapefile
provinces <- st_read("recintos_provinciales_inspire_peninbal_etrs89.shp")

# Transform to the project's standard CRS (EPSG:25830) and repair geometries
provinces <- st_transform(provinces, 25830)
provinces <- st_make_valid(provinces)

# Ensure the dataset matches the provinces' CRS before the spatial join
integrated_dataset_sf_PA_N2K <- st_transform(integrated_dataset_sf_PA_N2K, st_crs(provinces))

# Perform a geometry validation on both layers to ensure topological consistency
integrated_dataset_sf_PA_N2K <- st_make_valid(integrated_dataset_sf_PA_N2K)
provinces <- st_make_valid(provinces)

# Spatial Join and correct renaming
# We assign the province name (NAMEUNIT) to each feature using an intersection join
integrated_dataset_sf_PA_N2K <- integrated_dataset_sf_PA_N2K %>%
  st_join(provinces %>% select(NAMEUNIT), join = st_intersects, left = TRUE) %>%
  mutate(province = NAMEUNIT) %>%
  select(-NAMEUNIT)

# Debugging/Verification: Check province assignment results
names(provinces)
integrated_dataset_sf_PA_N2K$province

# Separate the lagoons that did not receive a province assignment (NA)
# This often happens if a site is located just outside the coastline or on a border
lagunas_na <- integrated_dataset_sf_PA_N2K %>% filter(is.na(province))
lagunas_ok <- integrated_dataset_sf_PA_N2K %>% filter(!is.na(province))
# Fix Missing Administrative Assignments 

# For each lagoon with NA (those that didn't intersect a polygon), 
# find the index of the nearest province feature.
nearest_provinces <- st_nearest_feature(lagunas_na, provinces)

# Assign the name of the nearest province using the indices obtained above.
lagunas_na$province <- as.character(provinces$NAMEUNIT[nearest_provinces])

# Re-combine the successfully joined lagoons with the newly fixed ones.
integrated_dataset_sf_PA_N2K <- bind_rows(lagunas_ok, lagunas_na)

#  Province Names to Short Codes

# Assign standardized 3 or 4-letter codes to each province of Spain.
prov_codes <- tribble(
  ~province_name,           ~province_code,
  "A Coruña",                "CORU",
  "Alacant/Alicante",        "ALI",
  "Albacete",                "ALB",
  "Almería",                 "ALM",
  "Araba/Álava",             "ALA",
  "Asturias",                "AST",
  "Ávila",                   "AVI",
  "Badajoz",                 "BAD",
  "Barcelona",               "BAR",
  "Bizkaia",                 "VIZ",
  "Burgos",                  "BUR",
  "Cáceres",                 "CAC",
  "Cádiz",                   "CAD",
  "Cantabria",               "CANT",
  "Castelló/Castellón",      "CAS",
  "Ceuta",                   "CEU",
  "Ciudad Real",             "CRE",
  "Córdoba",                 "CORD",
  "Cuenca",                  "CUE",
  "Gipuzkoa",                "GUI",
  "Girona",                  "GIR",
  "Granada",                 "GRA",
  "Guadalajara",             "GUA",
  "Huelva",                  "HUEL",
  "Huesca",                  "HUES",
  "Illes Balears",           "BAL",
  "Jaén",                    "JAE",
  "La Rioja",                "RIO",
  "León",                    "LEO",
  "Lleida",                  "LLE",
  "Lugo",                    "LUG",
  "Madrid",                  "MAD",
  "Málaga",                  "MAL",
  "Melilla",                 "MEL",
  "Murcia",                  "MUR",
  "Navarra",                 "NAV",
  "Ourense",                 "OUR",
  "Palencia",                "PALE",
  "Pontevedra",              "PON",
  "Salamanca",               "SAL",
  "Segovia",                 "SEG",
  "Sevilla",                 "SEV",
  "Soria",                   "SOR",
  "Tarragona",               "TAR",
  "Teruel",                  "TER",
  "Toledo",                  "TOL",
  "València/Valencia",       "VALE",
  "Valladolid",              "VALL",
  "Zamora",                  "ZAM",
  "Zaragoza",                "ZAR"
)

# Join the province codes to your dataset.
integrated_dataset_sf_PA_N2K <- integrated_dataset_sf_PA_N2K %>%
  left_join(prov_codes, by = c("province" = "province_name"))

# Create a unique ID consisting of the Province Code + a 3-digit counter.

# The counter resets for each province (e.g., MAD001, MAD002, ZAM001).
integrated_dataset_sf_PA_N2K <- integrated_dataset_sf_PA_N2K %>%
  group_by(province_code) %>%
  mutate(ID = str_c(province_code, str_pad(row_number(), width = 3, pad = "0"))) %>%
  ungroup() %>% 
  select(-INITIAL_ID) #delete temporary original ID used in the workflow



# 5 - Figures ####

#If you want to replicate the figures from the available dataset in the repositories, complete and activate the code line below, : 

integrated_dataset_sf_PA_N2K <- read_csv(".../Temporary_Ponds_of_Peninsular_Spain_V1.csv")

#   5.1 - Figure 2 ####


# Create a vector containing the names of the source columns (1 to 20)
source_cols <- paste0("SOURCE_", 1:20)

# Count how many sources each lagoon has
data_sources_count <- integrated_dataset_sf_PA_N2K %>%
  rowwise() %>%
  mutate(
    # Count non-NA values across the 20 source columns
    n_sources = sum(!is.na(c_across(all_of(source_cols))))
  ) %>%
  ungroup() %>%
  # Replace NA counts with 0 (for new observations without citations)
  mutate(n_sources = ifelse(is.na(n_sources), 0, n_sources))

# Categorize counts: group sites with 10 or more sources and label sites with zero
data_sources_count <- data_sources_count %>%
  mutate(
    n_sources_cat = case_when(
      n_sources >= 10 ~ "10+",
      n_sources == 0  ~ "0",
      TRUE            ~ as.character(n_sources)
    )
  )

# Calculate the percentage of lagoons for each source count category
percent_sources <- data_sources_count %>%
  count(n_sources_cat) %>%
  mutate(percent = n / sum(n) * 100)

# Define a fixed order for the factor levels: 0, 1, 2, ..., 9, 10+
levels_order <- c("0", as.character(1:9), "10+")

percent_sources <- percent_sources %>%
  mutate(n_sources_cat = factor(n_sources_cat, levels = levels_order))

# Generate the bar chart
ggplot(percent_sources, aes(x = factor(n_sources_cat), y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(percent, 2), "%")), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Distribution of Information Sources per Site",
    x = "Number of sources per pond",
    y = "Percentage",
  ) +
  theme_minimal()

#   5.2 - Figure 3 ####

# Node Preparation 
# We clean up the designation labels for the diagram nodes
ponds_chord <- integrated_dataset_sf_PA_N2K %>%
  st_drop_geometry() %>%
  mutate(
    # Standardize Natura 2000 categories
    N2K_node = case_when(
      is.na(N2K_site_designation) ~ "Unprotected_N2K",
      N2K_site_designation == "Unprotected" ~ "Unprotected_N2K",
      TRUE ~ N2K_site_designation
    ),
    # Standardize Other Protected Area categories
    OTHER_PAs_node = case_when(
      is.na(OTHER_PAs_max_protection) ~ "Unprotected_Other_PAs",
      OTHER_PAs_max_protection == "Unprotected" ~ "Unprotected_Other_PAs",
      TRUE ~ OTHER_PAs_max_protection
    )
  )

# Group Minority Categories for Other PAs
# Categories with very low frequency (under 0.5%) are grouped to reduce diagram noise
other_counts <- ponds_chord %>%
  count(OTHER_PAs_node) %>%
  mutate(
    percentage = n / sum(n) * 100,
    OTHER_PAs_node_grouped = ifelse(percentage < 0.5, "Other designations", OTHER_PAs_node)
  )

# Join the grouped categories back to the main dataset
ponds_chord <- ponds_chord %>%
  left_join(other_counts %>% select(OTHER_PAs_node, OTHER_PAs_node_grouped), by = "OTHER_PAs_node") %>%
  mutate(OTHER_PAs_node = OTHER_PAs_node_grouped) %>%
  select(-OTHER_PAs_node_grouped)

# Create Frequency Table for Chord Diagram
table_chord <- ponds_chord %>%
  count(N2K_node, OTHER_PAs_node) %>%
  filter(n > 0)

# Color Palette Definition

# Natura 2000 Green Scale
n2k_colors <- c(
  "Unprotected_N2K" = "#D3D3D3",       # Grey
  "ZEPA" = "#6FBF92",                  # Light Green
  "ZEC/LIC" = "#4FA875",               # Intermediate Green
  "ZEC/LIC y ZEPA" = "#1F7A5F"         # Dark Green
)

# Use the Okabe-Ito palette (color-blind friendly) for Other Protected Areas
other_pa_levels <- unique(ponds_chord$OTHER_PAs_node)
okabe_ito_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
other_pa_colors <- setNames(okabe_ito_colors[1:length(other_pa_levels)], other_pa_levels)

# Generate Chord Diagram
# This visualizes the flow and overlaps between N2K and Other PAs
chordDiagram(
  x = table_chord,
  grid.col = c(n2k_colors, other_pa_colors),
  transparency = 0.3,
  annotationTrack = c("name", "grid"),
  preAllocateTracks = 1,
  # Define the circular order of the nodes
  order = c("Unprotected_N2K", "ZEPA", "ZEC/LIC", "ZEC/LIC y ZEPA",
            "Unprotected_Other_PAs", sort(other_pa_levels[other_pa_levels != "Unprotected_Other_PAs"]))
)
