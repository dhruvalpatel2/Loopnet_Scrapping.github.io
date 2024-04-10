
library(ggplot2)
library(ggmap)

head(rounded_final_updated_cleaned_file)

register_google(key = "AIzaSyCRgZvJtcGfxELdwDSHwflRgZVm4cYuIHQ") 


# Use the sapply function to iterate over the 'Property URL' column and geocode each address
geocode_results <- sapply(rounded_final_updated_cleaned_file$Property.URL, function(x) {
  result <- geocode(x, output = "latlon", source = "google")
  return(result)
})

# The geocode_results object now contains the latitude and longitude for each address
# Convert it into a more usable format if necessary and merge with your original dataframe
latitudes <- geocode_results[1, ]
longitudes <- geocode_results[2, ]

# Add the latitude and longitude as new columns to your dataframe
rounded_final_updated_cleaned_file$Latitude <- latitudes
rounded_final_updated_cleaned_file$Longitude <- longitudes


rounded_final_updated_cleaned_file$ZIPCode <- substr(rounded_final_updated_cleaned_file$Property.URL, nchar(rounded_final_updated_cleaned_file$Property.URL)-4, nchar(rounded_final_updated_cleaned_file$Property.URL))

# Check the first few rows to verify the new column

write.csv(rounded_final_updated_cleaned_file, file = "shiny_app_data.csv", row.names = FALSE)










library(httr)
library(jsonlite)

extract_zip_from_address <- function(address, api_key) {
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json"
  response <- GET(url = base_url, query = list(address = address, key = api_key))
  data <- fromJSON(rawToChar(response$content), flatten = TRUE)
  
  if(data$status == "OK" && length(data$results) > 0) {
    # Loop through the results and then through the address components of each result
    for(result in data$results) {
      # Ensure that result is a list and contains address_components
      if(is.list(result) && "address_components" %in% names(result)) {
        for(component in result$address_components) {
          # Check if this component is the postal code
          if("postal_code" %in% component$types) {
            return(component$long_name)
          }
        }
      }
    }
  }
  
  cat(paste("No ZIP code found for address:", address, "\n"))
  return(NA)  # Return NA if no postal code component is found
}

api_key <- "AIzaSyCRgZvJtcGfxELdwDSHwflRgZVm4cYuIHQ"  # Replace with your actual Google API key

# Example usage with a dataframe named rounded_final_updated_cleaned_file
# Ensure the dataframe is loaded and contains a column 'Property.URL' with addresses

# Apply the function to update ZIP codes based on addresses in the 'Property.URL' column
rounded_final_updated_cleaned_file$ZIPCode <- sapply(rounded_final_updated_cleaned_file$Property.URL, function(address) {
  extract_zip_from_address(address, api_key)
}, USE.NAMES = FALSE)

# Check the first few rows to verify updated ZIP codes
head(rounded_final_updated_cleaned_file)


# Load your dataframe (assuming it's named rounded_final_updated_cleaned_file)
# Make sure your dataframe is already loaded or load it here



# Note: Replace "AIzaSyCRgZvJtcGfxELdwDSHwflRgZVm4cYuIHQ" with your actual API key and ensure compliance with API usage policies.








----------------
  library(httr)
library(jsonlite)
library(readr)

# Define the function to extract ZIP code from latitude and longitude using Google's Geocoding API
extract_zip_from_latlng <- function(latitude, longitude, api_key) {
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json"
  response <- GET(url = base_url, query = list(latlng = paste(latitude, longitude, sep = ","), key = api_key))
  data <- fromJSON(rawToChar(response$content), flatten = TRUE)
  
  # Check for a successful response and try to extract the ZIP code
  if(data$status == "OK") {
    address_components <- data$results[[1]]$address_components
    zip_indices <- sapply(address_components, function(x) "postal_code" %in% x$types)
    if(any(zip_indices)) {
      zip_code <- address_components[[which(zip_indices)[1]]]$long_name
      return(zip_code)
    }
  }
  return(NA)  # Return NA if unable to extract a ZIP code
}

api_key <- "AIzaSyCRgZvJtcGfxELdwDSHwflRgZVm4cYuIHQ"

# Load your dataframe

# Initialize an empty vector to store new ZIP codes
new_zip_codes <- vector("character", nrow(rounded_final_updated_cleaned_file))

# Apply the function to each row in the dataframe and update the 'ZIPCode' column
for(i in 1:nrow(rounded_final_updated_cleaned_file)) {
  new_zip_codes[i] <- extract_zip_from_latlng(
    rounded_final_updated_cleaned_file$Latitude[i],
    rounded_final_updated_cleaned_file$Longitude[i],
    api_key
  )
  # Optional: Print progress
  cat("Processed row", i, "of", nrow(rounded_final_updated_cleaned_file), "\n")
}

# Update the ZIPCode column with new ZIP codes
rounded_final_updated_cleaned_file$ZIPCode <- new_zip_codes



-------------------------------------







library(httr)
library(jsonlite)

fetch_census_data <- function(zip_code, api_key) {
  variables <- paste("B01003_001E", "B02001_002E", "B19013_001E", "B25001_001E", sep=",")
  request_url <- paste0("https://api.census.gov/data/2019/acs/acs5?get=", variables, "&for=zip%20code%20tabulation%20area:", zip_code, "&key=", api_key)
  
  response <- GET(request_url)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  
  if (length(data) > 1 && !is.null(data[2,1])) {
    total_population <- as.numeric(data[2,1])
    white_population <- as.numeric(data[2,2])
    median_income <- as.numeric(data[2,3])
    total_housing_units <- as.numeric(data[2,4])
    percent_white_population <- (white_population / total_population) * 100
  } else {
    total_population <- NA
    white_population <- NA
    percent_white_population <- NA
    median_income <- NA
    total_housing_units <- NA
  }
  
  return(list(total_population = total_population, percent_white_population = percent_white_population, median_income = median_income, total_housing_units = total_housing_units))
}

# Replace "YOUR_CENSUS_API_KEY" with your actual Census API key
census_api_key <- "3e55804f3f7cbaab28ebdbd1862dd728194dbbbe"


# Assuming the dataframe is already loaded and named 'rounded_final_updated_cleaned_file'
for(i in 1:nrow(rounded_final_updated_cleaned_file)) {
  zip_code <- as.character(rounded_final_updated_cleaned_file$ZIPCode[i])
  census_data <- fetch_census_data(zip_code, census_api_key)
  
  rounded_final_updated_cleaned_file$TotalPopulation[i] <- census_data$total_population
  rounded_final_updated_cleaned_file$PercentWhitePopulation[i] <- census_data$percent_white_population
  rounded_final_updated_cleaned_file$MedianIncome[i] <- census_data$median_income
  rounded_final_updated_cleaned_file$TotalHousingUnits[i] <- census_data$total_housing_units
}


write.csv(rounded_final_updated_cleaned_file, file = "shiny_app_data.csv", row.names = FALSE)




for(i in 1:nrow(rounded_final_updated_cleaned_file)) {
  # Extract ZIP code value, ensuring it's treated as a character string
  zip_code <- as.character(rounded_final_updated_cleaned_file$ZIPCode[i])
  
  # Basic validation: check if ZIP code is exactly 5 digits
  if(nchar(zip_code) == 5 && !any(grepl("[^0-9]", zip_code))) {
    tryCatch({
      census_data <- fetch_census_data(zip_code, census_api_key)
      
      # Assign the fetched data to the dataframe
      rounded_final_updated_cleaned_file$TotalPopulation[i] <- census_data$total_population
      rounded_final_updated_cleaned_file$PercentWhitePopulation[i] <- census_data$percent_white_population
      rounded_final_updated_cleaned_file$MedianIncome[i] <- census_data$median_income
      rounded_final_updated_cleaned_file$TotalHousingUnits[i] <- census_data$total_housing_units
    }, error = function(e) {
      message(sprintf("Error with ZIP code: %s, at row: %d", zip_code, i))
    })
  } else {
    message(sprintf("Invalid or missing ZIP code at row: %d", i))
  }
}













# Define a function to clean addresses in the Property.URL column
clean_address <- function(address) {
  # Example cleaning steps:
  # 1. Remove trailing numbers and potential codes (assumed to be at the end)
  address <- str_replace(address, "\\d{5,}$", "")
  # 2. Replace underscores, hyphens, or multiple spaces with a single space
  address <- str_replace_all(address, "[-_]+", " ")
  address <- str_replace_all(address, "\\s+", " ")
  # 3. Trim whitespace from the start and end of the address
  address <- str_trim(address)
  return(address)
}

# Apply the cleaning function to the Property.URL column
rounded_final_updated_cleaned_file$CleanedAddress <- sapply(rounded_final_updated_cleaned_file$Property.URL, clean_address)

# View the first few rows to verify the cleaned addresses
head(rounded_final_updated_cleaned_file)

# Optionally, save the dataframe with cleaned addresses to a new CSV file







extract_zip_from_address <- function(address, api_key) {
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json"
  # Ensure the address string is URL-encoded properly
  encoded_address <- URLencode(address)
  response <- GET(url = base_url, query = list(address = encoded_address, key = api_key))
  data <- fromJSON(rawToChar(response$content), flatten = TRUE)
  
  if(data$status == "OK") {
    for(result in data$results) {
      if(is.list(result) && "address_components" %in% names(result)) {
        for(component in result$address_components) {
          if("postal_code" %in% component$types) {
            return(component$long_name)
          }
        }
      }
    }
  }
  
  cat(paste("No ZIP code found for address:", address, "\n"))
  return(NA)  # Return NA if no postal code component is found
}

# Load your dataframe (assuming it's named df and has been loaded)
# df <- read_csv("/path/to/your/data.csv") # Uncomment and adjust the path

# Use your actual Google API key
api_key <- "3e55804f3f7cbaab28ebdbd1862dd728194dbbbe"

# Extract ZIP codes using cleaned addresses
rounded_final_updated_cleaned_file$ZIPCode <- sapply(rounded_final_updated_cleaned_file$CleanedAddress, function(address) {
  extract_zip_from_address(address, api_key)
}, USE.NAMES = FALSE)

# Check the first few rows to verify the new ZIP codes
head(rounded_final_updated_cleaned_file)







