# Install and load the required packages
if (!require("shiny")) install.packages("shiny")
if (!require("leaflet")) install.packages("leaflet")
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("htmltools")) install.packages("htmltools")
if (!require("maps")) install.packages("maps")
if (!require("sf")) install.packages("sf")
install.packages('rsconnect')

library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(htmltools)
library(maps)
library(sf)

# Define UI for the application
ui <- fluidPage(
  titlePanel("US Maps"),
  
  # First map: State Demographics Map
  h3("State Demographics Map"),
  leafletOutput("statesMap"),
  
  # Second map: US Listings Map
  h3("US Listings Map"),
  leafletOutput("listingsMap")
)

# Define server logic for the application
server <- function(input, output, session) {
  
  # First map: State Demographics
  # Reactive expression to read and prepare the states data
  statesData <- reactive({
    # Read the CSV file
    data <- read_csv("US_States_Governors_and_Population.csv")
    
    # Mutate to add a color column based on Governor Party
    data <- data %>%
      mutate(color = ifelse(Governor.Party == "Democratic", "blue", "red"),
             State = tolower(State)) # Ensure state names are lowercase for matching
    
    # Return the prepared data
    data
  })
  
  # Output the Leaflet map for the states
  output$statesMap <- renderLeaflet({
    # Fetch and prepare the shape data for US states
    states_shape <- st_as_sf(map("state", plot = FALSE, fill = TRUE), crs = 4326)
    states_shape$ID <- tolower(states_shape$ID) # Make IDs lowercase for matching
    
    # Get the prepared states data (reactive context)
    states_data <- statesData()
    
    # Join the shape data with the states data
    map_data <- left_join(states_shape, states_data, by = c("ID" = "State"))
    
    # Transform the projection to WGS84 (Leaflet's default)
    map_data <- st_transform(map_data, crs = 4326)
    
    # Render the Leaflet map
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = map_data, 
        fillColor = ~color, 
        fillOpacity = 0.7, 
        weight = 0.5, 
        color = "#FFFFFF", 
        popup = ~paste(ID, "<br>Population: ", Population)
      )
  })
  
  # Second map: Listings
  output$listingsMap <- renderLeaflet({
    listingsData <- read_csv("shiny_app_data.csv") %>%
      mutate(
        popupText = paste("Class:", Building.Class, "<br>",
                          "Size:", Building.Size, "<br>",
                          "Price/Room:", Price.Per.Room)
      )
    
    leaflet(data = listingsData) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Latitude, lat = ~Longitude,
        popup = ~popupText
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)