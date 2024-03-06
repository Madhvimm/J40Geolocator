# Load required libraries
library(readxl)
library(tidyr)
library(tidycensus)
library(sf)
library(tidyverse)
library(readr)
library(tigris)
library(rsconnect)
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(DT)
library(MatchIt)
library(tigris)
options(tigris_class = "sf")
library(plotly)
library(shinymaterial)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)


zcta_shape <- st_read("tl_2020_us_zcta520.shp")


# Read in the updated Maine data (assuming it's an RDS file)
usa_data <- readRDS("updated_usa_data_sf.rds")

usa_data <- usa_data %>%
  mutate(`Identified as disadvantaged` = ifelse(`Identified as disadvantaged`, "Yes", "No"))


# Load the city data file 
city_df <- read.csv("citiesdata_match.csv")


# Primary Typeface: Adobe Caslon or alternatives
primary_font <- "font-family: 'Helvetica Neue'"

# Define the Georgetown Blue color
georgetown_blue <- "#003DA5"
georgetown_blue2 <- "#041E42"


# Define the UI for the Shiny app
ui <- fluidPage(
  div(class = "navbar",
      tags$div(class = "container-fluid",
               # Navbar header
               tags$div(class = "navbar-header",
                        tags$a(class = "navbar-brand",
                               tags$h1(tags$b("Geolocation to Census Tract Tool"), 
                                       style = "font-size: 42px; color: #041E42;") 
                        )
               ),
               # Right-aligned elements
               tags$div(style = "text-align: right;", 
                        # Logo container
                        tags$div(style = "display: block; padding: 10px 0;", 
                                 tags$a(href = "https://mdi.georgetown.edu/eidc/",
                                        tags$img(src = "Environmental Impact Data Collaborative Logo Design.png", height = "100px"),
                                        target = "_blank" # Opens the link in a new tab
                                 )
                        ),
                        # Feedback button container
                        tags$div(style = "display: block;",
                                 actionButton("feedbackButton", "Give Feedback", class = "btn btn-success")
                        )
               )
      )
  ),
  # Horizontal line
  tags$hr(style = "border-top: 1px solid #ccc;"),
  
  
  
  # Adding tabsetPanel to handle multiple tabs
  tabsetPanel(
    # New Instruction tab
    tabPanel("Instructions",
             fluidRow(
               column(12,
                      tags$head(
                        tags$style(HTML("
                        .instruction-list > li {
                        border-bottom: 1px solid #ddd; 
                        padding-bottom: 12px; 
                        margin-bottom: 12px;
                        }
                        /* Apply Helvetica Neue font to the text */
                        body {
                        font-family: 'Helvetica Neue', 'serif';
                        font-size: 17px; /* Adjust the font size as needed */}"
                        )
                        )
                      ),
                      tags$h2("Welcome to the Geolocator Tool"),
                      tags$br(),  # Adds a line break
                      tags$p("The Justice 40 Initiative is a pivotal commitment to ensure federal investments benefit disadvantaged communities. The tool offers:"),
                      tags$br(),  # Adds a line break
                      tags$li(tags$b("Ease of Access")),
                      tags$br(),  # Adds a line break
                      tags$li(tags$b("Comprehensive Data Scope:"), " Spanning from broad state categories down to detailed Census Blocks, to a location"),
                      tags$br(),  # Adds a line break
                      tags$li(tags$b("Flexible Outputs: "), "For a seamless workflow, data can be effortlessly exported to Excel."),
                      tags$br(),  # Adds a line break
                      tags$p("Here's how to navigate the Tool:"),
                      tags$br(),  # Adds a line break
                      tags$ul(
                        tags$li(
                          tags$span("Geolocation to Census Tract: Enter a location to view demographic data. Location can be:"),
                          tags$br(),  # Adds a line break
                          tags$br(),  # Adds a line break
                          tags$ul(class = "instruction-list",
                                  tags$li(tags$b("State"), " - e.g., Maine, Iowa. Please enter in the Location Box and select the State from the Drop-down"),
                                  tags$li(HTML("<b>City</b> - e.g., Orlando City, Florida. Please enter the City Name (in this case Orlando) Location Box and select the State from Drop-down")),
                                  tags$li(HTML("<b>County</b> - e.g., Orange County, Florida. Please enter the County Name with <b>County</b> at the end in the Location Box and select the State from the Drop-down. Note: Parishes in Louisiana and Boroughs in Alaska are treated as Counties.")),
                                  tags$li(HTML("<b>Zip Code</b> - e.g., 04401 in Maine. Please enter the Zip Code <b>(04401)</b> in the Location Box and select the State (Maine) from the Drop-down")),
                                  tags$li(HTML("<b>Congressional District</b> - e.g., Maine 2 in Maine. Please enter the CD Name <b>(Maine 2)</b> in the Location Box and select the State (Maine) from the Drop-down")),
                                  tags$li(HTML("<b>Census Block</b> - e.g., Block 3164 in Maine. Please enter the Block Name <b>(Block 3164)</b> in the Location Box and select the State (Maine) from the Drop-down"))
                          )
                        ),
                        tags$br(),  # Adds a line break
                        tags$li(tags$b("City Comparison: "), "Find Top 10 similar cities based on demographic data and Find the Disadvantaged Status in the similar cities"),
                        tags$br(),  # Adds a line break
                        tags$li(tags$b("Batch Processing: "),"Upload data files for bulk analysis. Please note that 
                                currently, the tool only supports Bulk City Upload. We are updating the tool to work for all locations shortly."),
                        tags$br(),  # Adds a line break
                        # Add a download link for the template
                        tags$li(tags$b("Download Template for Bulk City Processing")),
                        tags$br(),  # Adds a line break
                        downloadButton("downloadTemplate", "Download Template: City Only"),
                        tags$br(),  # Adds a line break
                        #tags$li(tags$b("EV Charging Stations: "), "Explore the distribution of EV charging stations"),
                        #tags$br()  # Adds a line break
                      ),
                      tags$p(
                        tags$b(
                          tags$i(
                            "For more information, please contact at ",
                            tags$a(href = "mailto:eidc@georgetown.edu", target = "_blank", "eidc@georgetown.edu")
                          )
                        ),
                        style = "text-align: center; margin-top: 20px;"
                      )
                      ,
                      tags$br()
               )
             )
    ),
    # Second tab: Geolocation to Census Tract
    tabPanel("Geolocation to Census Tract",  
             fluidRow(
               useShinyjs(),  # Include shinyjs
               column(4, 
                      tags$div(style = paste(primary_font, "color:", georgetown_blue2, ";"), 
                               textInput("location", "Enter Location: (State, City, County, Zip Code, Congressional District, Census Block, Street Address, Location)", "")
                      ),
                      tags$div(style = paste(primary_font, "color:", georgetown_blue2, ";"), 
                               selectInput("state", "Choose a State:", choices = c("", sort(c(state.name, "District of Columbia"))), selected = NULL,multiple = FALSE)
                      ),
                      tags$div(style = paste(primary_font, "text-align: left;"), 
                               actionButton("submit", "Submit", style = paste("background-color:", georgetown_blue, "; color: white;"))
                      ),
                      span(id = "statusMessage", "Ready"),  # Set an ID for the status message
                      tags$div(style = paste(primary_font), 
                               downloadButton('downloadData', 'Download Census Data', style = paste("background-color:", georgetown_blue, "; color: white;"))
                      )
               ),
               column(8,  
                      wellPanel(
                        style = "background-color: #BBBCBC; color: #041E42; padding: 15px; border-radius: 5px;",
                        tags$div(
                          style = primary_font, 
                          shinycssloaders::withSpinner(uiOutput("messageDisplay"))
                        )
                      )
               ),
               column(12,
                      shinycssloaders::withSpinner(dataTableOutput("infoTable")),
                      shinycssloaders::withSpinner(leafletOutput("mapDisplay"))
               ),
               tags$head(tags$style(HTML(".datatable-header th {
               font-family: 'Helvetica Neue';
               font-size: 16px;
               color: georgetown_blue;
               }"))
             )
    )),
    
    # Second tab: City Comparison
    tabPanel("City Comparison",  
             fluidRow(
               column(12, 
                      tags$div(style = paste(primary_font, "color:", georgetown_blue2, ";"), 
                               selectInput("stateSelection", "Choose a State:", choices = sort(c(state.name, "District of Columbia")))
                      ),
                      tags$div(style = paste(primary_font, "color:", georgetown_blue2, ";"), 
                               selectInput("citySelection", "Choose a City:", choices = "", selectize = TRUE)
                      ),
                      tags$div(style = paste(primary_font, "text-align: left;"), 
                               actionButton("cityComparisonSubmit", "Find demographically similar cities", style = paste("background-color:", georgetown_blue, "; color: white;"))
                      ),
                      tags$div(style = primary_font, 
                               downloadButton('downloadCityComparison', 'Download Data', style = paste("background-color:", georgetown_blue, "; color: white;"))
                      )
               ),
               column(11, 
                      dataTableOutput("similarCitiesTable")
               ),
               column(12,
                      plotlyOutput("percentStackedBarChart"))
             )
    ),
    # THIRD tab: BATCH PROCESSING
    tabPanel("Bulk Processing",
             tags$br(),  # Adds a line break
             fluidRow(
               # File Upload Input
               column(12, 
                      fileInput("fileUpload", "Choose Excel File - Template can be downloaded from the Instructions Tab", accept = c(".xlsx"))
               ),
               # Action Button to Process Data
               column(12, 
                      actionButton("processData", "Process Data")
               ),
               tags$br(),  # Adds a line break
               tags$br(),  # Adds a line break
               tags$br(),  # Adds a line break
               tags$br(),  # Adds a line break
               # Placeholder for Data Preview and Results
               column(12, 
                      DTOutput("uploadedDataPreview"),
                      downloadButton("downloadBatchResults", "Download Results"),
                      shinycssloaders::withSpinner(DTOutput("processedDataPreview"))
               )
             )
    )
    
  )
)




# Create a copy for scaling and matching
scaled_city_df <- city_df

# Scale the data
scaled_city_df$Total_Population <- scale(scaled_city_df$Total_Population)
scaled_city_df$Median_Income <- scale(scaled_city_df$Median_Income)
scaled_city_df$Unemployment_Rate <- scale(scaled_city_df$Unemployment_Rate)
scaled_city_df$Median_Age <- scale(scaled_city_df$Median_Age)


find_matched_cities <- function(city, state) {
  
  # Define treatment for selected city
  city_df$treatment <- ifelse(
    city_df$City == city & 
      city_df$State == state, 
    1, 
    0)
  
  # Use MatchIt with Mahalanobis distance
  m.out <- matchit(treatment ~ Total_Population + Median_Income + Unemployment_Rate + Median_Age, 
                   data = city_df, 
                   ratio = 15, 
                   method = "nearest", 
                   distance = "mahalanobis")
  
  matched_data <- match.data(m.out)
  
  # Get top 10 matched cities
  top_similar_cities <- subset(matched_data, treatment == 0)[1:10,]
  
  # Add the selected city to the top of the data frame
  selected_city_data <- subset(matched_data, treatment == 1)
  combined_data <- rbind(selected_city_data, top_similar_cities)
  
  names(combined_data) <- gsub("\\.", " ", names(combined_data))
  names(combined_data) <- gsub("\\.{2,}", "", names(combined_data))
  names(combined_data) <- trimws(names(combined_data))
  
  print(colnames(combined_data))
  
  # Select the desired columns
  combined_data <- combined_data %>%
    dplyr::select(City, State, Total_Population, Median_Income, Unemployment_Rate, Median_Age,
                  `Total_no_of_census_tracts`, `Total_no_of_disadvantaged_tracts`, 
                  `Percentage_of_disadvantaged_population`)
  
  
  return(combined_data)
}




#creating a function to detect if the input is a county:
# Updated function to detect if the input is a county, parish, or borough
is_county_input <- function(input_string) {
  input_string <- tolower(input_string)
  return(grepl("county", input_string) || 
           grepl("parish", input_string) || 
           grepl("borough", input_string) ||
           input_string == "district of columbia")
}




# Function to check if input is a ZIP code
is_zip_code_input <- function(location) {
  return(nchar(location) == 5 && grepl("^\\d{5}$", location))
}

# Function to Check for Congressional District Input:
is_congressional_district_input <- function(input_location) {
  # Trim and convert to lowercase for case-insensitive comparison
  input_location <- tolower(trimws(input_location))
  
  # Pattern assumes the input might be of the form 'Maine 1' or 'ME 01'
  pattern <- "^[a-z]{2,}\\s\\d{1,2}$"
  return(grepl(pattern, input_location))
}


# Function to detect block inputs:
is_block_input <- function(input_location) {
  # Trim and convert to lowercase for case-insensitive comparison
  input_location <- tolower(trimws(input_location))
  
  # Using NAME20 as the identifier
  pattern <- "^block \\d+$"
  return(grepl(pattern, input_location))
}



get_input_type <- function(trimmed_location, state) {
  input_types <- list()
  
  print(paste("Trimmed Location:", trimmed_location))
  print(paste("State:", state))
  
  if (!is.null(trimmed_location) && trimmed_location != "") {
    lower_trimmed_location <- tolower(trimmed_location)
    
    # Check for City input if a state is provided
    if (!is.null(state) && state != "") {
      city_match <- places(state = state, year = 2020) %>%
        dplyr::filter(tolower(NAME) == lower_trimmed_location)
      if (nrow(city_match) > 0) {
        input_types <- c(input_types, "city")
      }
    }
    
    # Check for State input - only if location matches the state
    if (length(input_types) == 0 && !is.null(state) && tolower(state) == lower_trimmed_location) {
      print("Checking for state match")
      state_match <- usa_data %>%
        dplyr::filter(tolower(`State/Territory`) == lower_trimmed_location)
      if (nrow(state_match) > 0) {
        input_types <- c(input_types, "state")
        print("State match found")
      }
    }
    
    # Checks for County, ZIP code, Congressional District, and Block
    if (is_county_input(trimmed_location)) {
      input_types <- c(input_types, "county")
    }
    if (is_zip_code_input(trimmed_location)) {
      input_types <- c(input_types, "zip")
    }
    if (is_congressional_district_input(trimmed_location)) {
      input_types <- c(input_types, "congressional_district")
    }
    if (is_block_input(trimmed_location)) {
      input_types <- c(input_types, "block")
    }
  }
  
  return(input_types)
}



get_input_batch <- function(location, state) {
  # Initialize the return value
  input_type <- "City not found, try to find individually in the Geolocation to Census Tract Tab"
  
  # Ensure that location is a single value
  if (length(location) != 1) {
    stop("location must be a single value")
  }
  
  # Ensure that location is not NULL or an empty string
  if (!is.null(location) && nzchar(location)) {
    # Remove common suffixes
    location <- gsub(" city| town| village$", "", location, ignore.case = TRUE)
    
    # Checking for State input
    state_match <- usa_data %>%
      dplyr::filter(tolower(`State/Territory`) == tolower(location))
    if (nrow(state_match) > 0) {
      input_type <- "state"
    } 
    
    # Checking for City input if not matched with state
    if (input_type == "City not found, try to find individually in the Geolocation to Census Tract Tab") {
      city_match <- places(state = state, year = 2020) %>%
        dplyr::filter(NAME == location)
      if (nrow(city_match) > 0) {
        input_type <- "city"
      }
    }
  }
  
  return(input_type)
}




# Define the server logic for the Shiny app
server <- function(input, output,session) {
  
  # Reactive value to store the result data
  result_data <- reactiveVal(NULL)
  
  # Reactive expression for trimmed location
  trimmed_location <- reactive({
    trimws(input$location)
  })
  
  # Define an observer for the "Submit" button
  observeEvent(input$submit, {
      
    print("Submit button pressed")
    
    shinyjs::html("statusMessage", "Processing...")  # Update the message immediately
    
    
    
    # Check if the trimmed location field is empty
    if (is.null(trimmed_location()) || trimmed_location() == "") {
      showModal(modalDialog(
        title = "Location Required",
        "Please enter a location...",
        easyClose = TRUE
      ))
      return()
    }
    
    # Determine the type of input using the trimmed location
    input_types <- get_input_type(trimmed_location(), input$state)
    
    # Print the detected input types
    print(paste("Detected input types:", paste(input_types, collapse = ", ")))
    
    
    # Check if a state is needed but not provided
    if (any(input_types %in% c("city", "county", "zip", "congressional_district", "block")) && length(input$state) == 0) {
      showModal(modalDialog(
        title = "State Selection Required",
        "Please select at least one state for this type of search.",
        easyClose = TRUE
      ))
      return()
    }
    
    relevant_data <- NULL
    
    # Clear the result data for each new search
    result_data(NULL)
    
    # Initialize a flag to check if relevant data is found
    has_relevant_data <- FALSE
    
    # Transform states to uppercase
    input_states_upper <- toupper(input$state)
    
    # Iterate over each selected state
    for (state in input_states_upper) {
      # Process data based on each input type
      for (input_type in input_types) {
        # Data processing based on input type
        switch(input_type,
               "state" = {
                 cat("Detected a state input\n")
                 shinyjs::html("statusMessage", "Processing state data...")  # Update the message immediately
                 # Trim and convert to lowercase before filtering
                 lower_trimmed_location <- tolower(trimws(input$location))
                 filtered_data <- usa_data %>% dplyr::filter(tolower(`State/Territory`) == lower_trimmed_location)
                 result_data(filtered_data)
                 has_relevant_data <- TRUE
                 shinyjs::html("statusMessage", "State data processed.")  # Update the message after processing
                 return()
                 },
               "city" = {
                 cat("Checking for City...\n")
                 
                 shinyjs::html("statusMessage", "Processing City data...")  # Update the message immediately
                 
                 # Initialize the city_data_processed variable
                 city_data_processed <- FALSE
                 
                 # Convert the input location to lowercase for a case-insensitive comparison
                 lower_input_location <- tolower(trimws(input$location))
                 
                 # Function to process city data for a given year
                 process_city_data <- function(city_year, tracts_year) {
                   city_boundary <- places(state = input$state, year = city_year) %>%
                     dplyr::filter(tolower(NAME) == lower_input_location) %>%
                     st_transform(crs = 4269) # Using NAD83
                   
                   state_tracts <- tracts(state = input$state, year = tracts_year) %>%
                     st_transform(crs = 4269) # Using NAD83
                   
                   city_tracts <- st_intersection(state_tracts, city_boundary)
                   
                   # Extract the GEOID values from the city_tracts
                   selected_tracts <- city_tracts$GEOID
                   print(head(selected_tracts))
                   
                   # Filter usa_data based on these GEOIDs
                   selected_city_data <- usa_data %>%
                     dplyr::filter(`Census tract 2010 ID` %in% selected_tracts)
                   
                   return(selected_city_data)
                 }
                 
                 # Check for all combinations: 2020-2020, 2019-2019, 2020-2019, 2019-2020
                 city_found_2020 <- nrow(places(state = input$state, year = 2020) %>%
                                           dplyr::filter(tolower(NAME) == lower_input_location)) > 0
                 city_found_2019 <- nrow(places(state = input$state, year = 2019) %>%
                                           dplyr::filter(tolower(NAME) == lower_input_location)) > 0
                 
                 if (city_found_2020) {
                   selected_city_data_2020 <- process_city_data(2020, 2020)
                   if (nrow(selected_city_data_2020) > 0) {
                     result_data(selected_city_data_2020)
                     has_relevant_data <- TRUE
                     city_data_processed <- TRUE  # Indicate that city data has been processed
                     
                   } else {
                     # Try with 2019 tracts
                     selected_city_data_2019_tracts <- process_city_data(2020, 2019)
                     if (nrow(selected_city_data_2019_tracts) > 0) {
                       result_data(selected_city_data_2019_tracts)
                       has_relevant_data <- TRUE
                       city_data_processed <- TRUE  # Indicate that city data has been processed
                       
                     }
                   }
                 }
                 
                 if (!has_relevant_data && city_found_2019) {
                   selected_city_data_2019 <- process_city_data(2019, 2019)
                   if (nrow(selected_city_data_2019) > 0) {
                     result_data(selected_city_data_2019)
                     has_relevant_data <- TRUE
                     city_data_processed <- TRUE  # Indicate that city data has been processed
                     
                   } else {
                     selected_city_data_2020_tracts <- process_city_data(2019, 2020)
                     if (nrow(selected_city_data_2020_tracts) > 0) {
                       result_data(selected_city_data_2020_tracts)
                       has_relevant_data <- TRUE
                       city_data_processed <- TRUE  # Indicate that city data has been processed
                       
                     }
                   }
                 }
                 
                 if (!has_relevant_data) {
                   print("No relevant data found for the city in any combination.")
                 }
                 
                 # After all city data checks are done
                 if (city_data_processed) {
                   shinyjs::html("statusMessage", "City data processed.")
                 } else {
                   shinyjs::html("statusMessage", "No relevant city data found.")
                 }
               },
               
               "county" = {
                 cat("Checking for County...\n")
                 
                 shinyjs::html("statusMessage", "Processing County data...")  # Update the message immediately
                 
                 # Prepare the county name
                 county_name <- tolower(trimws(input$location))
                 
                 # Handle 'county', 'parish', and 'borough' in the name
                 if (grepl("\\bcounty\\b", county_name)) {
                   county_name <- gsub("\\bcounty\\b", "", county_name, ignore.case = TRUE)
                 } else if (grepl("\\bparish\\b", county_name)) {
                   county_name <- gsub("\\bparish\\b", "", county_name, ignore.case = TRUE)
                 } else if (grepl("\\bborough\\b", county_name)) {
                   county_name <- gsub("\\bborough\\b", "", county_name, ignore.case = TRUE)
                 }
                 county_name <- trimws(county_name)
                 
                 # Construct full county, parish, or borough name
                 full_county_name <- ifelse(county_name == "district of columbia", 
                                            "district of columbia", 
                                            ifelse(grepl("\\bparish\\b", tolower(input$location)), 
                                                   paste0(county_name, " parish"),
                                                   ifelse(grepl("\\bborough\\b", tolower(input$location)),
                                                          paste0(county_name, " borough"),
                                                          paste0(county_name, " county"))))
                 
                 relevant_data_county <- NULL  # Initialize an empty variable to store county data
                 
                 print("Print FOR DEBUGGING FULL COUNTY NAME")
                 print(full_county_name)
                 
                 if (length(input$state) > 0) {
                   for (state in input$state) {
                     # Process each selected state
                     county_data <- usa_data %>%
                       dplyr::filter(tolower(`County Name`) == tolower(full_county_name) & tolower(`State/Territory`) == tolower(state))
                     
                     if (!is.null(county_data) && nrow(county_data) > 0) {
                       if (is.null(relevant_data_county)) {
                         relevant_data_county <- county_data  # First assignment
                       } else {
                         relevant_data_county <- rbind(relevant_data_county, county_data)  # Append data for each state
                       }
                     }
                   }
                 } else {
                   # Search for the county across all states if no state is selected
                   relevant_data_county <- usa_data %>%
                     dplyr::filter(tolower(`County Name`) == tolower(full_county_name))
                 }
                 
                 if (!is.null(relevant_data_county) && nrow(relevant_data_county) > 0) {
                   result_data(relevant_data_county)  # Update result data
                   has_relevant_data <- TRUE
                   shinyjs::html("statusMessage", "County data processed.")  # Update the message after processing
                   
                 }
               },
               "zip" = {
                 cat("Checking for ZIP code...\n")
                 
                 shinyjs::html("statusMessage", "Processing ZIP data...")  # Update the message immediately
                 
                 
                 # Trim extra whitespace from the ZIP code input
                 trimmed_zip_input <- trimws(input$location)
                 
                 # Get the ZIP code boundary
                 # Filter the shapefile based on the user's input
                 zip_boundary <- zcta_shape %>%
                   dplyr::filter(ZCTA5CE20 == trimmed_zip_input) %>%
                   st_transform(crs = 4269)# Using NAD83, assuming your other data is in this CRS too
                 
                 shinyjs::html("statusMessage", "Getting ZIP boundaries from the census...")  # Update the message immediately
                 
                 # Get all tracts for the state
                 state_tracts <- tracts(state = input$state, year = 2020) %>%
                   st_transform(crs = 4269) # Using NAD80
                 
                 # Find the tracts within the ZIP code boundary
                 zip_tracts <- st_intersection(state_tracts, zip_boundary)
                 
                 shinyjs::html("statusMessage", "Find the tracts within the ZIP code boundary...")  # Update the message immediately
                 
                 # Extract the GEOID values from the zip_tracts
                 selected_tracts <- zip_tracts$GEOID
                 print(head(selected_tracts))
                 
                 # Filter usa_data based on these GEOIDs
                 selected_zip_data <- usa_data %>%
                   dplyr::filter(`Census tract 2010 ID` %in% selected_tracts)
                 print(selected_zip_data)
                 
                 # Check if there's relevant data
                 if (nrow(selected_zip_data) > 0) {
                   result_data(selected_zip_data) # Assuming 'result_data' is a function that handles the filtered data
                   has_relevant_data <- TRUE
                   shinyjs::html("statusMessage", "ZIP data processed.")  # Update the message after processing
                   return()
                   }
                 },
               "congressional_district" = {
                 cat("Checking for Congressional District...\n")
                 shinyjs::html("statusMessage", "Processing Congressional District...")  # Update the message immediately
                 # Prepare the input by trimming and converting to lowercase
                 prepared_input <- tolower(trimws(input$location))
                 
                 # Split input to extract state and district number
                 parts <- unlist(strsplit(prepared_input, " "))
                 input_state <- parts[1]
                 input_district <- sprintf("%02d", as.numeric(parts[2])) # Format to two digits
                 
                 # Get the Congressional District boundary
                 cd_boundary <- congressional_districts(state = input_state, year = 2020) %>%
                   dplyr::filter(CD116FP == input_district) %>%
                   st_transform(crs = 4269) # Using NAD83
                 
                 # Get all tracts for the state
                 state_tracts <- tracts(state = input_state, year = 2020) %>%
                   st_transform(crs = 4269) # Using NAD83
                 
                 # Find the tracts within the Congressional District boundary
                 cd_tracts <- st_intersection(state_tracts, cd_boundary)
                 # Extract the GEOID values from the cd_tracts
                 selected_tracts <- cd_tracts$GEOID
                 print(head(selected_tracts))
                 
                 # Filter usa_data based on these GEOIDs
                 selected_cd_data <- usa_data %>%
                   dplyr::filter(`Census tract 2010 ID` %in% selected_tracts)
                 print(selected_cd_data)
                 
                 # Check if there's relevant data
                 if (nrow(selected_cd_data) > 0) {
                   result_data(selected_cd_data)
                   has_relevant_data <- TRUE
                   shinyjs::html("statusMessage", "CD data processed.")  # Update the message after processing
                   return()
                 }
               },
               "block" = {
                 cat("Checking for Block...\n")
                 shinyjs::html("statusMessage", "Processing Census Block...")  # Update the message immediately
                 # Normalize the input block name
                 prepared_input_block_name <- tolower(trimws(input$location))
                 prepared_input_block_name <- gsub("^block\\s*", "", prepared_input_block_name, ignore.case = TRUE) # Remove 'block' prefix
                 prepared_input_block_name <- paste("Block", prepared_input_block_name) # Add 'Block' with proper case
                 
                 # Get the Block boundary
                 block_boundary <- blocks(state = input$state, year = 2020) %>%
                   dplyr::filter(NAME20 == prepared_input_block_name) %>%
                   st_transform(crs = 4269) # Using NAD83
                 
                 # Check if block_boundary has at least one GEOID20
                 if (nrow(block_boundary) < 1) {
                   cat("No matching blocks found for the input. Proceeding to next steps...\n")
                   # Set has_relevant_data to FALSE but do not return; continue to next steps
                   has_relevant_data <- FALSE
                 } else {
                   # Process the found block data
                   tract_ids <- substr(block_boundary$GEOID20, 1, nchar(block_boundary$GEOID20) - 4)
                   selected_block_data <- usa_data %>%
                     dplyr::filter(`Census tract 2010 ID` %in% tract_ids)
                   
                   if (nrow(selected_block_data) > 0) {
                     result_data(selected_block_data)
                     has_relevant_data <- TRUE
                     shinyjs::html("statusMessage", "Census Block data processed.")  # Update the message after processing
                   } else {
                     has_relevant_data <- FALSE
                   }
                 }
                 # Do not use return() here, allow the execution to continue
               },
               "other" = {
                 # Do nothing, proceed to Photon API check
                 }
        )
      }
    }
    
    # Photon API Check (if no state/county/city/zip/block/CD is found)
    if (!has_relevant_data) {
      cat("Fetching data using Photon API...\n")
      
      shinyjs::html("statusMessage", "Processing Location through Photon API (Might take few seconds to check for API call...")  # Update the message immediately
      
      
      # Construct the query with the state name
      query <- if(length(input$state) > 0) {
        paste(input$location, input$state, sep = ", ")
      } else {
        input$location
      }
      
      base_url <- "http://photon.komoot.io/api/?q="
      
      # Try API call
      res <- tryCatch({
        GET(paste0(base_url, URLencode(query), "&limit=1&location_bias_scale=2&lat=39.8283&lon=-98.5795"))
      }, error = function(e) {
        message("Error in API call: ", e$message)
        return(NULL)
      })
      
      if (!is.null(res) && res$status_code == 200) {
        
        cat("API Response Status: ", res$status_code, "\n")
        data <- content(res, "text")
        data <- fromJSON(data, flatten = TRUE)
        print(data)  # Debugging line to check the API response format
        
        if ("features" %in% names(data) && length(data$features) > 0 && data$features$properties.country[1] == "United States") {
          lat <- data$features$geometry.coordinates[[1]][2]
          lon <- data$features$geometry.coordinates[[1]][1]
          api_state <- data$features$properties.state[1] # Extracting the state from the API response
          
          # Check if the API state matches the input state
          if (!is.null(api_state) && tolower(api_state) == tolower(input$state)) {
            cat("Coordinates: ", lat, ", ", lon, "\n")
            point <- st_point(c(lon, lat))
            point_sf <- st_sfc(point, crs = 4326)
            point_sf <- st_transform(point_sf, crs = st_crs(usa_data))  # Ensure the point is in the same CRS as usa_data
            
            within_tract <- st_within(point_sf, usa_data)
            
            if (length(unlist(within_tract)) == 0) {
              nearest_tract_index <- st_nearest_feature(point_sf, usa_data)
              relevant_data <- usa_data[nearest_tract_index, ]
            } else {
              tract_indices <- unlist(within_tract)
              relevant_data <- usa_data[tract_indices, ]
            }
            
            if (!is.null(relevant_data) && nrow(relevant_data) > 0) {
              result_data(relevant_data)
              has_relevant_data <- TRUE
              shinyjs::html("statusMessage", "Photon API processing completed. Data found.")
            }
          } else {
            cat("No relevant data found in API response\n")
            shinyjs::html("statusMessage", "Photon API processing completed. No relevant data found.")
          }
        } else {
          cat("API call returned NULL or unsuccessful status code\n")
          shinyjs::html("statusMessage", "Photon API processing failed.")
        }
      }
    }
    
    # Error Modal if no relevant data found
    if (!has_relevant_data) {
      showModal(modalDialog(
        title = "Error",
        "Could not find the specified location in the USA. Please try again with a more specific location or check the spelling.",
        easyClose = TRUE
      ))
      }
    })
  
  
  # This is where you should add the new renderUI block
  output$messageDisplay <- renderUI({
    if (is.null(result_data())) {
      return(NULL)
    } else {
      num_tracts <- nrow(result_data())
      
      disadvantaged_count <- sum(result_data()$`Identified as disadvantaged` == "Yes")  # Using backticks because of spaces in column name
      advantaged_count <- num_tracts - disadvantaged_count  # Subtracting disadvantaged tracts from total gives the number of advantaged tracts
      
      # Calculate disadvantaged population
      disadvantaged_population <- sum(result_data()$`Total population` * (result_data()$`Identified as disadvantaged` == "Yes"))
      total_population <- sum(result_data()$`Total population`)
      disadv_population_percentage <- (disadvantaged_population / total_population) * 100
      
      adv_percentage <- (advantaged_count / num_tracts) * 100
      disadv_percentage <- (disadvantaged_count / num_tracts) * 100
      
      print(result_data)
      
      return(tags$div(
        style = "background-color: #D6D2C4; padding: 15px; border-radius: 5px;",
        h4(style = "color: #041E42;", paste("Total:", num_tracts, "Census Tracts Detected")),
        h5(style = "color: #041E42;", paste("Advantaged Tracts: ", round(adv_percentage, 2), "% of Total")),
        h5(style = "color: #041E42;", paste("Disadvantaged Tracts: ", round(disadv_percentage, 2), "% of Total")),
        h5(style = "color: #041E42;", paste(round(disadv_population_percentage, 2), "% of the Overall Population Resides in Disadvantaged Tracts"))
      ))
    }
  })
  
  
  
  # Render a table for the UI
  # Render a table for the UI using DT for pagination
  
  
  tags$style(HTML("
  .census-tract-column { width: 200px !important; }
  .county-name-column { width: 100px !important; }
  .state-territory-column { width: 100px !important; }
  .identified-disadvantaged-column { width: 100px !important; }
  .total-population-column { width: 100px !important; }
                  "))
  
  
  output$infoTable <- renderDT({
    req(result_data())  # This ensures that the rest of the code doesn't run until result_data is available
    
    result <- result_data()
    
    # If result is of class 'sf', remove the geometry
    if (inherits(result, "sf")) {
      result <- st_set_geometry(result, NULL)
    }
    
    data <- as.data.frame(result)
    
    # Define the required columns for display
    required_columns <- c("Census tract 2010 ID", "County Name", "State/Territory", 
                          "Identified as disadvantaged", "Total population")
    
    # Check if all required columns are present, else display available data
    intersecting_columns <- intersect(required_columns, names(data))
    data_to_display <- data[, intersecting_columns, drop = FALSE]
    
    # Check for missing columns
    missing_columns <- setdiff(required_columns, intersecting_columns)
    if (length(missing_columns) > 0) {
      message("Some required columns are missing. Displaying available columns.")
    }
    
    datatable(data_to_display, options = list(pageLength = 10, autoWidth = TRUE),
              class = 'cell-border stripe datatable-header', rownames = FALSE) %>%
      formatStyle(columns = "Census tract 2010 ID", className = 'census-tract-column') %>%
      formatStyle(columns = "County Name", className = 'county-name-column') %>%
      formatStyle(columns = "State/Territory", className = 'state-territory-column') %>%
      formatStyle(columns = "Identified as disadvantaged", className = 'identified-disadvantaged-column') %>%
      formatStyle(columns = "Total population", className = 'total-population-column') %>%
      formatStyle(columns = names(data_to_display), 
                  `font-size` = '14px', `font-family` = 'Adobe Caslon') %>%
      formatStyle(columns = names(data_to_display), 
                  fontWeight = 'bold', `font-size` = '16px', 
                  color = georgetown_blue2, backgroundColor = 'white', 
                  target = 'row')
  })
  
  
  output$mapDisplay <- renderLeaflet({
    if (is.null(result_data())) {
      return(NULL)
    }
    
    dc_data_all <- result_data()[result_data()$`State/Territory` == "District of Columbia", ]
    #print(unique(st_geometry_type(dc_data_all)))
    
    # Filter out non-polygon/multipolygon geometries
    valid_geoms <- result_data()[sf::st_is(result_data(), "POLYGON") | sf::st_is(result_data(), "MULTIPOLYGON"), ]
    
    print(nrow(valid_geoms))
    # Debugging: Print the first few rows of valid_geoms
    #print("Printing valid geoms for Debugging")
    #print(head(valid_geoms))
    
    dc_data <- valid_geoms[valid_geoms$`State/Territory` == "District of Columbia", ]
    print(nrow(dc_data))
    
    
    # Check if 'Identified as disadvantaged' column exists in the data
    has_disadv_col <- "Identified as disadvantaged" %in% names(valid_geoms)
    
    # Create a new column for fillColor based on the condition
    valid_geoms$fillColor <- ifelse(valid_geoms$`Identified as disadvantaged` == "Yes", "red", "green")
    
    
    leaflet(data = valid_geoms) %>%
      addTiles() %>%
      addMouseCoordinates() %>%
      addFullscreenControl() %>%
      addPolygons(
        fillColor = ~fillColor,  # Use the new column for coloring
        stroke = TRUE, 
        weight = 1, 
        opacity = 1, 
        color = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
        popup = ~paste0("Census Tract id: ", `Census tract 2010 ID`, "<br>",
                        "County Name: ", `County Name`, "<br>",
                        "State/Territory: ", `State/Territory`, "<br>",
                        "Total Population: ", `Total population`, "<br>",
                        "Disadvantaged: ", 
                        if (has_disadv_col) {
                          ifelse(is.na(`Identified as disadvantaged`), "Unknown", 
                                 ifelse(`Identified as disadvantaged` == "Yes", "Yes", "No"))
                        } else {
                          "Unknown"
                        })
      )})
  
  
  observe({
    req(result_data())
    dc_data <- result_data()[result_data()$`State/Territory` == "District of Columbia", ]
    print(paste("Number of rows in DC data:", nrow(dc_data)))
    print(head(dc_data))
    if (nrow(dc_data) > 0) {
      print(sf::st_geometry_type(dc_data))
      print(sf::st_crs(dc_data))
    }
  })
  
  
  
  
  

  # Define a reactive value for result messages (if needed)
  result_message <- reactiveVal("")
  
  
  # Define a download handler for downloading data
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$location, "_census_data.csv", sep = "") },
    content = function(file) {
      # Use result data for download
      data_to_save <- result_data()
      
      # If data_to_save is of class 'sf', remove the geometry
      if (inherits(data_to_save, "sf")) {
        data_to_save <- st_set_geometry(data_to_save, NULL)
      }
      
      # Remove columns of type 'list'
      data_to_save <- data_to_save[sapply(data_to_save, class) != "list"]
      
      # Write the cleaned data to a CSV file
      if (!is.null(data_to_save) && nrow(data_to_save) > 0) {
        write.csv(data_to_save, file, row.names = FALSE)
      }
    }
  )
  
  
  ##########CITY COMPARISION TAB - START
  
  # Within the server function
  # Observe the state selection and populate the city dropdown based on the chosen state
  observe({
    # Filter city_only_df for the selected state
    filtered_data <- city_df[city_df$State == input$stateSelection,]
    
    # Get the unique cities from the filtered data
    city_choices <- unique(filtered_data$City)
    
    # Update the citySelection dropdown with the cities corresponding to the chosen state
    updateSelectInput(session, "citySelection", choices = city_choices)
  })
  
  # Within the server function
  city_comparison_data <- reactiveVal()
  
  observeEvent(input$cityComparisonSubmit, {
    city <- input$citySelection
    state <- input$stateSelection
    matched_cities <- find_matched_cities(city, state)
    city_comparison_data(matched_cities)
  })
  
  # Render the table for similar cities comparison
  output$similarCitiesTable <- renderDT({
    data <- city_comparison_data()
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }
    rownames(data) <- NULL
    datatable(data, options = list(pageLength = 10, autoWidth = TRUE), 
              class = 'cell-border stripe',
              rownames = FALSE) %>%
      formatStyle(columns = names(data), 
                  `font-size` = '14px',
                  `font-family` = 'Adobe Caslon') %>%
      formatStyle(columns = names(data), 
                  fontWeight = 'bold', 
                  `font-size` = '16px', 
                  target = 'row')
  })
  
  #####CHECK
  
  output$percentStackedBarChart <- renderPlotly({
    data <- city_comparison_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    # Calculating the percentage of non-disadvantaged tracts
    data$`Percentage of non-disadvantaged population` <- 100 - data$`Percentage of disadvantaged population`
    
    # Sorting the data based on the percentage of disadvantaged population for clearer comparison
    data$City <- factor(data$City, levels = data$City[order(data$`Percentage of disadvantaged population`, decreasing = FALSE)])
    
    
    # Adding custom hover text
    hover_text <- paste(
      "State Name: ", data$State, "<br>",
      "City Name: ", data$City, "<br>",
      "Total Population: ", data$Total_Population, "<br>",
      "Median Income: ", "$", data$Median_Income, "<br>",
      "Unemployment Rate: ", data$Unemployment_Rate, "%", "<br>",
      "Median Age: ", data$Median_Age
    )
    
    
    # Creating the 100% stacked bar chart
    
    p <- plot_ly(
      data = data,
      x = ~City,
      y = ~`Percentage of disadvantaged population`,
      type = 'bar',
      marker = list(color = '#D50032'),
      hoverinfo = 'text', 
      hovertext = hover_text,
      name = 'Disadvantaged Population (%)'  # Adding the name attribute here
    ) %>%
      add_trace(
        y = ~`Percentage of non-disadvantaged population`,
        name = 'Non-Disadvantaged Population (%)',
        marker = list(color = '#00B5E2'),
        hoverinfo = 'text', 
        hovertext = hover_text
      ) %>%
      layout(
        title = "Proportional Comparison of Similar Cities based on Census Tracts",
        font = list(family = "Adobe Caslon", size = 12, color = georgetown_blue2),
        xaxis = list(title = "Demographically Similar City"),
        yaxis = list(title = "Percentage (%)", tickvals = c(0, 25, 50, 75, 100), ticktext = c("0%", "25%", "50%", "75%", "100%")),
        barmode = 'stack',
        hovermode = "closest",
        hoverlabel = list(bgcolor = "white")
      )
    return(p)
    
  })
  
  
  output$downloadCityComparison <- downloadHandler(
    filename = function() { 
      paste("city_comparison-", Sys.Date(), ".csv", sep = "") 
    },
    content = function(file) {
      # Retrieve the city comparison data
      data_to_save <- city_comparison_data()
      
      # If data_to_save is of class 'sf', remove the geometry
      if (inherits(data_to_save, "sf")) {
        data_to_save <- st_set_geometry(data_to_save, NULL)
      }
      
      # Remove columns of type 'list'
      data_to_save <- data_to_save[sapply(data_to_save, class) != "list"]
      
      # Write the cleaned data to a CSV file
      if (!is.null(data_to_save) && nrow(data_to_save) > 0) {
        write.csv(data_to_save, file, row.names = FALSE)
      }
    }
  )
  
  #####CHECK END
  
  ##########CITY COMPARISION TAB - END
  

  ###########################################################FOR 3 rd TAB
  # For storing and previewing the uploaded data
  uploaded_data <- reactiveVal()
  processed_data <- reactiveVal(data.frame())  # To store the processed data
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    
    tryCatch({
      data <- readxl::read_excel(input$fileUpload$datapath)
      print("Uploaded data:")
      print(head(data))
      uploaded_data(data)
    }, error = function(e) {
      showNotification("Error in file upload. Please check the file format.", type = "error")
      print("File upload error:")
      print(e)
    })
  }) # End of observeEvent for fileUpload
  
  output$uploadedDataPreview <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data(), options = list(pageLength = 5))
  })  # End of renderDT for uploadedDataPreview
  
  
  observeEvent(input$processData, {
    req(uploaded_data())
    
    data <- uploaded_data()
    
    print("PRINT HEAD OF DATA")
    print(head(data)) #FOR DEBUGGING
    
    processed_data_list <- list()
    
    tryCatch({
      for (i in 1:nrow(data)) {
        row_data <- data[i, ]
        input_type <- get_input_batch(row_data$Location, row_data$State)
        
        # Initialize default values
        total_tracts <- NA
        disadvantaged_tracts <- NA
        percentage_disadvantaged <- NA
        
        if (input_type == "city") {
          cat("Checking for City...\n")
          print("PRINTING Location")
          print(row_data$Location)
          
          city_boundary <- places(state = row_data$State, year = 2020) %>%
            dplyr::filter(NAME == row_data$Location) %>%
            st_transform(crs = 4269)
          
          state_tracts <- tracts(state = row_data$State, year = 2020) %>%
            st_transform(crs = 4269)
          
          if (nrow(city_boundary) > 0 && nrow(state_tracts) > 0) {
            city_tracts <- st_intersection(state_tracts, city_boundary)
            
            if (!is.null(city_tracts) && nrow(city_tracts) > 0) {
              selected_city_data <- usa_data %>%
                dplyr::filter(`Census tract 2010 ID` %in% city_tracts$GEOID)
              
              if (nrow(selected_city_data) > 0) {
                total_tracts <- nrow(selected_city_data)
                disadvantaged_tracts <- sum(selected_city_data$`Identified as disadvantaged` == "Yes", na.rm = TRUE)
                total_population <- sum(selected_city_data$`Total population`, na.rm = TRUE)
                disadvantaged_population <- sum(selected_city_data$`Total population`[selected_city_data$`Identified as disadvantaged` == "Yes"], na.rm = TRUE)
                percentage_disadvantaged <- if (total_population > 0) round((disadvantaged_population / total_population) * 100, 2) else NA_real_
              }
            }
          }
        }
        # Append the processed data with input type to the list
        processed_data_list[[i]] <- c(row_data, input_type, total_tracts, disadvantaged_tracts, percentage_disadvantaged)
      }
      
      # Convert the list to a dataframe
      processed_data_df <- do.call(rbind, processed_data_list)
      colnames(processed_data_df) <- c(colnames(data), "input_type", "Total_no_of_census_tracts", "Total_no_of_disadvantaged_tracts", "Percentage_of_disadvantaged_population")
      processed_data(processed_data_df)
      
      print("PRINTING PROCESSED DATA")
      print(processed_data_df)
    }, error = function(e) {
      print(e)
      traceback()
      showNotification(paste("Error processing data:", e$message), type = "error")
    }, finally = {
      cat("Operation complete.\n")
    })
  })
  
  output$processedDataPreview <- renderDT({
    req(processed_data())
    datatable(processed_data())
  })
  
  output$downloadBatchResults <- downloadHandler(
    filename = function() { "batch_results.csv" },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }  # End of content function
  )  # End of downloadHandler for downloadBatchResults
  
  
  ###########################################################FOR 3 rd TAB CLOSE
  
  
  
  # Observe event for feedback button to create the modal
  observeEvent(input$feedbackButton, {
    showModal(modalDialog(
      title = "Have Feedback?",
      textInput("name", "Enter Your Name"),
      textInput("email", "Enter Your Email"),
      textAreaInput("feedbackText", "Enter your feedback here.", "", rows = 3),
      tags$p("Please rate your experience (1-5):"),
      numericInput("starRating", label = NULL, value = 1, min = 1, max = 5),
      footer = tagList(
        modalButton("Close"),
        actionButton("submitFeedback", "Submit", class = "btn btn-success")
      ),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  
  # Logic to handle feedback submission
  observeEvent(input$submitFeedback, {
    # Retrieve feedback text and star rating
    feedback <- input$feedbackText
    rating <- input$starRating
    name <- input$name
    email <- input$email
    # Here, you would handle the feedback (e.g., save to a database, send an email, etc.)
    # For demonstration, we'll just print to the console
    print(paste("Name:", name, "Email:", email, "Feedback:", feedback, "Rating:", rating))
    
    # You can also display a notification to the user
    showNotification("Thank you for your feedback!", type = "message")
    
    # Close the modal after submission
    removeModal()
  })
  
  
  #TAB FOR DOWNLOAD TEMPLATE
  
  # Download Handler for the template
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      "Template_BatchProcessing.xlsx" # or the appropriate file extension
    },
    content = function(file) {
      file.copy("Template_BatchProcessing.xlsx", file)
    }
  )
  
  
  
}



# Run the Shiny app
shinyApp(ui = ui, server = server)


