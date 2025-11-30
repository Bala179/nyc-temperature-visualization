library(shiny)
library(shinyWidgets)
library(bslib)
library(shinycssloaders)
library(lubridate)
library(leaflet)
library(terra)
library(htmltools)

# ========================================
# FILE LOCATIONS
# ========================================

data_path <- file.path("data")
nyc_extracted_temps_location <- file.path(data_path, "nyc_extracted_temps.csv")

nyc_min_temps_location <- file.path(data_path, "nyc_daily_min.csv")
nyc_max_temps_location <- file.path(data_path, "nyc_daily_max.csv")
nyc_avg_temps_location <- file.path(data_path, "nyc_daily_avg.csv")

# Shapefile source: https://www2.census.gov/geo/tiger/TIGER2010/ZCTA5/2010/tl_2010_36_zcta510.zip
nyc_shapefile_location <- file.path(data_path, "shapefiles", 
                                    "nyc-zipcode", "tl_2010_36_zcta510.shp")

# ========================================
# LOAD PRE-PROCESSED DATA (ONCE AT STARTUP)
# ========================================

nyc_extracted_temps <- read.csv(nyc_extracted_temps_location)
rownames(nyc_extracted_temps) <- nyc_extracted_temps$zipcode

# Min, max, and average temperatures
nyc_min_temps <- read.csv(nyc_min_temps_location)
nyc_max_temps <- read.csv(nyc_max_temps_location)
nyc_avg_temps <- read.csv(nyc_avg_temps_location)

rownames(nyc_min_temps) <- nyc_min_temps$zipcode
rownames(nyc_max_temps) <- nyc_max_temps$zipcode
rownames(nyc_avg_temps) <- nyc_avg_temps$zipcode

# Polygon vector for NYC shapefile
gdf <- vect(nyc_shapefile_location)

# Crop out any polygons outside the range of NYC
e <- ext(-74.35, -73.6, 40.45, 41)
gdf <- crop(gdf, e)

# Raster for temperature values
grid_cells <- rast(xmin=-74.35, xmax=-73.65, ymin=40.45, ymax=41.05, resolution=0.1)

# Project the raster to the same CRS as the vector
grid_cells <- project(grid_cells, crs(gdf))

coords <- crds(grid_cells)

# ========================================
# UI
# ========================================

ui <- page_sidebar(
  title = "NYC Temperature Visualization",
  
  sidebar = sidebar(
    helpText(
      "An interactive temperature visualization of NYC."
    ),
    dateInput(
      inputId = "date",
      label = "Choose a date",
      value = as.Date("2020-01-01")
    ),
    sliderTextInput(
      inputId = "hr",
      label = "Choose an hour (in NY local time)",
      choices = seq(1, 22, by=3), 
      selected = 1,
      grid = TRUE
    )
  ),
  
  leafletOutput(outputId = "map", height = "600px") |> withSpinner()
)

# ========================================
# SERVER
# ========================================

server <- function(input, output, session) {
  
  date_reactive <- reactive({
    input$date
  })
  
  hr_reactive <- reactive({
    input$hr
  })
  
  # ----- LOGIC TO ADJUST HOURS FOR DAYLIGHT SAVING TIME -----
  observeEvent(input$date, {
    req(input$date)
    
    hr <- as.integer(input$hr)
    current_date <- as_datetime(input$date, tz = "America/New_York")
    current_date_time <- current_date + 
      hours(hr)
    
    if (dst(current_date)) {
      # Corner case: when DST ends after midnight on the current date
      if (!dst(current_date + days(1))) {
        
        # Set the new slider value to the closest available value
        if (hr == 1) {
          new_hr <- 2
        } else if (hr %% 3 == 2) {
          new_hr <- hr - 1
        } else {
          new_hr <- hr
        }
        
        uneven_choices <- c(2, 4, 7, 10, 13, 16, 19, 22)
        updateSliderTextInput(session, "hr",
                              label = "Choose an hour (in NY local time)",
                              choices = uneven_choices,
                              selected = new_hr)
        
      } else {
        # Set the new slider value to the closest available value
        if (hr %% 3 == 1) {
          new_hr <- hr + 1
        } else {
          new_hr <- hr
        }
        
        updateSliderTextInput(session, "hr",
                              label = "Choose an hour (in NY local time)",
                              choices = seq(2, 23, by=3),
                              selected = new_hr)
      }
    } else {
      
      # Corner case: when DST sets in after midnight on the current date
      if (dst(current_date + days(1))) {
        
        # Set the new slider value to the closest available value
        if (hr == 2) {
          new_hr <- 1
        } else if (hr %% 3 == 1) {
          new_hr <- hr + 1
        } else {
          new_hr <- hr
        }
        
        uneven_choices <- c(1, 5, 8, 11, 14, 17, 20, 23)
        updateSliderTextInput(session, "hr",
                              label = "Choose an hour (in NY local time)",
                              choices = uneven_choices,
                              selected = new_hr)
      } else {
        # Set the new slider value to the closest available value
        if (hr %% 3 == 2) {
          new_hr <- hr - 1
        } else {
          new_hr <- hr
        }
        
        updateSliderTextInput(session, "hr",
                              label = "Choose an hour (in NY local time)",
                              choices = seq(1, 22, by=3),
                              selected = new_hr)
      }
    }
    
    
  })
  
  output$map <- renderLeaflet({
    hr <- as.integer(hr_reactive())
    
    current_date <- as_datetime(date_reactive(), tz = "America/New_York")
    
    # Convert from NY Local to UTC
    current_date_time_ny <- current_date + 
      hours(hr)
    current_date_time_utc <- with_tz(current_date_time_ny, tzone = "UTC")
    
    # Date validation
    max_valid_date <- as.POSIXct(tail(colnames(nyc_extracted_temps), 1), 
                                 format="X%Y.%m.%d.%H.%M.%S", tz="UTC")
    
    validate(
      need(current_date_time_utc >= as.Date("2020-01-01"), 
           "Date must be in 2020 or after (by UTC)"),
      need(current_date_time_utc < max_valid_date + hours(3), 
           "Data is not yet available for this date and time")
    )
    
    if (is.na(current_date_time_ny)) {
      return()
    }
    
    # Prevent errors that may get thrown just before the slider updates its values
    # We want the slider minimum to be at 2 when DST is active, and 1 otherwise
    # This will cancel processing if these conditions are not met
    if ((dst(current_date_time_ny) & hr %% 3 == 1) |
        (!dst(current_date_time_ny) & hr %% 3 == 2)) {
      
      # Corner case: if DST is ending on that day and the selected time is 2 am,
      # we need to subtract off 1 hour from the converted UTC time.
      # If not, the date and time must not be valid.
      if (hr == 2 & dst(current_date) & !dst(current_date + days(1))) {
        current_date_time_utc <- current_date_time_utc - hours(1)
      } else {
        return()
      }
    }
    
    # Header formats of date and time in the CSV files
    formatted_datetime_header <- format(current_date_time_utc, "X%Y.%m.%d.%H.%M.%S")
    formatted_date_header <- format(current_date_time_ny, "X%Y.%m.%d")
    
    # Store the extracted temperatures for each region - ensuring that the
    # zipcodes are ordered correctly
    gdf$extracted_temps <- nyc_extracted_temps[gdf$ZCTA5CE10, formatted_datetime_header]
    
    gdf <- project(gdf, '+proj=longlat +datum=WGS84')
    
    palette <- colorNumeric(
      palette = "RdYlGn", domain = gdf$extracted_temps,
      na.color = "transparent", reverse = TRUE
    )
    
    # Add the min, max, and average temperatures if those are available
    if(formatted_date_header %in% colnames(nyc_min_temps)) {
      gdf$min_temps <- nyc_min_temps[gdf$ZCTA5CE10, formatted_date_header]
      gdf$max_temps <- nyc_max_temps[gdf$ZCTA5CE10, formatted_date_header]
      gdf$avg_temps <- nyc_avg_temps[gdf$ZCTA5CE10, formatted_date_header]
    }
    
    # Add tooltip text (unless the temperature is NA)
    # If min, max, and average values are available, display those too
    tooltip <- ifelse(
      is.na(gdf$extracted_temps), "",
      paste(
        "Zipcode: ", gdf$ZCTA5CE10, "<br/>",
        "Temperature: ", round(gdf$extracted_temps, 2), "°C",
        ifelse(is.na(gdf$min_temps), "", 
               paste(
                 "<br/>", "Daily High: ", round(gdf$max_temps, 2), "°C",
                 "<br/>", "Daily Low: ", round(gdf$min_temps, 2), "°C",
                 "<br/>", "Daily Average: ", round(gdf$avg_temps, 2), "°C",
                 sep = ""
               )
        ),
        sep = ""
      )
    ) |>
      lapply(htmltools::HTML)
    
    # Plot temperature as map
    leaflet(gdf) |>
      addTiles() |>
      setView(lng = -74, lat = 40.75, zoom = 10) |>
      addPolygons(
        stroke = TRUE, color = "white", weight = 0.3,
        fillOpacity = 0.8, smoothFactor = 0.5,
        fillColor = ~palette(extracted_temps),
        label = ~tooltip,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) |>
      addLegend(
        pal = palette, values = ~extracted_temps, opacity = 0.9,
        title = "Temperature (°C)", position = "topleft"
      )
  })
  
}

# Run app ----
shinyApp(ui, server)