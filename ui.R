# Load required libraries
require(leaflet)


# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
  titlePanel("Bike-sharing demand prediction app"), 
  # Create a side-bar layout
  sidebarLayout(position = "left",
    # Create a main panel to show cities on a leaflet map
    mainPanel(
    # leaflet output with id = 'city_bike_map', height = 1000
    leafletOutput("city_bike_map", height = 1000)
    ),
    # Create a side bar to show detailed plots for a city
    sidebarPanel(
    selectInput("city_dropdown", "City: ", c("All", "Seoul", "Suzhou", "London", "New York", "Paris")),
    plotOutput('temp_line'),
    plotOutput('bike_line', click = "plot_click"),
    verbatimTextOutput("bike_date_output"), 
    plotOutput('humidity_pred_chart')
    ))
    # select drop down list to select city
))