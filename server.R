# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")

test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

test_weather_data_generation()
# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike<-city_weather_bike_df %>% group_by(CITY_ASCII, LNG, LAT) %>% slice(which.max(BIKE_PREDICTION))
  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown == 'All') {
      # Then render output plots with an id defined in ui.R
      output$city_bike_map <- renderLeaflet({
        # If All was selected from dropdown, then render a leaflet map with circle markers
        # and popup weather LABEL for all five cities
        leaflet(cities_max_bike) %>% addTiles() %>%
          addCircleMarkers(lng=cities_max_bike$LNG, lat=cities_max_bike$LAT,
                           popup=cities_max_bike$LABEL,
                           radius=~case_when(cities_max_bike$BIKE_PREDICTION_LEVEL=='small' ~ 6,
                                             cities_max_bike$BIKE_PREDICTION_LEVEL=='medium' ~ 10,
                                             cities_max_bike$BIKE_PREDICTION_LEVEL=='large' ~ 12),
                           color=~color_levels(cities_max_bike$BIKE_PREDICTION_LEVEL))
      })
    }
    else {
      # If just one specific city was selected, then render a leaflet map with one marker
      filtered_data<-cities_max_bike %>% filter(CITY_ASCII==input$city_dropdown)
      city_weather_bike_df_filter<- city_weather_bike_df %>% filter(CITY_ASCII==input$city_dropdown)
      output$city_bike_map <- renderLeaflet({
        # on the map and a popup with DETAILED_LABEL displayed
        leaflet(filtered_data) %>% addTiles() %>%
          addCircleMarkers(lng=filtered_data$LNG, lat=filtered_data$LAT,
                           popup=filtered_data$DETAILED_LABEL,
                           radius=~case_when(filtered_data$BIKE_PREDICTION_LEVEL=='small' ~ 6,
                                             filtered_data$BIKE_PREDICTION_LEVEL=='medium' ~ 10,
                                             filtered_data$BIKE_PREDICTION_LEVEL=='large' ~ 12),
                           color=~color_levels(filtered_data$BIKE_PREDICTION_LEVEL))
      })
      output$temp_line <- renderPlot({
        line_plot_temperature <- ggplot(city_weather_bike_df, aes(x = 1:length(TEMPERATURE), y = TEMPERATURE)) +
          geom_line(color="yellow", size=1) +
          labs(x = "Time (3 hours ahead)", y = "TEMPERATURE (C)") +
          geom_point() +
          geom_text(aes(label = paste(TEMPERATURE, " C")), hjust = 0, vjust = 0) +
          ggtitle("Temperature Chart")
        line_plot_temperature
      })
      output$bike_line <- renderPlot({
        city_weather_bike_df_filter$FORECASTDATETIME <- as.POSIXct(city_weather_bike_df_filter$FORECASTDATETIME)
        line_plot_fct <- ggplot(city_weather_bike_df_filter, aes(x = FORECASTDATETIME, y = BIKE_PREDICTION)) +
          geom_line(linetype = "dashed", color = 'green', size = 1) +
          labs(x = "Time (3 hours ahead)", y = "Predicted Bike Count") +
          geom_point() +
          geom_text(aes(label = paste(BIKE_PREDICTION)), hjust = 0, vjust = 0) +
          ggtitle(paste('BIKE_PREDICTION_OF_', input$city_dropdown))
        
        line_plot_fct
      })
      output$bike_date_output <- renderText({
        paste("Time = ", city_weather_bike_df_filter[1,]$FORECASTDATETIME, " ",
              'BikeCountPred = ', city_weather_bike_df_filter[1,]$BIKE_PREDICTION)
        
      })
      output$humidity_pred_chart <- renderPlot({
        line_plot_title <- ggplot(city_weather_bike_df_filter, aes(x = HUMIDITY, y = BIKE_PREDICTION))+
          labs( x = "HUMIDITY", y = "BIKE_PREDICTION", hjust = 0, vjust = 0)+
          geom_point() +
          geom_smooth(method = 'lm', formula = y ~ poly(x, 4))+
          ggtitle(paste('BIKE_PREDICTION vs HUMIDITY_OF_', input$city_dropdown))
        line_plot_title
      })  
    } 
  })
})