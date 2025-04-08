library(shiny)
library(leaflet)
library(jsonlite)
library(ggplot2)
library(tidyverse)
library(shinydashboard)
library(dplyr)
library(shinycssloaders)

interface <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "searchbar", buttonId = "searchbtn", label = "Enter city name"),
      menuItem("Weather", tabName = "weather", icon = icon("cloud-sun")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @font-face {
          font-family: 'Hulk';
          src: url('hulk.ttf') format('truetype');
        }
        .box, .box-header, .box-body, .box-title, .box .text-output {
          font-family: 'Hulk', sans-serif !important;
        }
        .weather-box {
          background-image: url('cloud.jpg');
          background-size: cover;
          background-position: center;
          color: #333333;
        }
        .weather-box .fa {
          color: #333333;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "weather",
              fluidRow(
                column(
                  width = 6,
                  box(
                    width = NULL,
                    class = "weather-box",
                    status = "primary",
                    solidHeader = TRUE,
                    fluidRow(
                      column(
                        width = 12,
                        uiOutput("climate_symbol"),
                        div(
                          style = "font-size: 36px; font-weight: bold; margin-top: 10px; text-align: center; color: #333333;",
                          textOutput("temp_display")
                        ),
                        div(
                          style = "font-size: 20px; font-weight: bold; margin-top: 10px; text-align: center; color: #333333;",
                          textOutput("place_name")
                        )
                      )
                    ),
                    div(
                      style = "position: absolute; top: 10px; left: 10px; font-size: 14px; color: #333333;",
                      textOutput("current_date")
                    )
                  ),
                  box(width = 6, title = div(icon("droplet"), "Humidity"), textOutput("moisture"), background = "light-blue"),
                  box(width = 6, title = div(icon("temperature-high"), "Feels Like"), textOutput("perceived_temp"), background = "orange"),
                  box(width = 6, title = div(icon("smog"), "Condition"), textOutput("climate_status"), background = "olive"),
                  box(width = 6, title = div(icon("eye"), "Visibility"), textOutput("clarity"), background = "teal"),
                  box(width = 6, title = div(icon("wind"), "Wind Speed"), textOutput("breeze_velocity"), background = "navy"),
                  box(width = 6, title = div(icon("globe-americas"), "Air Pressure"), textOutput("atmo_pressure"), background = "maroon")
                ),
                column(
                  width = 6,
                  box(
                    width = NULL,
                    status = "info",
                    solidHeader = TRUE,
                    withSpinner(leafletOutput("geo_map", height = 465))
                  )
                )
              )
      ),
      tabItem(tabName = "forecast",
              fluidRow(
                column(
                  width = 3,
                  box(
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    div(
                      style = "margin-bottom: 10px;",
                      icon("map-marker-alt"),
                      " Location: ",
                      textOutput("place_name_", inline = TRUE)
                    ),
                    selectInput(
                      "feature",
                      "Features:",
                      list("temp", "feels_like", "temp_min", "temp_max", "pressure", "sea_level", "grnd_level", "humidity", "speed", "deg", "gust")
                    ),
                    img(src = "here.jpg", width = "100%", style = "margin-top: 10px;")
                  )
                ),
                column(
                  width = 9,
                  box(
                    width = NULL,
                    status = "success",
                    solidHeader = TRUE,
                    withSpinner(plotOutput("trend_graph", height = 465))
                  )
                )
              )
      )
    )
  )
)

get_coords_from_city <- function(city) {
  api_key <- "6c49000156f8a5671ce25c1df4217d2d"
  url <- sprintf("http://api.openweathermap.org/geo/1.0/direct?q=%s&limit=1&appid=%s", URLencode(city), api_key)
  response <- fromJSON(url)
  if (length(response) > 0) {
    return(list(lat = response$lat[1], lon = response$lon[1]))
  }
  return(NULL)
}

get_weather_details <- function(lat, lon) {
  api_key <- "6c49000156f8a5671ce25c1df4217d2d"
  url <- sprintf("https://api.openweathermap.org/data/2.5/weather?lat=%s&lon=%s&appid=%s", lat, lon, api_key)
  response <- fromJSON(url)
  return(response)
}

process_weather_data <- function(raw_data) {
  list(
    place = raw_data$name,
    temp = raw_data$main$temp - 273.2,
    feels_like = raw_data$main$feels_like - 273.2,
    humidity = raw_data$main$humidity,
    condition = raw_data$weather$description[[1]],
    visibility = raw_data$visibility / 1000,
    wind_speed = raw_data$wind$speed,
    pressure = raw_data$main$pressure
  )
}

get_forecast_details <- function(lat, lon) {
  api_key <- "6c49000156f8a5671ce25c1df4217d2d"
  url <- sprintf("https://api.openweathermap.org/data/2.5/forecast?lat=%s&lon=%s&appid=%s", lat, lon, api_key)
  return(fromJSON(url))
}

process_forecast_data <- function(raw_data) {
  data.frame(
    time = as.POSIXct(raw_data$list$dt_txt, tz = "UTC"),
    temp = raw_data$list$main$temp - 273.2,
    feels_like = raw_data$list$main$feels_like - 273.2,
    temp_min = raw_data$list$main$temp_min - 273.2,
    temp_max = raw_data$list$main$temp_max - 273.2,
    pressure = raw_data$list$main$pressure,
    sea_level = raw_data$list$main$sea_level,
    grnd_level = raw_data$list$main$grnd_level,
    humidity = raw_data$list$main$humidity,
    speed = raw_data$list$wind$speed,
    deg = raw_data$list$wind$deg,
    gust = raw_data$list$wind$gust
  )
}

server <- function(input, output, session) {
  
  app_state <- reactiveValues(
    coords = list(lat = 21.0227396, lon = 105.8369637),
    weather_raw = get_weather_details(21.0227396, 105.8369637),
    forecast_raw = get_forecast_details(21.0227396, 105.8369637)
  )
  
  update_data <- function(lat, lon) {
    app_state$coords$lat <- lat
    app_state$coords$lon <- lon
    app_state$weather_raw <- get_weather_details(lat, lon)
    app_state$forecast_raw <- get_forecast_details(lat, lon)
  }
  
  current_coords <- reactive({
    list(lat = app_state$coords$lat, lon = app_state$coords$lon)
  })
  
  output$geo_map <- renderLeaflet({
    coords <- current_coords()
    leaflet() %>%
      addTiles() %>%
      setView(lng = coords$lon, lat = coords$lat, zoom = 10) %>%
      addMarkers(lng = coords$lon, lat = coords$lat)
  })
  
  observeEvent(input$geo_map_click, {
    click <- input$geo_map_click
    update_data(click$lat, click$lng)
    leafletProxy("geo_map") %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat)
  })
  
  observeEvent(input$searchbtn, {
    if (nchar(input$searchbar) > 0) {
      coords <- get_coords_from_city(input$searchbar)
      if (!is.null(coords)) {
        update_data(coords$lat, coords$lon)
        leafletProxy("geo_map") %>%
          clearMarkers() %>%
          setView(lng = coords$lon, lat = coords$lat, zoom = 10) %>%
          addMarkers(lng = coords$lon, lat = coords$lat)
      } else {
        showNotification("City not found!", type = "error")
      }
    } else {
      showNotification("Please enter a city name!", type = "warning")
    }
  })
  
  weather_processed <- reactive({
    req(app_state$weather_raw)
    process_weather_data(app_state$weather_raw)
  })
  
  output$climate_symbol <- renderUI({
    desc <- weather_processed()$condition
    symbol_style <- "font-size: 60px; text-align: center; width: 100%; color: #333333;"
    if (grepl("clear", desc)) icon("sun", style = symbol_style)
    else if (grepl("rain", desc)) icon("cloud-rain", style = symbol_style)
    else icon("cloud", style = symbol_style)
  })
  
  output$temp_display <- renderText({ paste(round(weather_processed()$temp, 1), "°C") })
  output$place_name <- renderText({ weather_processed()$place })
  output$moisture <- renderText({ paste(weather_processed()$humidity, "%") })
  output$perceived_temp <- renderText({ paste(weather_processed()$feels_like, "°C") })
  output$climate_status <- renderText({ weather_processed()$condition })
  output$clarity <- renderText({ paste(weather_processed()$visibility, "Km") })
  output$breeze_velocity <- renderText({ paste(weather_processed()$wind_speed, "Km/h") })
  output$atmo_pressure <- renderText({ paste(weather_processed()$pressure, "hPa") })
  output$current_date <- renderText({ format(Sys.Date(), "%B %d, %Y") })
  
  forecast_processed <- reactive({
    req(app_state$forecast_raw)
    process_forecast_data(app_state$forecast_raw)
  })
  
  output$place_name_ <- renderText({ weather_processed()$place })
  output$trend_graph <- renderPlot({
    data <- forecast_processed()
    ggplot(data, aes(x = time, y = .data[[input$feature]])) +
      geom_line(color = "red") +
      geom_point(color = "black") +
      labs(x = "Time", y = input$feature) +
      theme_minimal() +
      scale_x_datetime(date_labels = "%b %d")
  })
}

shinyApp(interface, server)