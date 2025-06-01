library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

rawdata <- read.csv(zstdfile("data/new_data.csv.zst")) |>
  mutate(
    Time = ymd_hms(SrcUpdateTime, tz = "Asia/Taipei"),
    Date = date(Time),
    Hour = hour(Time)
  )
station_list <- read.csv(zstdfile("data/station.csv.zst")) |>
  semi_join(rawdata, by = c("StationUID" = "StationUID"))

data <- rawdata |>
  group_by(UID = StationUID, Date, Hour) |>
  summarise(
    Bikes = mean(AvailableRentBikes),
    Spaces = mean(AvailableReturnBikes),
    Size = mean(AvailableRentBikes + AvailableReturnBikes),
    Value = mean(2 * (
      AvailableRentBikes / (AvailableRentBikes + AvailableReturnBikes)
    ) - 1),
    .groups = "drop"
  )

server <- function(input, output, session) {
  output$time_heatmap_map <- renderLeaflet({
    leaflet(options = leafletOptions(
      minZoom = 14,
      maxZoom = 18,
      maxBounds = list(list(24.97, 121.46), list(25.15, 121.63))
    )) |>
      addTiles() |>
      setView(121.54, 25.017, zoom = 16)
  })
  
  observe({
    bounds <- input$time_heatmap_map_bounds
    if (is.null(bounds)) {
      return()
    }
    
    filtered_stations <- station_list |> filter(
      Latitude >= bounds$south,
      Latitude <= bounds$north,
      Longitude >= bounds$west,
      Longitude <= bounds$east
    )
    
    leafletProxy("time_heatmap_map") |>
      clearMarkers() |>
      addMarkers(
        data = filtered_stations,
        lng = ~ Longitude,
        lat = ~ Latitude,
        layerId = ~ StationUID,
        popup = ~ StationName
      )
  })
  
  output$time_heatmap <- renderPlot({
    #print(session$clientData$pixelratio)
    filtered_data <- data |>
      filter(UID == input$time_heatmap_map_marker_click$id) |>
      complete(
        Date = unique(data$Date),
        Hour = 0:23,
        fill = list(Value = NA)
      ) |>
      mutate(Date_Factor = factor(Date, levels = sort(unique(data$Date), decreasing = TRUE)))
    
    g <- ggplot(filtered_data, aes(
      x = Hour + 0.5,
      y = Date_Factor,
      fill = Value
    )) +
      geom_tile() +
      scale_fill_gradient2(
        limits = c(-1, 1),
        low = "red",
        mid = "white",
        high = "blue",
        na.value = "grey",
        name = "站點狀態\n",
        breaks = c(-1, 0, 1),
        labels = c("無車可借", "供需平衡", "無位可還")
      ) +
      scale_y_discrete(labels = format(ymd(levels(
        filtered_data$Date_Factor
      )), "%Y/%m/%d")) +
      scale_x_continuous(breaks = 0:24) +
      labs(
        title = station_list |>
          filter(
            StationUID == input$time_heatmap_map_marker_click$id
          ) |>
          pull(StationName),
        x = "時間",
        y = "日期"
      ) +
      coord_fixed(
        xlim = c(0, 24),
        ylim = c(0.5, length(levels(
          filtered_data$Date_Factor
        )) + 0.5),
        expand = FALSE,
        ratio = 1
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 24,
          face = "bold"
        ),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(angle = 0, hjust = 1),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 14),
        legend.ticks = element_line(color = "black"),
        legend.ticks.length = unit(c(-4, 0), "points")
      )
    g
  }, width = 720, height = 800) |> # height = 240 for 7-day data
    bindEvent(input$time_heatmap_map_marker_click)
}

ui <- page_navbar(
  theme = bs_theme(base_font = font_google("Roboto")),
  nav_panel(
    "Time Heatmap",
    fluidPage(
      titlePanel("Time Heatmap"),
      leafletOutput("time_heatmap_map"),
      plotOutput("time_heatmap", height = "auto")
    )
  ),
  title = "YouBike",
  tags$head(tags$style(
    HTML(
      "
      .navbar {
        position: sticky;
        top: 0;
        z-index: 48763;
        background: #ffef00;
        box-shadow: black 0 -50px 10px 48px;
      }
      #time_heatmap_map {
        max-width: 720px;
        height: 360px;
        margin: auto;
      }
      #time_heatmap > img {
        display: block;
        width: 100%;
        max-width: 720px;
        height: auto;
        margin: auto;
      }
    "
    )
  ))
)

shinyApp(ui, server)
