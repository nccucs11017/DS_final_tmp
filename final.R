library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

rawdata <- read.csv(zstdfile("data/data.csv.zst")) |>
  mutate(
    Time = ymd_hms(SrcUpdateTime, tz = "Asia/Taipei"),
    Date = date(Time),
    Hour = hour(Time)
  )
# rawdata <- rawdata |> filter(longitude > 121.5285,
#                              longitude < 121.5520,
#                              latitude > 25.0076,
#                              latitude < 25.0266)
station_list <- read.csv(zstdfile("data/station.csv.zst")) |>
  semi_join(rawdata, by = c("sno" = "StationID"))



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
      setView(121.54, 25.017, zoom = 16) |>
      addMarkers(
        lng = station_list$longitude,
        lat = station_list$latitude,
        layerId = paste("TPE", station_list$sno, sep = ""),
        popup = paste(station_list$sna, "<br>TPE", station_list$sno, sep = "")
      )
  })
  
  observe({
    bounds <- input$time_heatmap_map_bounds
    if (is.null(bounds)) {
      return()
    }
    
    filtered_stations <- station_list |> filter(
      latitude >= bounds$south,
      latitude <= bounds$north,
      longitude >= bounds$west,
      longitude <= bounds$east
    )
    
    leafletProxy("time_heatmap_map") |>
      clearMarkers() |>
      addMarkers(
        data = filtered_stations,
        lng = ~ longitude,
        lat = ~ latitude,
        layerId = paste("TPE", filtered_stations$sno, sep = ""),
        popup = ~ sna
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
      mutate(Date_Factor = factor(Date, levels = rev(unique(data$Date))))
    
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
            paste0("TPE", sno) == input$time_heatmap_map_marker_click$id
          ) |>
          pull(sna),
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
          size = 16,
          face = "bold"
        ),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(angle = 0, hjust = 1),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.ticks = element_line(color = "black"),
        legend.ticks.length = unit(c(-4, 0), "points")
      )
    g
  }, width = 720, height = 240) |>
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
