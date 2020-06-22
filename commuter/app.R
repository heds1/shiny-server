library(shiny)
library(dplyr)
library(leaflet)
library(proj4)

# set up parameters for coordinate conversion (NZTM --> NZGD)
proj4_params <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

# load data, convert coordinates
df <- read.csv(paste0(getwd(), "/2018-census-main-means-of-travel-to-work-by-statistical-area.csv")) %>%
  mutate(
    WorkplaceLong = (project(list(SA2_workplace_easting, SA2_workplace_northing), proj=proj4_params, inverse=T))$x,
    WorkplaceLat = (project(list(SA2_workplace_easting, SA2_workplace_northing), proj=proj4_params, inverse=T))$y
  )




ui <- {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h2('sidebar')
      ),
      mainPanel(
        leafletOutput('map')
      )
      
    )
  )
}

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(~WorkplaceLong, ~WorkplaceLat,
                       stroke=FALSE, fillOpacity=0.1)
      # fitBounds(172,-35,178,-45)
  })
  
}

shinyApp(ui, server)

