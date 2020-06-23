library(shiny)
library(dplyr)
library(leaflet)
library(proj4)
library(tidyr)
library(sp)
library(maptools)

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {

  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)

  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }

  # If there is only one path...
  if (is.null(id_field)) {

    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))

    return(lines)

    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  

    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])

    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))

    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }

    return(sp_lines)
  }
}

# set up parameters for coordinate conversion (NZTM --> NZGD)
proj4_params <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

# load data, convert coordinates
df <- read.csv(paste0(getwd(), "/2018-census-main-means-of-travel-to-work-by-statistical-area.csv")) %>%
	mutate(		# convert to lat/lng
		WorkplaceLong = (project(list(SA2_workplace_easting, SA2_workplace_northing), proj=proj4_params, inverse=T))$x,
		WorkplaceLat = (project(list(SA2_workplace_easting, SA2_workplace_northing), proj=proj4_params, inverse=T))$y,
		ResidenceLong = (project(list(SA2_usual_residence_easting, SA2_usual_residence_northing), proj=proj4_params, inverse=T))$x,
		ResidenceLat = (project(list(SA2_usual_residence_easting, SA2_usual_residence_northing), proj=proj4_params, inverse=T))$y) %>%
	select(
		WorkplaceLong,
		WorkplaceLat,
		ResidenceLong,
		ResidenceLat,
		Work_at_home,
		Drive_a_private_car_truck_or_van,
		Drive_a_company_car_truck_or_van,
		Passenger_in_a_car_truck_van_or_company_bus,
		Public_bus,
		Train,
		Bicycle,
		Walk_or_jog,
		Ferry,
		Other) %>%
	pivot_longer(
		cols = c(Work_at_home,
			Drive_a_private_car_truck_or_van,
			Drive_a_company_car_truck_or_van,
			Passenger_in_a_car_truck_van_or_company_bus,
			Public_bus,
			Train,
			Bicycle,
			Walk_or_jog,
			Ferry,
			Other),
		names_to = 'CommuteType') %>%
	filter(value != -999) %>% # -999 is suppressed data for confidentiality
	sample_n(size = 5000)

# comm_summ <- df %>%
# 	group_by(CommuteType) %>%
# 	summarise(totals = sum(value)) %>%
# 	arrange(desc(totals))

test <- df %>% mutate(id = 1:nrow(.)) %>%
	pivot_longer(cols = c(WorkplaceLong, ResidenceLong), names_to = 'Status', values_to='Lng') %>%
	pivot_longer(cols = c(WorkplaceLat, ResidenceLat), names_to = 'Status2', values_to='Lat') %>%
	filter(case_when(
		grepl('Workplace', Status) & grepl('Workplace', Status2) |
		grepl('Residence', Status) & grepl('Residence', Status2) ~ TRUE,
		TRUE ~ FALSE

	))

df2 <- points_to_line(test, 'Lng', 'Lat', 'id')

leaflet() %>% addTiles() %>% addPolylines(data=df2)

commute_types <- unique(df$CommuteType)

ui <- {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h2('sidebar'),
		selectizeInput('commute_types',
			'Commute type',
			choices = commute_types, 
			multiple = TRUE,
			selected = 'Public_bus')
      ),
      mainPanel(
        leafletOutput('map')
      )
      
    )
  )
}

server <- function(input, output, session) {
  
	map_data <- reactive({
		df %>% filter(
			CommuteType %in% input$commute_types
		)
	})

  	output$map <- renderLeaflet({

    	this_map <- leaflet(map_data()) %>%
      		addTiles() # %>%

		this_map %>% addPolylines(t2)


		# addCircleMarkers(~WorkplaceLong, ~WorkplaceLat,
		#                  stroke=FALSE, fillOpacity=0.1)
		# fitBounds(172,-35,178,-45)

		for (i in 1:nrow(map_data())) {
			this_map <- addPolylines(this_map,
				lat = as.numeric(map_data()[i, c('WorkplaceLat', 'ResidenceLat')]),
				lng = as.numeric(map_data()[i, c('WorkplaceLong', 'ResidenceLong')]),
				weight = map_data()[i, 'value'])
		}

		this_map
    
  })
  
}

shinyApp(ui, server)

