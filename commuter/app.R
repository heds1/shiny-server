library(shiny)
library(dplyr)
library(leaflet)
library(proj4)
library(tidyr)
library(sp)
library(maptools)
library(ggplot2)

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
	filter(value != -999) #%>% # -999 is suppressed data for confidentiality
	#sample_n(size = 5000)

# split types into list of dfs so we can create layers
commute_type_list <- df %>% group_by(CommuteType) %>% group_split()

# get group keys
commute_type_keys <- df %>% group_by(CommuteType) %>% group_keys()

# to get index, can use group keys like this:
grep('Bicycle', commute_type_keys$CommuteType)
# [1] 1

# e.g., 
# bicycle <- commute_type_list[[grep('Bicycle', commute_type_keys$CommuteType)]]
# train <- commute_type_list[[grep('Train', commute_type_keys$CommuteType, ignore.case = TRUE)]]
# company_vehicle <- commute_type_list[[grep('Drive_a_company_car_truck_or_van', commute_type_keys$CommuteType, ignore.case = TRUE)]]
# private_vehicle <- commute_type_list[[grep('Drive_a_private_car_truck_or_van', commute_type_keys$CommuteType, ignore.case = TRUE)]]
# other <- commute_type_list[[grep('Other', commute_type_keys$CommuteType, ignore.case = TRUE)]]
# private_passenger <- commute_type_list[[grep('Passenger_in_a_car_truck_van_or_company_bus', commute_type_keys$CommuteType, ignore.case = TRUE)]]
# bus <- commute_type_list[[grep('Public_bus', commute_type_keys$CommuteType, ignore.case = TRUE)]]
# walk_or_jog <- commute_type_list[[grep('Walk_or_jog', commute_type_keys$CommuteType, ignore.case = TRUE)]]
# ferry <- commute_type_list[[grep('ferry', commute_type_keys$CommuteType, ignore.case = TRUE)]]

# function to create a matrix suitable for passing to addPolylines
create_polyline_matrix <- function(data) {

	data_vector <- c()

	for (row in 1:nrow(data)) {
		data_vector <- c(data_vector, as.numeric(c(data[row,1], data[row,2], data[row,3], data[row,4], NA, NA)))
	}

	return (matrix(data = data_vector, ncol = 2, byrow = TRUE))

}

for (commute_type in commute_type_keys$CommuteType) {
	this_type_data <- commute_type_list[[grep(commute_type, commute_type_keys$CommuteType)]]
	this_mat <- create_polyline_matrix(this_type_data)
	assign(paste0(commute_type, '_lines'), this_mat, envir = .GlobalEnv)
}

# for (commute_type in commute_type_keys$CommuteType) {
# 	print(paste0(commute_type, '_lines'))
# }

# "Bicycle_lines"
# [1] "Drive_a_company_car_truck_or_van_lines"
# [1] "Drive_a_private_car_truck_or_van_lines"
# [1] "Ferry_lines"
# [1] "Other_lines"
# [1] "Passenger_in_a_car_truck_van_or_company_bus_lines"
# [1] "Public_bus_lines"
# [1] "Train_lines"
# [1] "Walk_or_jog_lines"
# [1] "Work_at_home_lines"

# bike_lines <- create_polyline_matrix(bicycle)
# train_lines <- create_polyline_matrix(train)
# company_vehicle_lines <- create_polyline_matrix(company_vehicle)
# private_vehicle_lines <- create_polyline_matrix(private_vehicle)
# private_passenger_lines <- create_polyline_matrix(private_passenger)
# walk_or_jog_lines <- create_polyline_matrix(walk_or_jog)
# ferry_lines <- create_polyline_matrix(ferry)
# bus_lines <- create_polyline_matrix(bus)
#other_lines <- create_polyline_matrix(other)

leaflet() %>%
	addTiles() %>%
	addPolylines(data = Bicycle_lines, group = 'bicycle', color = 'blue') %>%
	addPolylines(data = Train_lines, group = 'train', color = 'blue') %>%
	addPolylines(data = Public_bus_lines, group = 'bus', color = 'blue') %>%
	addPolylines(data = Drive_a_company_car_truck_or_van_lines, group = 'company_vehicle', color = 'blue') %>%
	addPolylines(data = Drive_a_private_car_truck_or_van_lines, group = 'private_vehicle', color = 'blue') %>%
	addPolylines(data = Passenger_in_a_car_truck_van_or_company_bus_lines, group = 'private_passenger', color = 'blue') %>%
	addPolylines(data = Walk_or_jog_lines, group = 'walk_or_jog', color = 'blue') %>%
	addPolylines(data = Ferry_lines, group = 'ferry', color = 'blue') %>%
	#addPolylines(data = bus_lines, group = 'bus', color = 'purple') %>%

	# to control layers
	addLayersControl(
		#baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
		overlayGroups = c("bicycle", "train", "bus", "company_vehicle", "private_vehicle", "private_passenger", "walk_or_jog", "ferry"),
		options = layersControlOptions(collapsed = FALSE)
	)

comm_summ <- df %>%
	group_by(CommuteType) %>%
	summarise(totals = sum(value)) %>%
	mutate(percent = totals / sum(totals) * 100) %>%
	arrange(desc(percent))

# test <- df %>% mutate(id = 1:nrow(.)) %>%
# 	pivot_longer(cols = c(WorkplaceLong, ResidenceLong), names_to = 'Status', values_to='Lng') %>%
# 	pivot_longer(cols = c(WorkplaceLat, ResidenceLat), names_to = 'Status2', values_to='Lat') %>%
# 	filter(case_when(
# 		grepl('Workplace', Status) & grepl('Workplace', Status2) |
# 		grepl('Residence', Status) & grepl('Residence', Status2) ~ TRUE,
# 		TRUE ~ FALSE)) %>%
# 	mutate()


# as.numeric(c(df[1,1], df[1,2], NA, NA, df[1,3], df[1,4]))

pre_matrix_dat <- c()
matrix_df <- data.frame(v1,v2)

for (row in 50000:59000) {

	pre_matrix_dat <- c(pre_matrix_dat, as.numeric(c(df[row,1], df[row,2], df[row,3], df[row,4], NA, NA)))
	

}

my_matrix <- matrix(data = pre_matrix_dat, ncol=2, byrow=TRUE)
my_matrix_df <- as.data.frame(my_matrix)

# df2 <- points_to_line(test, 'Lng', 'Lat', 'id')

# leaflet() %>% addTiles() %>% addPolylines(data=df2)

commute_types <- unique(df$CommuteType)

ui <- {
	tagList(
		navbarPage(
			title="Commuter",
			tabPanel("Map",
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
			),
			tabPanel("Graphs",
				tabsetPanel(
					tabPanel('Commute types',
						plotOutput('commute_types_bar')),
					tabPanel('Graph2')
				)),
			tabPanel("About")
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

		# this_map %>% addPolylines(t2)


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

	output$commute_types_bar <- renderPlot({

		ggplot(comm_summ, aes(x = CommuteType, y = percent)) +
			geom_col() +
			scale_x_discrete(limits = rev(comm_summ$CommuteType)) +
			coord_flip()

	})
	
}

shinyApp(ui, server)

