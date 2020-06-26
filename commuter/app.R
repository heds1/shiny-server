library(shiny)
library(dplyr)
library(leaflet)
library(proj4)
library(tidyr)
#library(sp)
#library(maptools)
library(ggplot2)
library(shinycssloaders)
library(shinyjs)

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
	# -999 is suppressed data for confidentiality
	filter(value != -999) %>%
	# threshold the counts into bins so we can pass a weight to addPolylines
	mutate(
		wt = case_when(
			value <10 ~ 1,
			value >=10 & value <20 ~ 2,
			value >=20 & value <30 ~ 3,
			value >=30 & value <40 ~ 4,
			value >=40 & value <50 ~ 5,
			value >50 ~ 6)) %>%
	filter(CommuteType != 'Other',
		CommuteType != 'Work_at_home')

# test <- Bicycle_lines[1000:1006,]

# leaflet() %>% addTiles() %>%
# 	addPolylines(data = test, weight = c(1, 6))

# split weights into list of dfs so we can get appropriate vals
line_weight_list <- df %>% select(CommuteType, wt) %>% group_by(CommuteType) %>% group_split()

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

# create all the matrices
for (commute_type in commute_type_keys$CommuteType) {
	this_type_data <- commute_type_list[[grep(commute_type, commute_type_keys$CommuteType)]]
	this_mat <- create_polyline_matrix(this_type_data)
	assign(paste0(commute_type, '_lines'), this_mat, envir = .GlobalEnv)
}



# test <- df %>% mutate(id = 1:nrow(.)) %>%
# 	pivot_longer(cols = c(WorkplaceLong, ResidenceLong), names_to = 'Status', values_to='Lng') %>%
# 	pivot_longer(cols = c(WorkplaceLat, ResidenceLat), names_to = 'Status2', values_to='Lat') %>%
# 	filter(case_when(
# 		grepl('Workplace', Status) & grepl('Workplace', Status2) |
# 		grepl('Residence', Status) & grepl('Residence', Status2) ~ TRUE,
# 		TRUE ~ FALSE)) %>%
# 	mutate()


# as.numeric(c(df[1,1], df[1,2], NA, NA, df[1,3], df[1,4]))

# pre_matrix_dat <- c()
# matrix_df <- data.frame(v1,v2)

# for (row in 50000:59000) {

# 	pre_matrix_dat <- c(pre_matrix_dat, as.numeric(c(df[row,1], df[row,2], df[row,3], df[row,4], NA, NA)))
	

# }

# my_matrix <- matrix(data = pre_matrix_dat, ncol=2, byrow=TRUE)
# my_matrix_df <- as.data.frame(my_matrix)

# function to get line weights
get_line_weights <- function(commute_type) {
	return (line_weight_list[[grep(commute_type, commute_type_keys, ignore.case = TRUE)]]$wt)
}

# colors
my_pal_hex <- brewer.pal(8, "Dark2")
names(my_pal_hex) <- commute_type_keys$CommuteType
my_pal <- colorFactor(my_pal_hex, domain = names(my_pal_hex))

# my_pal <- colorFactor(my_pal_hex, domain = commute_type_keys$CommuteType)


ui <- {
	tagList(
		useShinyjs(),
		tags$head(
			tags$style(
				'.shiny-notification{
					position: fixed;
					top: 33%;
					left: 33%;
					right: 33%;'

			)
		),
		navbarPage(
			
			title="There and Back Again",
			tabPanel("Map",
				sidebarLayout(
					sidebarPanel(
						h2('How Aotearoa Gets to Work', class="display-2"),
						actionButton('load_data','Get started!')
					),
					mainPanel(
						leafletOutput('map', height = '90vh')
					)
    			)
				
			),
			tabPanel("Graphs",
				tabsetPanel(
					tabPanel('Commute types',
						plotOutput('commute_types_bar')) %>% withSpinner(),
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

	# the strategy is to just load the base map on instantiation. layers can be
	# added by proxy, so that server load is not front-loaded.
  	output$map <- renderLeaflet({

		leaflet(options = leafletOptions(minZoom = 4)) %>%
			addTiles() %>%
    		setView(174,-41.2,6) %>%
			addLegend(
				position = "bottomright",
        		pal = my_pal,
				values = names(my_pal_hex)) %>%
				#values = commute_type_keys$CommuteType) %>%
				

			# to control layers
			addLayersControl(
				#baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
				overlayGroups = c("Bicycle", "Train", "Bus", "Company vehicle", "Private vehicle", "Private passenger", "Walk or jog", "Ferry"),
				options = layersControlOptions(collapsed = FALSE)) %>%

			# hide most layers on startup
			hideGroup(c("Bicycle", "Train", "Company vehicle", "Private vehicle", "Private passenger", "Walk or jog", "Ferry"))
    
	})

	# add layers
	observeEvent(input$load_data, {
		shinyjs::disable('load_data')
		withProgress(message = 'Loading data...', value = 0, {
			n <- 8
			incProgress(1/n, "Loading bicycles...")
			map <- leafletProxy("map") %>%
				addPolylines(
					data = Bicycle_lines,
					group = 'Bicycle',
					#color = my_pal('Bicycle'),
					color = my_pal_hex[['Bicycle']],

					weight = get_line_weights('bicycle'))
			
			incProgress(1/n, "Loading trains...")
			map <- addPolylines(map,
				data = Train_lines,
				group = 'Train',
				#color = my_pal('Bicycle'),
				color = my_pal_hex[['Train']],
				weight = get_line_weights('train'))
			
			incProgress(1/n, "Loading buses...")
			map <- addPolylines(map,
				data = Public_bus_lines,
				group = 'Bus',
				#color = my_pal('Public_bus'),
				color = my_pal_hex[['Public_bus']],
				weight = get_line_weights('bus'))
			
			incProgress(1/n, "Loading company vehicles...")
			map <- addPolylines(map,
				data = Drive_a_company_car_truck_or_van_lines,
				group = "Company vehicle",
				color = my_pal_hex[['Drive_a_company_car_truck_or_van']],
				weight = get_line_weights('company'))

			incProgress(1/n, "Loading private vehicles...")
			map <- addPolylines(map,
				data = Drive_a_private_car_truck_or_van_lines,
				group = "Private vehicle",
				color = my_pal_hex[['Drive_a_private_car_truck_or_van']],
				weight = get_line_weights('private'))
			
			incProgress(1/n, "Loading passengers...")
			map <- addPolylines(map,
				data = Passenger_in_a_car_truck_van_or_company_bus_lines,
				group = 'Private passenger',
				color = my_pal_hex[['Passenger_in_a_car_truck_van_or_company_bus']],
				#color = my_pal_hex[6],
				weight = get_line_weights('passenger'))
			
			incProgress(1/n, "Loading walkers...")
			map <- addPolylines(map,
				data = Walk_or_jog_lines,
				group = 'Walk or jog',
				#color = my_pal_hex[7],
				color = my_pal_hex[['Walk_or_jog']],
				weight = get_line_weights('walk'))
			
			incProgress(1/n, "Loading ferries...")
			map <-  addPolylines(map,
				data = Ferry_lines,
				group = 'Ferry',
				#color = my_pal_hex[8],
				color = my_pal_hex[['Ferry']],
				weight = get_line_weights('ferry'))
			
			map
		})


	})

	output$commute_types_bar <- renderPlot({

		comm_summ <- df %>%
			group_by(CommuteType) %>%
			summarise(totals = sum(value)) %>%
			mutate(percent = totals / sum(totals) * 100) %>%
			arrange(desc(percent))

		ggplot(comm_summ, aes(x = CommuteType, y = percent)) +
			geom_col() +
			scale_x_discrete(limits = rev(comm_summ$CommuteType)) +
			coord_flip()

	})
	
}

shinyApp(ui, server)

