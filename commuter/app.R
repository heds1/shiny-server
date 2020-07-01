library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(shinycssloaders)
library(shinyjs)
library(rgdal)
library(rmapshaper) # for simplify (compress polygons)

# function to get line weights for a commute type. takes a commute_type argument
# that is used to match with the names of line_weights to get the correct
# dataframe containing the weights. weights are not matched explicitly, they are
# simply matched by order. that is, it's assumed that the matrix passed to
# addPolylines has the line data in the same order as that passed in as weights.

get_line_weights <- function(commute_type) {
	return (line_weights[[paste0(commute_type, 'LineWeights')]]$Weight)
}

# commuter line data are stored in data/line-matrices/*.txt
# read these into a list of matrices
line_matrices <- list()
for (file in dir("./data/line-matrices")) {
	line_matrices[[sub('.txt', '', file)]] <- as.matrix(
		read.table(paste0(getwd(), '/data/line-matrices/', file)))
}

# commute line weights are stored in data/line-weights/*.csv
# read these into a list of dfs
line_weights <- list()
for (file in dir("./data/line-weights")) {
	line_weights[[sub('.csv', '', file)]] <- read.csv(
		paste0(getwd(), '/data/line-weights/', file)
	)
}

# territorial authority (TA) and regional council (REGC) polygons are stored in
# shapefiles, read in here
polygon_layers <- list()

# read in
# polygon_layers[['territorial']] <- readOGR(
# 	dsn = "C:/Users/hls/code/statsnz/ta",
#     layer = "territorial-authority-2018-generalised")

polygon_layers[['regional']] <- readOGR(
	dsn = "C:/Users/hls/code/statsnz/regc",
	layer = "regional-council-2018-generalised")

polygon_layers[['regional']] <- rmapshaper::ms_simplify(polygon_layers[['regional']])

# group names. todo make these nicer
commute_groups <- gsub('LineMatrix', '', names(line_matrices))


df <- read.csv(paste0(getwd(), '/cleaned-commuter-data.csv'))

regional_coordinates <- read.csv('C:/Users/hls/code/shiny-server/commuter/data/regional-coordinates.csv')



# colors
#my_pal_hex <- brewer.pal(length(line_matrices), "Paired")
my_pal_hex <- c(
	'#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99',
	'#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A')
names(my_pal_hex) <- names(line_matrices)
my_pal <- colorFactor(my_pal_hex, domain = names(my_pal_hex))

leaflet_labels_js <- "shinyjs.addLabels = function() {
		$('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Select commuter types</label>');
		$('.leaflet-control-layers-base').prepend('<label style=\"text-align:center\">Select area boundaries</label>');
	}"

ui <- {
	tagList(
		useShinyjs(),
		extendShinyjs(text=leaflet_labels_js),
		includeCSS('C:/Users/hls/code/shiny-server/commuter/style.css'),
		fluidPage(
			fluidRow(
				div(class="col-sm-4",
					tags$form(class="well",
						h3('How Aotearoa Gets to Work', class="display-2", style="text-align: center;"),
						hr(),
						tabsetPanel(
							id = "hidden_tabs",
							type = "hidden",
							tabPanelBody("home_panel",
								p("Use this tool to explore how Kiwis get to work, based on Census 2018 data."),
								p("The lines on the map represent the distance travelled by people using different
									modes of transport. Thicker lines mean more people used that mode of transport for that particular journey.
									You can explore the data further by checking out the graphs on the left."),
								div(
									p(style="display:inline", "Click on the button below to get started, or "),
									actionLink('about_controller',
									"learn more about these data.")
								),
								hr(),
								div(style="text-align: center;", 
									actionButton('load_data',"Let's get to work!")
								)
							),
							tabPanelBody("about_panel",
								div(style="padding-bottom: 10px;",
									p(style="display:inline", "This tool uses the "),
									a(href="https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/",
										"Statistics New Zealand 2018 Census Commuter View dataset"),
									p(style="display:inline", "to map the journeys of respondents from their place of residence to their place of work.")
								),
								
								div(style="padding-bottom: 10px;",
									p(style="display:inline", "The regional council boundaries were obtained from the "),
									a(href="https://datafinder.stats.govt.nz/layer/95065-statistical-area-2-higher-geographies-2018-generalised/",
										"Statistical Area 2 Higher Geographies 2018 dataset."),
									p(style="display:inline", "The location data were mapped to the regional boundaries by assignment 
										into Statistical Area 2 (SA2) zones, which are intended to group the population into defined areas that
										interact together socioeconomically.")
								),
								div(
									p(style="display:inline", "The data described above was further processed for use within this tool. The code used to reproduce these
										data processing steps, as well as the full code for the application, is open source and 
										available on the author's "),
									a(href="https://github.com/heds1/shiny-server/tree/master/commuter", "Github repo"),
									p(style="display:inline", " for this project. Questions, ideas, bug reports or observations about the weather are all very welcome"),
									HTML("&mdash;"),
									p(style="display:inline", "please use "),
									a(href="https://www.hedleystirrat.co.nz/about/", "this form "),
									p(style="display:inline", "to contact the author.")
								),
								hr(),
								div(style="text-align: center",
									actionButton('home_controller', "Return home"))
							)
						)
					),
					tags$form(class="well",
						tabsetPanel(
							tabPanel("Compare regions", plotOutput("regional_comparison")),
							tabPanel("All regions", plotOutput("plot2")),
							tabPanel("Yet another plot", plotOutput("plot3"))
						)
					)
				),
				div(class="col-sm-8",
					tags$form(class='well',
						leafletOutput('map', height = '90vh'))
				)
			)
		)
	)
}


server <- function(input, output, session) {

	# show 'about' panel
	observeEvent(input$about_controller, {
    	updateTabsetPanel(session, "hidden_tabs", selected = 'about_panel')
	}) 

	# show home panel
	observeEvent(input$home_controller, {
		updateTabsetPanel(session, "hidden_tabs", selected = "home_panel")
	})
  
	# the strategy is to just load the base map on instantiation. layers can be
	# added by proxy, so that server load is not front-loaded.
  	output$map <- renderLeaflet({ 

		map <- leaflet(options = leafletOptions(minZoom = 4)) %>%
			addTiles() %>%
    		setView(174,-41.2,6) %>%
			addLegend(
				position = "bottomright",
        		pal = my_pal,
				values = names(my_pal_hex)) %>%
			addLayersControl(
				baseGroups = c('None', names(polygon_layers)),
				overlayGroups = commute_groups, #c(commute_groups, names(polygon_layers)),
				options = layersControlOptions(collapsed = FALSE)) %>%

			# hide most layers on startup
			# hideGroup(c(commute_groups, names(polygon_layers)))
			hideGroup(commute_groups)

		return (map)
    
	})

	# zoom to region on click
	# inspiration from SymbolixAU here:
	# https://stackoverflow.com/questions/42771474/r-shiny-leaflet-click-on-shape-and-zoom-to-bounds-using-maps-package
	observe({
        click <- input$map_shape_click
        if(is.null(click))
            return()

		# get lng/lat/zoom data for this region with click$id
		this_region <- regional_coordinates[match(click$id, regional_coordinates$id),]

		# update map with lng/lat/zoom for this region
        leafletProxy("map") %>% 
            setView(
				lng = this_region$lng,
				lat = this_region$lat,
				zoom = this_region$zoom)
    })

	# render plot
	output$regional_comparison <- renderPlot({

		# todo do this outside this function globally
		names(my_pal_hex) <- gsub("LineMatrix","", names(my_pal_hex))
		
		# get clicked region
		click <- input$map_shape_click

		# average commute rates for whole nation
		national_comparison <- df %>%
			group_by(CommuteType) %>%
			summarise(Sum = sum(Count)) %>%
			mutate(Proportion = Sum / sum(Sum) * 100) %>%
			arrange(desc(Proportion))
		
		# if user hasn't clicked on a region, then render national comparison
		if (is.null(click)) {
			
			# national_comparison %>%
			# 	ggplot() +
			# 		geom_col(aes(x = CommuteType, y = Proportion, fill = CommuteType)) +
			# 		scale_x_discrete(limits = rev(national_comparison$CommuteType)) +
			# 		coord_flip() +
			# 		ggtitle("National averages") +
			# 		theme(
			# 			plot.title = element_text(hjust = 0.5),
			# 			panel.grid.major = element_blank(),
			# 			panel.grid.minor = element_blank(),
			# 			panel.background = element_blank(),
			# 			axis.title.y = element_blank(),
			# 			axis.ticks.y = element_blank(),
			# 			axis.text.y = element_blank()) +
			# 		scale_fill_manual("Type", values = my_pal_hex)
					
			# what abuot stacked..
			df %>%
				mutate(ResidenceREGCName = gsub(" Region", "", ResidenceREGCName)) %>%
				group_by(ResidenceREGCName, CommuteType) %>%
				summarise(Sum = sum(Count)) %>%
				mutate(Proportion = Sum / sum(Sum) * 100) %>%
					ggplot() +
						geom_col(aes(x = ResidenceREGCName, y = Proportion, fill = CommuteType)) +
						#scale_x_discrete(limits = sort(unique())) +
						coord_flip() +
						ggtitle("Commute-type distribution by region") +
						theme(
							plot.title = element_text(hjust = 0.5),
							panel.grid.major = element_blank(),
							panel.grid.minor = element_blank(),
							panel.background = element_blank(),
							axis.title.y = element_blank(),
							axis.ticks.y = element_blank()) +
						scale_fill_manual("Type", values = my_pal_hex)

		# otherwise, show regional data
		} else {

			df %>%
				filter(ResidenceREGCName == click$id) %>%
					group_by(CommuteType) %>%
					summarise(Sum = sum(Count)) %>%
					mutate(Proportion = Sum / sum(Sum) * 100) %>%
					arrange(desc(Proportion)) %>%
					ggplot(aes(x = CommuteType, y = Proportion)) +
						geom_col() +
						scale_x_discrete(limits = rev(national_comparison$CommuteType)) +
						coord_flip()

		}
		
	})

	# add layers
	observeEvent(input$load_data, {
		js$addLabels()
		shinyjs::disable('load_data')
		withProgress(message = 'Loading data...', value = 0, {
			n <- length(line_matrices) + length(polygon_layers)

			map <- leafletProxy("map")
			
			for (layer in names(polygon_layers)) {

				polygon_names <- as.character(polygon_layers[[layer]]@data$REGC2018_1)

				incProgress(1/n, paste0("Loading ", layer, " boundaries"))
				
				map <- addPolygons(map,
					data = polygon_layers[[layer]],
					group = layer,
					layerId = polygon_names,
					opacity = 0.2,
					fillOpacity = 0.2,
					highlight = highlightOptions(
						weight = 5,
						#color = "#666",
						#dashArray = "",
						fillOpacity = 0.7,
						bringToFront = TRUE))
			}
			
			for (commute_lines in names(line_matrices)) {
			
				this_name <- gsub('LineMatrix', '', commute_lines)
				incProgress(1/n, paste0("Loading ", tolower(this_name)))

				map <- addPolylines(map,
					data = line_matrices[[commute_lines]],
					group = this_name,
					color = my_pal_hex[[commute_lines]],
					weight = get_line_weights(this_name))
			}
		})

		return (map)

	})

	# render plot
	output$commute_props_by_region <- renderPlot({
		commute_proportions_by_region <- df %>%
			filter(ResidenceREGCName != 'Area Outside Region') %>%
			group_by(ResidenceREGCName, CommuteType) %>%
			summarise(Count = n()) %>%
			ungroup() %>%
			group_by(ResidenceREGCName) %>%
			mutate(Total = sum(Count)) %>%
			mutate(Percent = Count / Total * 100)

		commute_proportions_by_region %>%
			filter(CommuteType %in% input$commute_props_by_region_selector) %>%
			ggplot(aes(x = ResidenceREGCName, y = Percent)) +
				geom_col()
	})
	
}

shinyApp(ui, server)

#runApp('C:/Users/hls/code/shiny-server/commuter')


