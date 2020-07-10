library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(rgdal)
library(shinyjs) # for disable of load_data button

# check whether this is local dev or production, set directories appropriately
base_dir <- ifelse(Sys.getenv("SHINY_DEV") == "True",
	"C:/Users/hls/code/shiny-server/commuter/",
	"")

# commuter line data are stored in data/line-matrices/*.txt
# read these into a list of matrices
line_matrices <- list()
for (file in dir("data/line-matrices")) {
	line_matrices[[sub('.txt', '', file)]] <- as.matrix(
		read.table(paste0(base_dir, 'data/line-matrices/', file)))
}

# commute line weights are stored in data/line-weights/*.csv
# read these into a list of dfs
line_weights <- list()
for (file in dir("data/line-weights")) {
	line_weights[[sub('.csv', '', file)]] <- read.csv(
		paste0(getwd(), '/data/line-weights/', file)
	)
}

# regional council (REGC) polygons are stored in shapefiles, read in here
polygon_layers <- list()

polygon_layers[['Regional']] <- readOGR(
	dsn = paste0(base_dir, "data/regc-layer"),
	layer = "regional-council-2018-generalised")

# remove " Region" from names
polygon_layers$Regional$REGC2018_1 <- gsub(" Region", "", polygon_layers$Regional$REGC2018_1)

regional_coordinates <- read.csv(paste0(base_dir, 'data/regional-coordinates.csv'))

commute_type_proportions <- read.csv(paste0(base_dir, 'data/commute-type-proportions.csv'), stringsAsFactors = FALSE)

# colors
#my_pal_hex <- brewer.pal(length(line_matrices), "Paired")
my_pal_hex <- c(
	'#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99',
	'#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A')
names(my_pal_hex) <- names(line_matrices)
my_pal <- colorFactor(my_pal_hex, domain = names(my_pal_hex))

# function to create stacked barplots
stacked_plot <- function(dat) {

	dat %>%
		ggplot(aes(x = ResidenceREGCName, y = Proportion, fill = CommuteType)) +
			geom_col() +
			scale_fill_manual("Type", values = my_pal_hex) +
			coord_flip() +
			theme(
				plot.title = element_text(hjust = 0.5),
					panel.grid.major = element_blank(),
					panel.grid.minor = element_blank(),
					panel.background = element_blank(),
					axis.title.y = element_blank(),
					axis.text.x = element_blank(),
					axis.ticks.y = element_blank(),
					legend.position = 'none'
			)
}

ui <- {
	tagList(
		useShinyjs(),
		includeCSS(paste0(base_dir, 'style.css')),
		fluidPage(
			fluidRow(
				div(class="col-sm-8",
					tags$form(class='well',
						leafletOutput('map', height = '90vh'))
				),
				div(class="col-sm-4",
					tags$form(class="well",
						tabsetPanel(id = "plots",
							tabPanel(title = "Compare regions",
								value = "compare_regions",
								br(),
								div(align = "left", class = "multicol",
									checkboxGroupInput("region_selector",
										label = NULL,
										choices = sort(unique(commute_type_proportions$ResidenceREGCName)),
										selected = c("Wellington", "Canterbury", "Auckland"),
										inline = FALSE)
								),
								plotOutput("compare_regions")
							),
							tabPanel(title = "Single region", 
								value = "single_region",
								br(),
								p("Make sure that the regional boundaries layer is selected, then click on a region 
								to compare it with the national average."),
								plotOutput("single_region")
							),
							tabPanel(title = "About",
								br(),
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
								)
							)
						)	
					)
				)
			)
		)
	)
}


server <- function(input, output, session) {

	# define modal
	start_modal <- modalDialog(
		title = "Aotearoa Commuter Visualiser",
		p("Use this tool to explore how Kiwis get to work, based on Census 2018 data."),
		h4('How to use the map'),
		p("Use the layer selector button in the top-right corner of the map to load and display different layers."),
		p("The layers correspond to different modes of transport used by commuters on the Census day. These are 
			represented by lines on the map that start at the respondents' neighbourhoods and end at their places of work.
			Thicker lines mean more people used that mode of transport for that particular journey."),
		p("You can explore the data further by checking out the graphs on the right."),
		h4('Where do these data come from?'),
		div(style="padding-bottom: 10px;",
			p(style="display:inline", "This tool uses the "),
			a(href="https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/",
				"Statistics New Zealand 2018 Census Commuter View dataset"),
			p(style="display:inline", ". For more information about where the data came from and how it's used in this map,
			refer to the 'About' page.")
		),
		size = "l",
		easyClose = FALSE,
		fade = TRUE,
		footer = tagList(
			tags$a(class="btn btn-default", href="https://apps.hedleystirrat.co.nz", "Cancel"),
			modalButton("OK")
        )
	)

	# show modal (new modal for each new connection/session)
	showModal(start_modal)

	# the strategy is to just load the base map on instantiation. layers can be
	# added by proxy, so that server load is not front-loaded.
  	output$map <- renderLeaflet({ 

		map <- leaflet(options = leafletOptions(minZoom = 4)) %>%
			addTiles() %>%
    		setView(174,-41.2,6) %>%
			# addLegend(
			# 	position = "bottomleft",
        	# 	pal = my_pal,
			# 	values = names(my_pal_hex)) %>%
			addLayersControl(
				baseGroups = c('None', names(polygon_layers)),
				overlayGroups = names(my_pal_hex),
				options = layersControlOptions(collapsed = TRUE)) %>%
			hideGroup(names(my_pal_hex)) %>%
			htmlwidgets::onRender("
				function() {
					$('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center;font-weight:700;\">Select commuter types</label>');
					$('.leaflet-control-layers-base').prepend('<label style=\"text-align:center;font-weight:700;\">Select area boundaries</label>');
				}
			")
	})

	# loaded_layers is a reactiveVal that starts off empty (no layers loaded),
	# and is appended to when a new layer is loaded.
	loaded_layers <- reactiveVal("")

	observeEvent(input$map_groups, {
		# get all selected layers (have to remove "None" from polygon baseGroups)
		selected_layers <- input$map_groups
		selected_layers <- selected_layers[selected_layers!="None"]

		# check whether any selected layers haven't been loaded
		added_layer <- selected_layers[!(unlist(selected_layers) %in% loaded_layers())]
		
		# if there's a selected but unloaded layer, load it
		if (length(added_layer) > 0) {

			withProgress(message = 'Loading data...', value = 0, {

				n <- 3

				incProgress(1/n, paste0("Loading ", tolower(added_layer)))

				# polygons and lines need to be loaded differently
				if (added_layer %in% names(polygon_layers)) {

					polygon_names <- as.character(polygon_layers[[added_layer]]@data$REGC2018_1)
				
					# add polygons to map
					map <- leafletProxy("map") %>%
							addPolygons(data = polygon_layers[[added_layer]],
							group = added_layer,
							layerId = polygon_names,
							opacity = 0.2,
							fillOpacity = 0,
							highlight = highlightOptions(
								weight = 5,
								fillOpacity = 0.05,
								bringToFront = TRUE))
					
					incProgress(1/n, paste0("Loading ", tolower(added_layer)))

					showNotification("Regional boundaries loaded", type = "message", duration = 10)
					
				} else {

					# add layer to map
					map <- leafletProxy("map") %>%
						addPolylines(data = line_matrices[[added_layer]],
							group = added_layer,
							color = my_pal_hex[[added_layer]],
							weight = line_weights[[added_layer]]$Weight) %>%
						clearControls() %>%
						addLegend(
							position = "bottomleft",
							pal = my_pal,
							values = selected_layers)

					incProgress(1/n, paste0("Loading ", tolower(added_layer)))

					showNotification(paste0(added_layer, " lines loaded"), type = "message", duration = 10)
				}

				# append to loaded_layers          
				loaded_layers(c(loaded_layers(), added_layer))

				incProgress(1/n, paste0("Loading ", tolower(added_layer)))
			})
        }
	})

	# add layers
	observeEvent(input$load_data, {

		shinyjs::disable('load_data')

		map <- leafletProxy("map")
		
		for (layer in names(polygon_layers)) {

			polygon_names <- as.character(polygon_layers[[layer]]@data$REGC2018_1)

			# append to loaded_layers          
			loaded_layers(c(loaded_layers(), layer))
			
			# add polygons to map
			map <- addPolygons(map,
				data = polygon_layers[[layer]],
				group = layer,
				layerId = polygon_names,
				opacity = 0.2,
				fillOpacity = 0,
				highlight = highlightOptions(
					weight = 5,
					fillOpacity = 0.05,
					bringToFront = TRUE))
		}

		showNotification("Regional boundaries loaded", type = "message", duration = 10)

		return (map)
	})

	# zoom to region on click, and also update tabsetPanel to single-region plot
	# inspiration from SymbolixAU here:
	# https://stackoverflow.com/questions/42771474/r-shiny-leaflet-click-on-shape-and-zoom-to-bounds-using-maps-package
	observeEvent(input$map_shape_click, {

		click <- input$map_shape_click

		# get lng/lat/zoom data for this region with click$id
		this_region <- regional_coordinates[match(click$id, regional_coordinates$id),]
		
		# open single_region comparison tabset
		updateTabsetPanel(session, "plots", selected = "single_region")
			
		# update map with lng/lat/zoom for this region
        leafletProxy("map") %>% 
            setView(
				lng = this_region$lng,
				lat = this_region$lat,
				zoom = 8)
	})

	# render single-region plot
	output$single_region <- renderPlot({

		# get clicked region
		click <- input$map_shape_click
		
		# if user hasn't clicked on a region, then just show national averages
		if (is.null(click)) {
			commute_type_proportions %>%
				filter(ResidenceREGCName == "All of New Zealand") %>%
				stacked_plot()

		# otherwise, show regional data comparison with national
		} else {
			commute_type_proportions %>%
				filter(ResidenceREGCName %in% c("All of New Zealand", click$id)) %>%
				stacked_plot()
		}
	})

	# render multiple-region comparison plot
	output$compare_regions <- renderPlot({

		# if nothing is selected, just show national average
		if (is.null(input$region_selector)) {
			commute_type_proportions %>%
				filter(ResidenceREGCName == "All of New Zealand") %>%
				stacked_plot()
		} else {
			commute_type_proportions %>%
				filter(ResidenceREGCName %in% input$region_selector) %>%
				stacked_plot()	
		}
	})
}

shinyApp(ui, server)




