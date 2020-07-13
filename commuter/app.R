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
stacked_plot <- function(dat, bar_order) {

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
					axis.ticks.y = element_blank()) +
			scale_x_discrete(limits = bar_order)
			# scale_x_discrete(limits = rev(unique(commute_type_proportions$ResidenceREGCName)))
}

# function to get dataframe of ResidenceREGCName values in order of decreasing
# proportion of a given commute type
get_REGC_order <- function(commute_type = NULL) {

	if (is.null(commute_type)) {
		commute_type <- unique(commute_type_proportions$CommuteType)
	}

	ordered_df <- commute_type_proportions %>%
		filter(CommuteType %in% commute_type) %>%
		filter(ResidenceREGCName != "All of New Zealand") %>%
		arrange(desc(Proportion)) %>%
		select(Proportion, ResidenceREGCName)

	return (ordered_df)
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
						style="overflow:auto",
						tabsetPanel(id = "plots",
							tabPanel(title = "Compare regions",
								value = "compare_regions",
								br(),
								p("This graph compares the distribution of commute types across regions. Select a
									commute type in the dropdown menu below to see which regions commute by that method the most!"),
								selectInput("commute_type_sorter",
									label = NULL,
									choices = sort(unique(commute_type_proportions$CommuteType)),
									selected = NULL),
								textOutput("commute_type_ranking"),
								br(),
								plotOutput("compare_regions")
							),
							tabPanel(title = "Single region", 
								value = "single_region",
								br(),
								p("Make sure that the regional boundaries layer is selected, then click on a region 
								to compare it with the national average."),
								br(),
								plotOutput("single_region")
							),
							tabPanel(title = "About",
								h3("About the app"),
								div(style="padding-bottom: 10px;",
									p(style="display:inline", "This tool is an R Shiny web-app that uses the open-source Leaflet library to render the interactive map. It uses the "),
									a(href="https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/",
										"Statistics New Zealand 2018 Census Commuter View dataset"),
									p(style="display:inline", "to map the journeys of respondents from their place of residence to their place of work.")
								),
								div(style="padding-bottom: 10px;",
									p(style="display:inline", "The regional council boundaries were obtained from the "),
									a(href="https://datafinder.stats.govt.nz/layer/95065-statistical-area-2-higher-geographies-2018-generalised/",
										"Statistical Area 2 Higher Geographies 2018 dataset."),
									p(style="display:inline", "The location data were mapped to the regional boundaries by their assignment 
										into Statistical Area 2 (SA2) zones, which are intended to group the population into defined areas that
										interact together socioeconomically.")
								),
								div(
									p(style="display:inline", "The data described above was further processed for use within this tool. The code used to reproduce these
										data processing steps, as well as the full code for the application, is open source and 
										available on this project's "),
									a(href="https://github.com/heds1/shiny-server/tree/master/commuter", "Github repo.")
								),
								h3("About the author"),
								div(style="display:inline",
									a(href="https://hedleystirrat.co.nz/", "Hedley"),
									p(style="display:inline", " enjoys data science, web development, and long, romantic walks on the beach with his dog."),
									p(style="display:inline", "He wrote this app primarily to avoid weeding the garden.
									Feature requests, bug reports, ideas, questions or observations about the weather are all very welcome,
									so if you'd like to get in touch, please use  "),
									a(href="https://www.hedleystirrat.co.nz/about/", "this form."),
									p(style="display:inline", "Thanks!")
								),
								hr(),
								div(img(id="my_img", src = 'img.jpg', width = '400px')),
								hr()
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
		p("You can use this tool to explore the different ways that Kiwis get to work... and the lengths that some of us go to!"),
		h4('How to use the map'),
		p("The layer-selector button in the top-right corner of the map can be used to load and display the map's layers."),
		p("The layers correspond to different modes of transport used by commuters on census day 2018. These are 
			represented by lines on the map that start at the respondents' neighbourhoods and end at their places of work.
			Thicker lines mean more people used that mode of transport for that particular journey. (What's the longest journey that you can find?)"),
		p("You can explore the data further by checking out the graphs on the right."),
		h4('Where do these data come from?'),
		div(style="padding-bottom: 10px;",
			p(style="display:inline", "This tool uses the "),
			a(href="https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/",
				"Statistics New Zealand 2018 Census Commuter View dataset."),
			p(style="display:inline", "For more information about this dataset and how it's used in this map"),
			HTML("&mdash;"),
			p(style="display:inline", "as well as the code to reproduce this map yourself!"),
			HTML("&mdash;"),
			p(style="display:inline", "check out the About tab about the graphs on the right.")
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
			addProviderTiles(providers$CartoDB.Positron) %>%
    		setView(174,-41.2,6) %>%
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
							weight = line_weights[[added_layer]]$Weight)

					incProgress(1/n, paste0("Loading ", tolower(added_layer)))

					showNotification(paste0(added_layer, " lines loaded"), type = "message", duration = 10)
				}

				# append to loaded_layers          
				loaded_layers(c(loaded_layers(), added_layer))

				incProgress(1/n, paste0("Loading ", tolower(added_layer)))
			})
        }

		# update legend
		leafletProxy("map") %>%
			clearControls() %>%
			addLegend(
				position = "bottomleft",
				pal = my_pal,
				values = selected_layers[selected_layers != "Regional"])
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

		REGC_order <- get_REGC_order(input$commute_type_sorter)

			commute_type_proportions %>%
				filter(ResidenceREGCName != "All of New Zealand") %>%
				stacked_plot(bar_order = rev(REGC_order$ResidenceREGCName))

	})

	# render text of commute type rankings
	output$commute_type_ranking <- renderText({
		REGC_order <- get_REGC_order(input$commute_type_sorter)

		paste0("The ", REGC_order$ResidenceREGCName[1], " region (",
			round(REGC_order$Proportion[1], 1),
			"%) has the highest proportion of people commuting by ",
			tolower(input$commute_type_sorter), ", while the ",
			REGC_order$ResidenceREGCName[nrow(REGC_order)], " region (",
			round(REGC_order$Proportion[nrow(REGC_order)],1),
			"%) has the lowest."
		)
	})

	# render photo
	# output$my_photo <- renderImage(paste0(base_dir, "img.png"))
}

shinyApp(ui, server)




