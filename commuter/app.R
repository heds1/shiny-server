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
				div(class="col-sm-4",
					tags$form(class="well",
						h3('How Aotearoa Gets to Work', class="display-2", style="text-align: center;"),
						hr(),
						tabsetPanel(
							id = "hidden_tabs",
							#type = "hidden",
							tabPanel("home_panel",
								p("Use this tool to explore how Kiwis get to work, based on Census 2018 data."),
								p("The lines on the map represent the distance travelled by people using different
									modes of transport. Thicker lines mean more people used that mode of transport for that particular journey.
									You can explore the data further by checking out the graphs on the left, and toggle specific commute-type layers
									on and off with the button on the top-right corner of the map."),
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
							tabPanel("about_panel",
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
								plotOutput("compare_regions")),
							tabPanel(title = "Single region", 
								value = "single_region",
								br(),
								p("Make sure that the regional boundaries layer is selected, then click on a region 
								to compare it with the national average."),
								plotOutput("single_region"))
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
				position = "bottomleft",
        		pal = my_pal,
				values = names(my_pal_hex)) %>%
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

	# add layers
	observeEvent(input$load_data, {
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
					fillOpacity = 0.05,
					highlight = highlightOptions(
						weight = 5,
						fillOpacity = 0.2,
						bringToFront = TRUE))
			}
			
			for (i in names(line_matrices)) {

				incProgress(1/n, paste0("Loading ", tolower(i), " layer..."))

				map <- addPolylines(map,
					data = line_matrices[[i]],
					group = i,
					color = my_pal_hex[[i]],
					weight = line_weights[[i]]$Weight)
			}
		})

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




