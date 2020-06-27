library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(shinycssloaders)
library(shinyjs)
library(rgdal)

polygon_layers <- list()

polygon_layers[['territorial']] <- readOGR(
	dsn = "C:/Users/hls/code/statsnz/ta",
    layer = "territorial-authority-2018-generalised")

polygon_layers[['regional']] <- readOGR(
	dsn = "C:/Users/hls/code/statsnz/regc",
	layer = "regional-council-2018-generalised")

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

# group names. todo make these nicer
commute_groups <- gsub('LineMatrix', '', names(line_matrices))

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



	js$addLabels()

	# add layers
	observeEvent(input$load_data, {
		js$addLabels()
		shinyjs::disable('load_data')
		withProgress(message = 'Loading data...', value = 0, {
			n <- length(line_matrices) + length(polygon_layers)

			map <- leafletProxy("map")
			
			for (layer in names(polygon_layers)) {

				incProgress(1/n, paste0("Loading ", layer, " boundaries"))
				
				map <- addPolygons(map,
					data = polygon_layers[[layer]],
					group = layer)

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

#runApp('C:/Users/hls/code/shiny-server/commuter')