library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(shinycssloaders)
library(shinyjs)



# function to get line weights. takes a commute_type argument that is used to
# match with the commute_type_keys variable to get the correct dataframe in the
# commute_type_list list of dfs containing the weights. weights are not matched
# explicitly, they are simply matched by order. that is, it's assumed that the
# matrix passed to addPolylines has the line data in the same order as that
# passed in as weights.

get_line_weights <- function(commute_type) {
	return (line_weight_list[[grep(
		commute_type, commute_type_keys$CommuteType, ignore.case = TRUE)]]$Weight)
}


df <- read.csv(paste0(getwd(), '/data/cleaned-commuter-data.csv'))

# split weights into list of dfs so we can get appropriate vals
line_weight_list <- df %>% select(CommuteType, Weight) %>% group_by(CommuteType) %>% group_split()

# get group keys
commute_type_keys <- df %>% group_by(CommuteType) %>% group_keys()

# commuter line data are stored in data/line-matrices/*.txt
# read these into a list of matrices
line_matrices <- list()
for (file in dir("./data/line-matrices")) {
	line_matrices[[sub('.txt', '', file)]] <- as.matrix(
		read.table(paste0(getwd(), '/data/line-matrices/', file)))
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
			addLayersControl(
				overlayGroups = commute_groups,
				options = layersControlOptions(collapsed = FALSE)) %>%

			# hide most layers on startup
			hideGroup(commute_groups)
    
	})

	# add layers
	observeEvent(input$load_data, {
		shinyjs::disable('load_data')
		withProgress(message = 'Loading data...', value = 0, {
			n <- length(line_matrices)

			map <- leafletProxy("map")

			for (commute_lines in names(line_matrices)) {
			
				this_name <- gsub('LineMatrix', '', commute_lines)
				incProgress(1/n, paste0("Loading ", tolower(this_name)))

				map <- addPolylines(map,
					data = line_matrices[[commute_lines]],
					group = this_name,
					color = my_pal_hex[[commute_lines]],
					weight = get_line_weights(this_name)
					)
			
			}
			return (map)
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

