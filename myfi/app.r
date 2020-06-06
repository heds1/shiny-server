# figure out what your property will contribute to your FI number

# assumptions - it's paid off by the time you retire


library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

ui <- {
	fluidPage(title="FI/RE Calculator",
		includeCSS('bootstrap.css'),
		includeCSS('style.css'),
		tags$head(`title`='Test'),
		HTML('
			<nav class="navbar navbar-expand-lg navbar-light bg-light">
			<a class="navbar-brand" href="#">FI/RE Calculator</a>
			<button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
				<span class="navbar-toggler-icon"></span>
			</button>

			<div class="collapse navbar-collapse" id="navbarSupportedContent">
				<ul class="navbar-nav mr-auto">
				<li class="nav-item active">
					<a class="nav-link" href="#">Home <span class="sr-only">(current)</span></a>
				</li>
				<li class="nav-item">
					<a class="nav-link" href="#">About</a>
				</li>
			</div>
			</nav>
		'),

		sidebarLayout(
			sidebarPanel(
				h2(style="text-align:center;", 'Summary'),
				hr(),
				div(
					div(id='summary-numbers',
						p(id='summary-fonts', 'CURRENT NET WORTH: '),
						div(style='font-weight:600;', textOutput(outputId = 'current_nw'))
					)
				),
				div(
					div(id='summary-numbers',
					p(id='summary-fonts', 'FI NUMBER: '),
					div(style='font-weight:600;', textOutput(outputId = 'fi_number_sidebar'))
					)	
				),
				div(id='summary-numbers',
					p(id='summary-fonts', "PREDICTED RETIREMENT YEAR: "),
					div(style='font-weight:600', textOutput('projected_fi_year'))
				),
				br(),
				uiOutput('fi_progress_bar'),
				div(id='summary-fonts', style="text-align: center;",
					textOutput('fi_progress_text')
				)
				
				
				
			),
			mainPanel(
				tabsetPanel(type="pill",
					tabPanel(
						'Your FI numbers',
						p('Calculate your FI number here'),
						numericInput(
							inputId = 'annual_withdrawal',
							label = 'What is your target annual income from retirement accounts?',
							value = 50000
						),
						numericInput(
							inputId = 'swr',
							label = 'What is your target safe withdrawal rate?',
							value = 0.035
						),
						numericInput(
							inputId = 'current_nw',
							label = 'Current net worth',
							value = 40000
						),
						numericInput(
							inputId = 'num_properties',
							label = 'Current number of properties',
							value = 0
						),
						uiOutput(
							'properties'
						)
					),
					tabPanel(
						'Projections',
						numericInput(
							inputId = 'yearly_savings',
							label = 'How much will you save per year for retirement?',
							value = 20000
						),
						numericInput(
							inputId = 'interest_rate',
							label = 'What is the expected % return on your investments?',
							value = 5
						),
						textOutput(
							'years_to_retirement_text'
						),
						plotOutput(
							'years_to_retirement_plot'
						)
					),
					tabPanel(
						'Goals'
					)
				)
			)
		)	
	)
}

# function to calculate number of years until retirement
cmpd_interest_until_target <- function(rate, initial, target, injection) {
	dat <- data.frame(year = 0, principal = initial)
	while(dat[nrow(dat), 'principal'] < target) {
		this_year <- dat[nrow(dat), 'year'] + 1
		this_principal <- dat[nrow(dat), 'principal'] + rate * dat[nrow(dat), 'principal'] + injection
		dat <- bind_rows(dat, data.frame(year = this_year, principal = this_principal))
	}
	return (dat)
}

server <- function(input, output, session) {

	# calculate FI number
	fi_number <- reactive({
		this_num <- round(input$annual_withdrawal / input$swr, 0)
		digits <- nchar(this_num)/2
		signif(this_num, digits = digits)
	})

	# render FI number in sidebar
	output$fi_number_sidebar <- renderText({
		paste0("$", fi_number())
	})

	# convert interest rate to percentage if required
	# naive, just assumes that if a rate is above 20% they mean the % rather
	# than actual value
	cleaned_interest_rate <- reactive({
		if (input$interest_rate > .2) {
			return (input$interest_rate / 100)
		} else {
			return (input$interest_rate)
		}
	})

	# calculate years to retirement, return list containing years and plot
	years_to_retirement <- reactive({
		df <- cmpd_interest_until_target(rate = cleaned_interest_rate(), initial = input$current_nw, target = fi_number(), injection = input$yearly_savings)
		years <- df[nrow(df), 'year']
		plt <- ggplot(df, aes(x = year, y = principal)) + geom_point()
		return(list(years=years, plot = plt))
	})

	# set predicted retirement year
	predicted_retirement_year <- reactive({
		year(Sys.time()) + years_to_retirement()$year
	})

	# output text of years to financial independence
	output$fi_progress_summary <- renderText({
		paste0("With the above settings, you will be financially independent in the year ", predicted_retirement_year())
	}) 

	# output text of years to retirement for summary
	output$projected_fi_year <- renderText({
		predicted_retirement_year()
	})

	# output plot of years to retirement
	output$years_to_retirement_plot <- renderPlot({
		years_to_retirement()$plot
	})

	# render current net worth
	output$current_nw <- renderText({
		paste0(" $", input$current_nw)
	})

	# reactive percent FI
	percent_fi <- reactive({
		round(input$current_nw / fi_number() * 100, 1)
	})

	# render FI progress text
	output$fi_progress_text <- renderText({
		paste0("YOU ARE ", percent_fi(), "% FINANCIALLY INDEPENDENT")
	})

	# render bootstrap progress bar
	output$fi_progress_bar <- renderUI({

		# generate HTML for progress bar
		HTML(
			paste0(
				"<div class=\"progress\" style=\"height: 30px;\">
					<div class=\"progress-bar\" role=\"progressbar\" style=\"width: ", percent_fi(), "%\" aria-valuenow=\"", percent_fi(), "\" aria-valuemax=\"100\" 
				 </div>"
			)
		)
	})

	output$properties <- renderUI({
		num_properties <- input$num_properties

		if (num_properties > 0) {
			
			lapply(1:num_properties, function(i) {
				numericInput(
					inputId = paste0("ind", i),
					label = paste0("Value of property ", i),
					value = 0,
					step = 20000
				)
			}) 
		}

		
	})

}

# shinyApp(ui = htmlTemplate("c:/users/hls/code/shiny-server/myfi/www/index.html"), server)
shinyApp(ui, server)
