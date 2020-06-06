# figure out what your property will contribute to your FI number

# assumptions - it's paid off by the time you retire


library(shiny)
library(dplyr)
library(ggplot2)

ui <- {
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				h2('Summary'),
				div(style="display: inline-flex;",
					p(style="padding-right: 0.5rem;", 'Current net worth: '),
					textOutput(outputId = 'current_nw')
				),
				div(style="display: inline-flex;",
					p(style="padding-right: 0.5rem;", 'FI number: '),
					textOutput(outputId = 'fi_number_sidebar')
				),
				div(
					textOutput('fi_progress_text'),
					uiOutput('fi_progress_bar')	
				)
				
			),
			mainPanel(
				tabsetPanel(
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
					)
				)
			)
		)	
	)
}

# compound_interest <- function(rate, years, initial, injection) {
# 	# rate = interest rate as decimal (p.a.)
# 	# years = number of years to compound
# 	# initial = initial investment
# 	# injection = extra savings each year
# 	total <- initial

# 	for (year in 1:years) {

# 		total <- total + rate * total + injection
	
# 	}

# 	return (total)

# }

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

	# output text of years to retirement
	output$years_to_retirement_text <- renderText({
		paste0("With the above settings, you will be able to retire in ", years_to_retirement()$years, " years!")
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
		paste0("You are ", percent_fi(), "% financially independent.")
	})

	# render bootstrap progress bar
	output$fi_progress_bar <- renderUI({

		# generate HTML for progress bar
		HTML(
			paste0(
				"<div class=\"progress\">
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

shinyApp(ui, server)






# # assume
# #	end of 2021: age = 30, net worth = 40,000
# #	long-term interest returns after fees and tax and inflation = 5%
# #	injection of 40,000 savings each year

# compound_interest(0.06, 20, 40000, 40000)
# # [1] 1599709

# # this is close to 1,666,667 which is a target of $50,000 withdrawal per year
# # using SWR of 3%


# test <- function(target,)


# # current value of property if paid off
# rv <- 300000
# # income generated from property (gross)
# gross_income <- 15000

# # gross-to-net factor (% of gross that becomes net)
# gnet_factor <- .5

# net_income <- gnet_factor * gross_income

# # SWR
# swr <- 0.035

# # property's equivalent contribution to required equity position at retirement,
# # based on swr
# property_contrib <- net_income / swr



# test(target = 1700000)