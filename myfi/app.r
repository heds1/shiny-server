# figure out what your property will contribute to your FI number

# assumptions - it's paid off by the time you retire


library(shiny)
library(dplyr)

ui <- {
	fluidPage(

		sidebarLayout(
			sidebarPanel(
				h2('Summary'),
				div(
					p(
						'Current net worth: '
					),
					textOutput(
						outputId = 'current_nw'
					)
				),
				div(
					p(
						'FI number: '
					),
					textOutput(
						outputId = 'fi_num'
					)
				),
				div(
					p(
						'% progress toward financial independence'
					),
					uiOutput(
						'fi_progress'
					)
					
				)
				
			),
			mainPanel(
			tabsetPanel(
				tabPanel(
					'Your numbers',
						numericInput(
							inputId = 'current_nw',
							label = 'Current net worth',
							value = 0
						),
						numericInput(
							inputId = 'fi_num',
							label = 'FI number',
							value = 1000000
						)
				),
				tabPanel(
					'Projections',
					p('test')
				),
				tabPanel(
					'Plots'
				)
			)
		)

		)

		
		
	)
}

server <- function(input, output, session) {

	output$current_nw <- reactive({
		input$current_nw
	})

	output$fi_num <- reactive({
		input$fi_num
	})

	output$fi_progress <- renderUI({

		# get percent FI based on inputs
		percent_fi <- input$current_nw / input$fi_num * 100

		# generate HTML for progress bar
		HTML(
			paste0(
				"
					<div class=\"progress\">
						<div class=\"progress-bar\" role=\"progressbar\" style=\"width: ", percent_fi, "%\" aria-valuenow=\"", percent_fi, "\" aria-valuemax=\"100\" 
					</div>
					
				"
				
			)
			# '
			# <div class="progress">
			# 	<div class="progress-bar" role="progressbar" style="width: 25%" aria-valuenow="25" aria-valuemin="0" aria-valuemax="100"></div>
			# </div>
			# '
		)
		# todo add

	})

}

shinyApp(ui, server)



compound_interest <- function(rate, years, initial, injection) {
	# rate = interest rate as decimal (p.a.)
	# years = number of years to compound
	# initial = initial investment
	# injection = extra savings each year
	total <- initial

	for (year in 1:years) {

		total <- total + rate * total + injection
	
	}

	return (total)

}


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
