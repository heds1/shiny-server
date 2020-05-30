library(shiny)
library(ggplot2)

ui <- {
    fluidPage(
        fluidRow(
            actionButton(
                inputId = 'click_me',
                label = 'Click me')),
        fluidRow(
            plotOutput('constellation_plot'))
    )
}

server <- function(input, output, session) {

    constellation <- eventReactive(input$click_me, {
        
        # get random number of stars
        num_stars <- sample(1:500, 1)

        # create star df with x, y and magnitude
        df <- data.frame(
            x = seq(from = 1, to = num_stars),
            y = runif(num_stars),
            mag = sample(1:10, num_stars, replace = TRUE))

        # plot it
        ggplot(df, aes(x = x, y = y, size = mag)) +
            geom_point(colour = 'white') +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                legend.position = "none",
                panel.background = element_rect(fill = 'black'))
    })

    output$constellation_plot <- renderPlot({
        constellation()
    })

}

shinyApp(ui, server)