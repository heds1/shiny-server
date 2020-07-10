library(shiny)
library(leaflet)

ui <- {
    fluidPage(
        fluidRow(
            leafletOutput('map'),
            textOutput('tracker'),
            textOutput('loaded_layers_')
        )
    )
}

server <- function(input, output, session) {

    output$map <- renderLeaflet({
        map <- leaflet() %>%
            addTiles() %>%
            addLayersControl(
                overlayGroups = c("One","Two","Three")) %>%
                hideGroup(c("One","Two","Three"))
    })

    output$tracker <- renderText({
        input$map_groups
    })

    # # # observer to track when addPolylines calls need to be done
    # observe({
    #     selected_groups <- req(input$map_groups)
      

    # })

    # current_lines <- eventReactive(input$map_groups, {
    #     paste0(input$map_groups)
    # })

    # output$cur <- renderText(current_lines)


    # need to look at selected groups and load data if the group is not loaded


    # output$selected_groups <- renderText({
    #     my_grps
    # })

    # loaded_layers starts off empty. appended to when a new layer is added.
    loaded_layers <- reactiveVal("empty")

    output$loaded_layers_ <- renderText({
        paste0("Loaded: ", loaded_layers())
    })

    observeEvent(input$map_groups, {

        selected_layers <- input$map_groups
        # loaded_layers <- c("One","Three")
        # selected_layers <- c("One","Two")

        # get unloaded layers if there are any
        layer_to_add <- selected_layers[!(unlist(selected_layers) %in% loaded_layers())]
 
        if (length(layer_to_add) > 0) {

            # # append to loaded_layers
            # loaded_layers <- c(loaded_layers, layer_to_add)
            
            loaded_layers(c(loaded_layers(), layer_to_add))

            # add layer to map
            map <- leafletProxy("map") %>%
                addCircleMarkers(lng = 175, lat = -40)

        }

    })


}

shinyApp(ui,server)