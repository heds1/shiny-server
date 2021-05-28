library(leaflet)
library(shiny)

ui <- {
    fluidPage(
        leaflet(quakes) %>%
            addTiles(group = "OpenStreetMap") %>%
            addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
            addCircles(~long, ~lat, group = "Show quakes") %>%
            addLayersControl(
                baseGroups = c("OpenStreetMap", "Positron"),
                overlayGroups = "Show quakes",
                options = layersControlOptions(collapsed = FALSE)) %>%
            htmlwidgets::onRender("
                function() {
                    $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Overlay options</label>');
                    $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center\">Base layer options</label>');
                }
            ")
    )
}

server <- function(input, output, session) {
    
}

shinyApp(ui, server)
