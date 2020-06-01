# this is just a silly Shiny app made to take up several seconds of processing
# time, to do some testing on Shiny Server and its single R process

library(shiny)
library(caret)

ridiculous_modelling <- function() {

    # train models
    mod_rf <- train(mpg ~ ., method = "rf", data = mtcars)
    mod_rpart <- train(mpg ~ ., method = "rpart", data = mtcars)
    mod_lm <- train(mpg ~ ., method = "lm", data = mtcars)
    mod_glm <- train(mpg ~ ., method = "glm", data = mtcars)
    mod_tb <- train(mpg ~ ., method = "treebag", data = mtcars)

    # combine predictions
    stacked_predictions <- data.frame(
        rf = predict(mod_rf, mtcars),
        rpart = predict(mod_rpart, mtcars),
        lm = predict(mod_lm, mtcars),
        glm = predict(mod_glm, mtcars),
        tb = predict(mod_tb, mtcars),
        mpg = mtcars$mpg)

    # train stacked model
    mod_stacked <- train(mpg ~ ., method = "rf", data = stacked_predictions)

    return (mod_stacked)
}

ui <- {
    fluidPage(
        fluidRow(
            actionButton(
                inputId = 'run_models',
                label = 'Run models')),
        fluidRow(
            dataTableOutput(
                outputId = 'model_output')),
        fluidRow(
            textOutput(
                outputId = 'time_taken'))
    )
}

server <- function(input, output, session) {

    run_model <- eventReactive(input$run_models, {
        start_time <- Sys.time()
        results <- ridiculous_modelling()
        end_time <- Sys.time()

        return(list(
            results = results$results,
            start_time = start_time,
            end_time = end_time))
    })

    output$model_output <- renderDataTable({
        run_model()$results
    })

    output$time_taken <- renderText({
        paste0("Time taken: ", run_model()$end_time - run_model()$start_time, "s")
    })
}

shinyApp(ui, server)