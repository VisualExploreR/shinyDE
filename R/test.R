library(shiny)
library(shinyjs)
runApp(shinyApp(
  ui = fluidPage(
    useShinyjs(),
    selectInput("test", "Select once", letters),
    actionButton("submit", "Choose")
  ),
  server = function(input, output, session) {
    observeEvent(input$submit, {
      disable("test")
    })
  }
))