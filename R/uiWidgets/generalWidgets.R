## dataset drop-down options 
output$datasetCtrl <- renderUI({
  selectInput("dataset", "Choose a dataset:", 
              choices = rawDatasetNames())
})

