## issue tracker:

## useful links
# http://rstudio.github.io/DT/server.html
# http://shiny.rstudio.com/articles/progress.html

## import libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(stringr)

## import functions
source('./functions/aggregate.R')
source('./functions/helper.R')


## file size options
# by default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10GB.
options(shiny.maxRequestSize = 10000*1024^2)

shinyServer(function(input, output, session) {
  ## dataset reactive variables
  source('./reactives/dataset.R', local=TRUE)
  
  ## variable reactives
  ## original variables
  origVars <- reactive({
    dataset <- rawDataset(); if (is.null(dataset)) {return(NULL)}
    colnames(dataset)
  })
  
  ## original factor variables
  origFactorVars <- reactive({
    dataset <- rawDataset(); if (is.null(dataset)) {return(NULL)}
    getFactorVarNames(dataset)
  })
  
  ## original numeric variables
  origNumericVars <- reactive({
    dataset <- rawDataset(); if (is.null(dataset)) {return(NULL)}
    getNumericVarNames(dataset)
  })
  
  ## processed dataset factor variables
  factorVars <- reactive({
    dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    getFactorVarNames(dataset)
  })
  
  ## processed dataset numeric variables
  numericVars <- reactive({
    dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    getNumericVarNames(dataset)
  })
  
  ## plot-related reactives
  source('./reactives/plot.R', local=TRUE)
  
  ## UI controls
  source('./uiControls/fileUIControls.R', local=TRUE)
  source('./uiControls/aggUIControls.R', local=TRUE)
  source('./uiControls/plotUIControls.R', local=TRUE)
  
  ## download handlers
  source('./reactives/download.R', local=TRUE)
  
  ## construct server-side datatable (buggy; suspected cause: deprecated implementation for DT library)
#   datatable <- reactive({
#     dataset <- manAggDataset(); if(is.null(dataset)) {return(NULL)}
#     DT::datatable(dataset, 
#                   filter = 'bottom',
#                   server = TRUE,
#                   options = list(ajax = list(url = action()),
#                                  lengthChange = TRUE,
#                                  lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100', 'All'))))
#   })
  
  ## display tabular datatable content
  output$displayTable <- DT::renderDataTable({
    #datatable()
    DT::datatable(manAggDataset(), filter='bottom')
  })
  
  ## display plot
  output$plot <- renderPlot({
    print(plotInput())
  }, height=700)

})


