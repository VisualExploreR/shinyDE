## import libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(stringr)


## import functions
source('./functions/plot.R')
source('./functions/aggregate.R')
source('./functions/helper.R')


## file size options
# by default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10GB.
options(shiny.maxRequestSize = 10000*1024^2)


shinyServer(function(input, output, session) {
  ## reactive variables
  source('./reactives/dataset.R', local=TRUE)  # dataset variables
  source('./reactives/varNames.R', local=TRUE)  # variable name variables
  source('./reactives/plot.R', local=TRUE)  # plot-related reactives
  
  ## UI controls
  source('./uiWidgets/fileWidgets.R', local=TRUE)
  source('./uiWidgets/aggWidgets.R', local=TRUE)
  source('./uiWidgets/plotWidgets.R', local=TRUE)
  
  ## download handlers
  source('./reactives/download.R', local=TRUE)
  
  ## display tabular datatable content
  output$displayTable <- DT::renderDataTable({
    DT::datatable(manAggDataset(), filter='bottom')
  })
  
  ## display plot
  output$plot <- renderPlot({
    print(plotInput())
  }, height=700)

})
