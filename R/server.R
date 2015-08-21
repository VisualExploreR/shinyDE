## import libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(stringr)
library(shinyBS)
library(shinyjs)


## import functions
source('./functions/helper.R')
source('./functions/plot.R')
source('./functions/aggregate.R')


## file size options
# by default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10GB.
options(shiny.maxRequestSize = 10000*1024^2)


shinyServer(function(input, output, session) {
  ## reactive variables
  source('./reactives/reactives.R', local=TRUE)  # general/miscellaneous
  source('./reactives/dataset.R', local=TRUE)  # dataset variables
  source('./reactives/plot.R', local=TRUE)  # plot-related reactives
  
  ## UI controls
  source('./uiWidgets/generalWidgets.R', local=TRUE)
  source('./uiWidgets/fileWidgets.R', local=TRUE)
  source('./uiWidgets/manAggWidgets.R', local=TRUE)
  source('./uiWidgets/plotWidgets.R', local=TRUE)
  
  ## download handlers
  source('./reactives/download.R', local=TRUE)
  
  ## display plot and data table
  observeEvent(input$reactive, {
    shinyBS::updateButton(session, "submit", disabled = input$reactive==TRUE)

    if (input$reactive) {
      
      ## display plot reactively
      output$plot <- renderPlot({
        print(plotInput())
      }, height=700)
      
      ## display data table reactively
      output$displayTable <- DT::renderDataTable({
        DT::datatable(manAggDataset(), filter='bottom')
      })
      
    } else {
      
      ## display plot upon submit
      output$plot <- renderPlot({
        input$submit
        isolate(print(plotInput()))
      }, height=700)
      
      ## display data table upon submit
      output$displayTable <- DT::renderDataTable({
        input$submit
        isolate(DT::datatable(manAggDataset(), filter='bottom'))
      })
    }
  })
  
#   ## OPTION 1
#   observeEvent(input$facetMeth %in% c('grid', 'wrap'), {
#     if (is.null(input$facetMeth)) return()
#     if (input$facetMeth=='grid') {
#       shinyjs::enable('facetCol')
#       shinyjs::enable('facetRow')
#       shinyjs::disable('facetWrap')
#     } else if (input$facetMeth=='wrap') {
#       shinyjs::disable('facetCol')
#       shinyjs::disable('facetRow')
#       shinyjs::enable('facetWrap')
#     }
#   })
#   
#   ## OPTION 2
#   observeEvent(input$facetCol=='None' & input$facetRow=='None', {
#     if (is.null(input$facetWrap)) return()
#     if (input$facetWrap != 'None') {
#       shinyjs::disable('facetCol')
#       shinyjs::disable('facetRow')
#     }
#   })
  
  ## when everything is none
#   isInitFacetState <- reactive({
#     wgts <- c('facetCol', 'facetRow', 'facetWrap')
#     if (!checkWidgetsLoaded(input, wgts)) return()
#     
#   })

})
