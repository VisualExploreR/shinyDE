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

  ## for rectangular highlighting in plot
  observeEvent(input$submit, { 
    brush <- input$zoom_brush 
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  }) 
  
  ## 
#   observeEvent(input$facetMeth=='grid', {
#     if (is.null(input$facetMeth)) {
#       #shinyjs::disable('facetWrap')
#       #shinyjs::toggleState('facetWrap')
#       print('ha')
#     } else if (input$facetMeth=='grid') {
#       #shinyjs::disable('facetWrap')
#       shinyjs::toggleState('facetWrap')
#       print('ba')
#     } else if (input$facetMeth=='wrap') {
#       #shinyjs::disable('facetCol')
#       #shinyjs::disable('facetRow')
#       shinyjs::toggleState('facetCol')
#       shinyjs::toggleState('facetRow')
#       print('ka')
#     }
#   })
  
  
})
