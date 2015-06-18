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
source('aggregate.R')
source('helper.R')


## file size options
# by default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10GB.
options(shiny.maxRequestSize = 10000*1024^2)

shinyServer(function(input, output, session) {
  ## dataset reactive variables
  source('dataset.R', local=TRUE)
  
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
  
  ## x options reactive
  xOpts <- reactive({
    dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    xOpts <- names(dataset)
    #    if (!is.null(input$plotType)) 
    #      if (input$plotType=='line') 
    #        xOpts <- setdiff(xOpts, input$color)
    xOpts
  })
  
  ## y options reactive
  yOpts <- reactive({
    dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    yOpts <- names(dataset)
    #    if (!is.null(input$plotType)) 
    #      if (input$plotType=='line') 
    #        yOpts <- setdiff(yOpts, c(input$x, input$color))
    yOpts
  })
  
  ## color options reactive
  colOpts <- reactive({
    dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    colOpts <- c('None', factorVars())
    #     if (!is.null(input$plotType)) 
    #       if (input$plotType=='line') 
    #         colOpts <- setdiff(colOpts, input$x)
    colOpts
  })
  
  ## facet options reactive
  facetOpts <- reactive({
    facetOpts <- factorVars()
    #     if (!is.null(input$plotType)) 
    #       if (input$plotType=='line')
    #         facetOpts <- setdiff(facetOpts, c(input$x, input$y))
    facetOpts
  })
  
  ## plot reactive
  plotInput <- reactive({
    #dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    dataset <- finalDF(); if (is.null(dataset)) {return(NULL)}
    x <- input$x; y <- input$y
    facetRow <- input$facetRow; facetCol <- input$facetCol
    facetWrap <- input$facetWrap
    color <- input$color
    plotType <- input$plotType
    
    if (is.null(x) | is.null(y)) {return(NULL)}
    if (is.null(facetRow) | is.null(facetCol)) {return(NULL)}
    if (is.null(color) | is.null(plotType)) {return(NULL)}
    if (!(x %in% xOpts()) | !(y %in% yOpts())) {return(NULL)}
      
    ## modify y (measure variable) for semi-automatic aggregation dataset
    if (!(y %in% colnames(dataset))) {
      y <- colnames(dataset)[grepl(y, colnames(dataset))]
    }
    
    ## scatter plot
    if (plotType=='scatter') {
      p <- ggplot(dataset, aes_string(x=x, y=y)) + 
        geom_point()
    } 
    
    ## line plot
    else if (plotType=='line') {
      p <- ggplot(dataset, aes_string(x=x, y=y)) + 
        geom_point()
      
      if (x==color | color=='None') {
        p <- p + geom_line(aes(group=1))
      }
      else {
        p <- p + geom_line(aes_string(group=color))
      }
    }
    
    ## bar plot
    else if (plotType=='bar') {
      p <- ggplot(dataset, aes_string(x=x, y=y)) +
        geom_bar(stat='identity')
      
      if (color != 'None') {
        p <- p + aes_string(fill=color)
      }
    }
    
    ## plot colors
    if (color != 'None') {
      p <- p + aes_string(color=color)      
    }
    
    ## facet grids
    facetGrids <- paste(facetRow, '~', facetCol)
    if (facetGrids != '. ~ .')
      p <- p + facet_grid(facetGrids) 
    
    ## facet wrap
    facetWrap <- paste('~', facetWrap)
    
#     df <- mtcars
#     aggBy <- 'cyl'
#     aggTarget <- 'mpg'
#     aggMeth <- 'mean'
#     tuck <- aggregate(df, aggBy, aggTarget, aggMeth)

#     ggplot(tuck, aes(x=aggBy, y=mpg_mean)) + 
#       geom_point() +
#       geom_line(aes(group=1)) + 
#       facetWrap(~cyl)

# 
#     if (facetWrap != '~ .')
#       p <- p + facetWrap(facetWrap)
    
    ## return
    p
  })

  ## UI controls
  source('uiControls.R', local=TRUE)
  
  ## download handlers
  source('download.R', local=TRUE)
  
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


