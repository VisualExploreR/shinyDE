## issue tracker:

## useful links
# http://rstudio.github.io/DT/server.html
# http://shiny.rstudio.com/articles/progress.html

## import libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

## import functions
source('aggregate.R')
source('helper.R')

## file size options
# by default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10GB.
options(shiny.maxRequestSize = 10000*1024^2)

shinyServer(function(input, output, session) {
  
  # grab input dataset
  #     raw <- reactive({
  #       # input$file will be NULL initially. After the user selects
  #       # and uploads a file, it will be a data frame with 'name',
  #       # 'size', 'type', and 'datapath' columns. The 'datapath'
  #       # column will contain the local filenames where the data can
  #       # be found.
  #       
  #       fileInfo <- input$file
  #       if (is.null(fileInfo)) {return(NULL)}
  #       read.csv(fileInfo$datapath, header = as.logical(input$header),
  #                sep = input$sep, quote = input$quote)
  #     })
  
  ## raw dataset (COMMENT THIS PORTION OUT TO ALLOW DATASET UPLOAD)
  raw <- reactive({
    mtcars$cyl <- as.factor(mtcars$cyl)
    mtcars$am <- as.factor(mtcars$am)
    mtcars$gear <- as.factor(mtcars$gear)
    mtcars  
    # read.csv('./data/diamonds_big.csv', stringsAsFactors=TRUE, na.strings=c('', ' '))    
  })
  
  ## raw or aggregated dataset
  dataset <- reactive({
    ## raw dataset
    if (is.null(input$aggBy) | is.null(input$aggTarget) | is.null(input$aggMeth)) {
      dataset <- raw()
    }
    
    ## aggregated dataset
    else {
      dataset <- aggregate(raw(), input$aggBy, input$aggTarget, input$aggMeth)
    }
    
    ## return dataset
    dataset
  })
  
  ## final dataset
  finalDF <- reactive({
    dataset <- dataset()
    if (!is.null(input$shareOf) & !is.null(input$shareTarget))
      if (input$shareOf != 'None' & input$shareTarget != 'None')
        dataset <- calcShare(dataset, input$shareOf, input$shareTarget)
    dataset
  })
  
  ## original variables
  origVars <- reactive({
    dataset <- raw(); if (is.null(dataset)) {return(NULL)}
    colnames(dataset)
  })
  
  ## original factor variables
  origFactorVars <- reactive({
    dataset <- raw(); if (is.null(dataset)) {return(NULL)}
    getFactorVarNames(dataset)
  })
  
  ## original numeric variables
  origNumericVars <- reactive({
    dataset <- raw(); if (is.null(dataset)) {return(NULL)}
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
  
  xOpts <- reactive({
    dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    xOpts <- names(dataset)
    #    if (!is.null(input$plotType)) 
    #      if (input$plotType=='line') 
    #        xOpts <- setdiff(xOpts, input$color)
    xOpts
  })
  
  yOpts <- reactive({
    dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    yOpts <- names(dataset)
    #    if (!is.null(input$plotType)) 
    #      if (input$plotType=='line') 
    #        yOpts <- setdiff(yOpts, c(input$x, input$color))
    yOpts
  })
  
  colOpts <- reactive({
    dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    colOpts <- c('None', factorVars())
    #     if (!is.null(input$plotType)) 
    #       if (input$plotType=='line') 
    #         colOpts <- setdiff(colOpts, input$x)
    colOpts
  })
  
  facetOpts <- reactive({
    facetOpts <- factorVars()
    #     if (!is.null(input$plotType)) 
    #       if (input$plotType=='line')
    #         facetOpts <- setdiff(facetOpts, c(input$x, input$y))
    facetOpts
  })
  
  ## 
  
  ## plot reactive
  plotInput <- reactive({
    dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    plotType <- input$plotType
    
    ## scatter plot
    if (plotType=='scatter') {
      p <- ggplot(dataset, aes_string(x=input$x, y=input$y)) + 
        geom_point()
    } 
    
    ## line plot
    else if (plotType=='line') {
      p <- ggplot(dataset, aes_string(x=input$x, y=input$y))
      print(head(dataset))
      print(input$x)
      print(input$y)
      fuck <<- dataset
      
      
      
      #if (input$x==input$color | input$color=='None') {
      if (input$color=='None') {
        p <- p + geom_line(aes(group=1))
      }
      else {
        print('tan')
        p <- p + geom_line(aes_string(group=input$color))
      }
    }
    
    ## bar plot
    else if (plotType=='bar') {
      p <- ggplot(dataset, aes_string(x=input$x, y=input$y)) +
        geom_bar(stat='identity')
      
      if (input$color != 'None') {
        p <- p + aes_string(fill=input$color)
      }
    }
    
    ## plot colors
    if (input$color != 'None') {
      p <- p + aes_string(color=input$color)      
    }
    
    ## x and y facets
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)   
    
    ## return
    p
  })
  
  ## aggregation-by options
  output$aggByCtrl <- renderUI({
    aggByOpts <- origVars()
    selectInput('aggBy', 'Aggregate By', choices=aggByOpts, multiple=T)
  })
  
  ## aggregation target options
  output$aggTargetCtrl <- renderUI({
    aggTargetOpts <- origNumericVars()
    selectInput('aggTarget', 'Aggregation Target', choices=aggTargetOpts, multiple=T)
  })
  
  ## aggregation method options
  output$aggMethCtrl <- renderUI({
    aggMethOpts <- c('mean', 'sum', 'count')
    selectInput('aggMeth', 'Aggregation Method', choices=aggMethOpts, multiple=T)
  })
  
  ## aggregation share of
  output$shareOfCtrl <- renderUI({
    shareOfOpts <- factorVars()
    selectInput('shareOf', 'Share Of', choices=c('None', shareOfOpts))
  })
  
  ## aggregation share target
  output$shareTargetCtrl <- renderUI({
    shareTargetOpts <- numericVars()
    selectInput('shareTarget', 'Share Target', choices=c('None', shareTargetOpts))
  })
  
  ## x-axis options
  output$xCtrl <- renderUI({
    selectInput('x', 'X', xOpts())
  })
  
  ## y-axis options
  output$yCtrl <- renderUI({
    selectInput('y', 'Y', yOpts())
  })
  
  ## color control options
  output$colCtrl <- renderUI({
    selectInput('color', 'Color', colOpts())
  })
  
  ## row-wise facet-options
  output$facetRowCtrl <- renderUI({
    selectInput('facet_row', 'Facet Row', c(None='.', facetOpts()))
  })
  
  ## column-wise facet options
  output$facetColCtrl <- renderUI({
    selectInput('facet_col', 'Facet Column', c(None='.', facetOpts()))
  })
  
  ## plot type options
  output$plotTypeCtrl <- renderUI({
    dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
    selectInput(inputId = "plotType", label = "Plot Type", 
                choices = c('Line'='line', 'Scatter'='scatter', 'Bar'='bar'),
                multiple = FALSE)
  })
  
  ## CSV download button (for UI)
  output$dlBtnCSV <- renderUI({
    downloadButton('dlCSV', 'Download')
  })
  
  ## image download button (for UI)
  output$dlBtnPlot <- renderUI({
    downloadButton('dlPlot', 'Export Plot')
  })
  
  ## CSV download handler
  output$dlCSV <- downloadHandler(
    filename = function() { 
      ts <- gsub(' |-|:', '', as.character(Sys.time()))
      paste0('output_', ts, '.csv') 
    },
    content = function(file) {
      write.csv(dataset(), file, row.names=F)
    }
  )
  
  ## image download handler
  output$dlPlot <- downloadHandler(
    filename = function() { 
      ts <- gsub(' |-|:', '', as.character(Sys.time()))
      paste0('output_', ts, '.png') 
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = plotInput(), device = device)
    }
  )
  
  ## register the final dataset to be used upon AJAX call from UI
  action <- reactive({DT::dataTableAjax(session, finalDF())})
  
  ## construct server-side datatable
  datatable <- reactive({
    dataset <- finalDF(); if (is.null(dataset)) {return(NULL)}
    DT::datatable(dataset, 
                  filter = 'bottom',
                  server = TRUE,
                  options = list(ajax = list(url = action()),
                                 lengthChange = TRUE,
                                 lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100', 'All'))))
  })
  
  ## display tabular datatable content
  output$table <- DT::renderDataTable({
    datatable()
  })
  
  ## display plot
  output$plot <- renderPlot({
    print(plotInput())
  }, height=700)
  
})

