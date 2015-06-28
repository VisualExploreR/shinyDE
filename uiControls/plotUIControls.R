## aggregation method options (for plot view only)
output$plotAggMethCtrl <- renderUI({
  aggMethOpts <- c('sum', 'mean', 'count', 'min', 'max', 'median')
  selectInput('plotAggMeth', 'Aggregation Method', aggMethOpts)
  #selectInput('plotAggMeth', 'Aggregation Method', choices=c(aggMethOpts, 'None'))
})

## x-axis options
output$xCtrl <- renderUI({
  selectInput('x', 'X', xOpts())
})

## y-axis options
output$yCtrl <- renderUI({
  if (is.null(input$plotType)) {return(NULL)}
  if (input$plotType != 'histogram')
    selectInput('y', 'Y', yOpts())
})

## color control options
output$colCtrl <- renderUI({
  selectInput('color', 'Color', colOpts())
})

## row-wise facet-options
output$facetRowCtrl <- renderUI({
  selectInput('facetRow', 'Facet Row', facetOpts())
})

## column-wise facet options
output$facetColCtrl <- renderUI({
  selectInput('facetCol', 'Facet Column', facetOpts())
})

## facet wrap options
output$facetWrapCtrl <- renderUI({
  selectInput('facetWrap', 'Facet Wrap', facetOpts())
})

## plot type options
output$plotTypeCtrl <- renderUI({
  dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
  selectInput(inputId = "plotType", label = "Plot Type", 
              choices = c('Line'='line', 'Scatter'='scatter', 'Graph'='graph',
                          'Bar'='bar', 'Histogram'='histogram', 
                          'Density'='density', 'Box'='box',
                          'Path'='path'),
              multiple = FALSE)
})


## alpha (opacity) options
output$alphaCtrl <- renderUI({
  dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
  sliderInput("alpha", label = h5("Opacity"),
              min = 0, max = 1, value = 1, step = 0.1)
})

## jitter options
output$jitCtrl <- renderUI({
  if (is.null(input$plotType)) {return(NULL)}
  if (input$plotType %in% c('scatter', 'graph')) {
    selectInput('jitter', 'Jitter Effect', 
                c('None'='None', 'Jitter'='jitter'))
  }
})

## geom smoothing options
output$smthCtrl <- renderUI({
  if (is.null(input$plotType)) {return(NULL)}
  if (input$plotType %in% c('scatter', 'graph')) {
    if (all(c(input$x, input$y) %in% numericVars())) {
      selectInput('smooth', 'Smoothing Effect', 
                  c('None'='None', 'Linear'='lm', 'Loess'='loess'))
    }
  }  
})

## size options
output$sizeCtrl <- renderUI({
  if (is.null(input$plotType)) {return(NULL)}
  if (input$plotType %in% c('scatter', 'graph'))
    selectInput('size', 'Size', sizeOpts())        
})

## shape options
output$shapeCtrl <- renderUI({
  if (is.null(input$plotType)) {return(NULL)}
  if (input$plotType %in% c('scatter', 'graph'))
    selectInput('shape', 'Shape', shapeOpts())
})

## coordinate flip options 
output$coordFlipCtrl <- renderUI({
  checkboxInput('coordFlip', 'Flip X and Y coordinates.', value = FALSE)
})

## histogram binwidth options
output$binWidthCtrl <- renderUI({
  if (is.null(input$plotType)) {return(NULL)} 
  if (input$plotType=='histogram')
    selectInput('binWidth', 'Bin Width', c('1'='1', '2'='2'))
})







#### TESTING 
## general plot UI controls

## scatter plot UI controls

## line plot UI controls

## bar plot UI controls

## histogram UI controls

## density plot UI controls

## box plot UI controls
