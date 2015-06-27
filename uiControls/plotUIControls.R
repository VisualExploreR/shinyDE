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
              choices = c('Line'='line', 'Scatter'='scatter', 'Bar'='bar',
                          'Histogram'='histogram', 'Density'='density', 'Box'='box'),
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
  
})

## geom smooth options
output$smthCtrl <- renderUI({
  
})

## size options
output$sizeCtrl <- renderUI({
  selectInput('size', 'Size', sizeOpts())
})

## shape options
output$shapeCtrl <- renderUI({
  selectInput('shape', 'Shape', shapeOpts())
})


