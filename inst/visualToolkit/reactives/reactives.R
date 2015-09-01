#### variables for rawDataset() -- probably not very useful

## original variables
origVars <- reactive({
  dataset <- rawDataset(); if (is.null(dataset)) return()
  colnames(dataset)
})

## original factor variables
origFactorVars <- reactive({
  dataset <- rawDataset(); if (is.null(dataset)) return()
  getFactorVarNames(dataset)
})

## original numeric variables
origNumericVars <- reactive({
  dataset <- rawDataset(); if (is.null(dataset)) return()
  getNumericVarNames(dataset)
})



#### variables for dataset() -- raw or manually aggregated dataset

## processed dataset factor variables
factorVars <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  getFactorVarNames(dataset)
})

## processed dataset numeric variables
numericVars <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  getNumericVarNames(dataset)
})

## processed dataset variables with less than or equal to N unique values
varsUniqValsCntLOEN <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  n <- input$nUniqValsCntThres; if (is.null(n)) return()
  getVarNamesUniqValsCntLOEN(dataset, n)
})



#### variables for finalDF()
finalDF <- reactive({  
  if (is.null(semiAutoAggOn())) return()
  
  ## semi-automatic aggregation (if enabled)
  if (semiAutoAggOn())
    semiAutoAggDF()

  ## natural dataset (raw or manually aggregated dataset)
  else
    dataset()
})

## number of rows
nrows <- reactive({
  if (is.null(finalDF())) return()
  nrow(finalDF())
})

finalDFVars <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  colnames(dataset)
})

finalDFFactorVars <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  getFactorVarNames(dataset)
})

finalDFNumericVars <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  getNumericVarNames(dataset)
})

# xRange <- reactive({
#   dataset <- finalDF(); if (is.null(dataset)) return()
#   if (is.null(input$x)) return()
#   if (input$x %in% finalDFNumericVars())
#     range(dataset[input$x], na.rm=TRUE)
# })

## work-around for round error in sliderInput (for consistency w/ yRange())
xRange <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(input$x)) return()
  if (input$x %in% finalDFNumericVars())
    range <- range(dataset[input$x], na.rm=TRUE)
  range[1] <- range[1] - 1
  range[2] <- range[2] + 1
  range
})

# yRange <- reactive({
#   dataset <- finalDF(); if (is.null(dataset)) return()
#   y <- y()
#   if (is.null(y)) return()
#   if (y %in% finalDFNumericVars())
#     range(dataset[[y]], na.rm=TRUE)
# })

## work-around for rounding error in sliderInput
yRange <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  y <- y()
  if (is.null(y)) return()
  if (y %in% finalDFNumericVars())
    range <- range(dataset[[y]], na.rm=TRUE)
  range[1] <- range[1] - 1
  range[2] <- range[2] + 1
  range
})

xFactorVarUniqVals <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(input$x)) return()
  if (input$x %in% finalDFFactorVars()) {
    unique(as.character(dataset[[input$x]]))
  }
})

yFactorVarUniqVals <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  y <- y()
  if (is.null(y)) return()
  if (y %in% finalDFFactorVars()) {
    levels(dataset[[y]])
  }
})

y <- reactive({
  if (is.null(input$y)) return()
  if (is.null(input$plotAggMeth)) return()
  if (is.null(finalDF())) return()
  if (is.null(semiAutoAggOn())) return()
  y <- ensureProperVarName2(colnames=colnames(finalDF()), var=input$y, aggMeth=input$plotAggMeth, semiAutoAggOn=semiAutoAggOn())
  y
})




## conditional: facet widgets are loaded
facetWidgetsLoaded <- reactive({
  wgts <- c('facetCol', 'facetRow', 'facetWrap')
  return(checkWidgetsLoaded(input, wgts))
})

## conditional: no facet was selected
noFacetSelected <- reactive({
  if (!facetWidgetsLoaded()) return(FALSE)
  facetFam <- c(input$facetCol, input$facetRow, input$facetWrap)
  noFacetSelected <- all('None' == facetFam) | all('' == facetFam)
  return(noFacetSelected)
})

## conditional: facet grid was selected
facetGridSelected <- reactive({
  if (!facetWidgetsLoaded()) return(FALSE)
  return(input$facetCol != 'None' | input$facetRow != 'None')
})

## conditional: facet wrap was selected
facetWrapSelected <- reactive({
  if (!facetWidgetsLoaded()) return(FALSE)
  return(input$facetWrap != 'None')
})



## reactive that returns TRUE if plot utilizes both x and y controls
isXYCtrlPlot <- reactive({
  if (is.null(input$plotType)) return()
  return(input$plotType %in% c('line', 'scatter', 'bar', 'box', 'path'))
})


## reactive that returns a value "discrete" or "continuous"
xType <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(input$x)) return()

  if (input$x %in% finalDFNumericVars()) {
    return('continuous')
  } else {
    return('discrete')
  }
})


## reactive that returns a value "discrete" or "continuous"
yType <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (!isXYCtrlPlot()) return()
  if (is.null(y())) return()
  
  if (y() %in% finalDFNumericVars()) {
    return('continuous')
  } else {
    return('discrete')
  }
})


## conditional reactive: semi-automatic aggregation is on
semiAutoAggOn <- reactive({
  if (is.null(input$plotAggMeth)) return()
  tolower(input$plotAggMeth) != 'none'
})


