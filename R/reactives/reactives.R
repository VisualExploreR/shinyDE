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
  #dataset <- finalDF(); if (is.null(dataset)) return()
  getFactorVarNames(dataset)
})

## processed dataset numeric variables
numericVars <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  #dataset <- finalDF(); if (is.null(dataset)) return()
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
  if (is.null(input$semiAutoAgg)) return()
  
  ## semi-automatic aggregation (if enabled)
  if (input$semiAutoAgg=='allowed') {
    semiAutoAggDF()
  } 
  
  ## natural dataset (raw or manually aggregated dataset)
  else if (input$semiAutoAgg=='disabled') {
    dataset()
  }  
})

## number of rows
nrows <- reactive({
  if (is.null(finalDF())) return()
  nrow(finalDF())
})

finalDFFactorVars <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  getFactorVarNames(dataset)
})

finalDFNumericVars <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  getNumericVarNames(dataset)
})

xRange <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(input$x)) return()
  if (input$x %in% finalDFNumericVars())
    range(dataset[input$x], na.rm=TRUE)
})

yRange <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(input$y)) return()
  if (input$y %in% finalDFNumericVars())
    range(dataset[input$y], na.rm=TRUE)
})

# xRangeStepSize <- reactive({
#   if (is.null(xRange())) return()
#   next
# })
# 
# yRangeStepSize <- reactive({
#   if (is.null(yRange())) return()
#   next
# })

xFactorVarUniqVals <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(input$x)) return()
  if (input$x %in% finalDFFactorVars()) {
    unique(dataset[[input$x]])
  }
})

yFactorVarUniqVals <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(input$y)) return()
  if (input$y %in% finalDFFactorVars()) {
    unique(dataset[[input$y]])
  }
})

