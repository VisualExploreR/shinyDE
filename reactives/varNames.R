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
  dataset <- finalDF(); if (is.null(dataset)) {return(NULL)}
  getFactorVarNames(dataset)
})

## processed dataset numeric variables
numericVars <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) {return(NULL)}
  getNumericVarNames(dataset)
})

## processed dataset variables with less than or equal to N unique values
varsUniqValsCntLOEN <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) {return(NULL)}
  n <- input$nUniqValsCntThres; if (is.null(n)) {return(NULL)}
  getVarNamesUniqValsCntLOEN(dataset, n)
})

