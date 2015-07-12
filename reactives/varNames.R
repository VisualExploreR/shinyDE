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
  #dataset <- finalDF(); if (is.null(dataset)) return()
  n <- input$nUniqValsCntThres; if (is.null(n)) return()
  getVarNamesUniqValsCntLOEN(dataset, n)
})



#### variables for finalDF() -- may not be useful (after the talk with Eugene on 5/12/2015)





