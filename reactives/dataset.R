
## FOR DEVELOPMENT ONLY ##
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
## FOR DEVELOPMENT ONLY ## 

## reactive variable for custom (uploadable) dataset file info
customDatasetFileInfo <- reactive({
  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  fileInfo <- input$file
  fileInfo  
})

## reactive variable for custom (uploaded) dataset
customDataset <- reactive({
  fileInfo <- customDatasetFileInfo()
  if (is.null(fileInfo)) {return(NULL)}
  read.csv(fileInfo$datapath, header = as.logical(input$header),
           sep = input$sep, quote = input$quote)
})

## reactive variable for custom dataset name
customDatasetName <- reactive({
  customDatasetFileInfo()$name
})

## reactive variable for raw dataset names
rawDatasetNames <- reactive({
  c("mtcars", "rock", "pressure", "diamonds", 
    customDatasetName(),
    getLoadedDataFrameNames())
})

## reactive variable for raw dataset
rawDataset <- reactive({
  if (is.null(input$dataset)) {return(NULL)} 

  ## if no dataset was uploaded, then set one of the preloaded datasets as raw dataset
  if (is.null(input$file)) {
    get(input$dataset)
  }
  
  ## if custom dataset was uploaded
  else {
    ## if custom dataset was selected, then set it as raw dataset
    if (input$dataset == customDatasetName()) {
      customDataset()      
    } 
    
    ## if custom dataset was not selected, then set one of the preloaded datasets as raw dataset
    else {
      get(input$dataset)
    }
  }
})

## manually aggregated dataset
manAggDataset <- reactive({
  ## if all fields for manual aggregation are filled in
  if (!is.null(input$aggBy) & !is.null(input$aggTarget) & !is.null(input$aggMeth)) {
    ## return manually aggregated dataset
    aggregate(rawDataset(), input$aggBy, input$aggTarget, input$aggMeth)
  } 
  
  ## else, return raw dataset  
  else {
    rawDataset()
  }
})

## semi-manually aggregated dataset
# semiManAggDataset <- reactive({
#   ## if plot aggregation is specified
#   if (input$plotAggMeth != 'None') {
#     aggBy <- c(input$x)
#     
#     if (input$facetRow != 'None') aggBy <- c(aggBy, input$facetRow)
#     if (input$facetColumn != 'None') aggBy <- c(aggBy, input$facetColumn)
#     if (input$color != 'None') aggBy <- c(aggBy, input$color)
#     
#     aggregate(dataset(), aggBy=aggBy, aggTarget=input$y, aggMeth=input$plotAggMeth)
#   } 
# 
#   ## else, return raw dataset
#   else {
#     dataset()
#   }
# })

## raw or aggregated dataset
dataset <- reactive({
  if (is.null(input$rawVsManAgg)) {return(NULL)}

  ## raw dataset
  if (input$rawVsManAgg == 'raw') {
    dataset <- rawDataset()
  } 

  ## aggregated dataset
  else if (input$rawVsManAgg=='manAgg') {
    dataset <- manAggDataset()
  }
  
  dataset
})

## reactive variable for final dataset
finalDF <- reactive({  
  if (is.null(input$semiAutoAgg)) {return(NULL)}
  
  ## semi-automatic aggregation (if enabled)
  if (input$semiAutoAgg=='allowed') {
    ## if plot aggregation is specified
    if (input$plotAggMeth != 'None') {
      aggBy <- input$x
      aggTarget <- input$y
      aggMeth <- input$plotAggMeth
            
      ## append to aggBy
      if (!is.null(input$facetRow) & input$facetRow != '.') 
        aggBy <- c(aggBy, input$facetRow)
      if (!is.null(input$facetCol) & input$facetCol != '.') 
        aggBy <- c(aggBy, input$facetCol)
      if (!is.null(input$color) & input$color != 'None') 
        aggBy <- c(aggBy, input$color)
            
      aggregate(dataset(), aggBy=aggBy, aggTarget=input$y, aggMeth=input$plotAggMeth)
    } 
  } 
  
  else if (input$semiAutoAgg=='disabled') {
    dataset()
  }  
})


## final dataset
# finalDF <- reactive({
#   dataset <- dataset()
#   if (!is.null(input$shareOf) & !is.null(input$shareTarget))
#     if (input$shareOf != 'None' & input$shareTarget != 'None')
#       dataset <- calcShare(dataset, input$shareOf, input$shareTarget)
#   dataset
# })

