
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
  if (is.null(fileInfo)) return()
  if (is.null(input$header) | is.null(input$sep) | is.null(input$quote)) return()
  read.csv(fileInfo$datapath, header = as.logical(input$header),
           sep = input$sep, quote = input$quote)
})

## reactive variable for custom dataset name
customDatasetName <- reactive({
  customDatasetFileInfo()$name
})

## reactive variable for raw dataset names
rawDatasetNames <- reactive({
## USE THIS TO DEBUG "class(obj)=='data.frame' has length > 1" ERROR MESSAGE
#   print('yo')
#   a <- customDatasetName()
#   print('bro')
#   b <- getLoadedDataFrameNames()
#   print('ho')
  
  c("diamonds", "mtcars", "rock", "pressure", 
    customDatasetName(),
    getLoadedDataFrameNames())
})

## reactive variable for raw dataset
rawDataset <- reactive({
  ## from dataset selection drop-down
  if (is.null(input$dataset)) return() 
  
  ## if no custom dataset was uploaded, then set one of the preloaded datasets as raw dataset
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

## raw or aggregated dataset
dataset <- reactive({
  if (is.null(input$rawVsManAgg)) return()

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

## reactive for semi-automatic aggregate by
semiAutoAggBy <- reactive({
  aggBy <- c(input$x, input$color, input$size, input$shape, input$fill, input$facetRow, input$facetCol, input$facetWrap)
  aggBy <- unique(aggBy)
  nonAggBy <- c('None', 'none', '.')
  aggBy <- setdiff(aggBy, nonAggBy)
  aggBy
})

## reactive for semi-automatic aggregated dataset
semiAutoAggDF <- reactive({
  if (is.null(input$semiAutoAgg)) return()
  if (is.null(dataset())) return()  
  
  if (input$semiAutoAgg=='allowed') {  
    ## if plot aggregation is specified (e.g. sum, mean, max, min)
    if (input$plotAggMeth != 'None') {
      aggBy <- input$x
      #aggBy <- semiAutoAggBy()
      aggTarget <- input$y
      aggMeth <- input$plotAggMeth
      aggregate(dataset(), aggBy=aggBy, aggTarget=input$y, aggMeth=input$plotAggMeth)
    } 
  }
})

## reactive variable for final dataset
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


