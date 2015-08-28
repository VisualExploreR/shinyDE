## x options reactive
xOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  names(dataset)
})

## y options reactive
yOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  names(dataset)
})

## color options reactive
colOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  if (is.null(input$plotType)) return()
  
  if (input$plotType=='scatter') {
    colOpts <- c('None', names(dataset()))
  } else if (input$plotType %in% c('line', 'path')) {
    colOpts <- c('None', factorVars())
  }

  colOpts
})

## fill options reactive
fillOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  fillOpts <- c('None', factorVars(), varsUniqValsCntLOE6)
  fillOpts      
})

## facet options reactive
facetOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  facetOpts <- c('None', factorVars(), varsUniqValsCntLOE6)
  facetOpts
})

## size options reactive
sizeOpts <- reactive({
  c('None', numericVars())
})

## shape options reactive
shapeOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  #vars <- setdiff(varsUniqValsCntLOE6, numericVars())
  #c('None', vars)
  c('None', varsUniqValsCntLOE6)
})

## histogram max bin width reactive
histMaxBinWidth <- reactive({
  if (is.null(input$x)) return()
  dataset <- dataset(); if (is.null(dataset)) return()
  if (!(input$x %in% colnames(dataset))) return()
  maxBinWidth <- round(diff(range(dataset[[input$x]], na.rm=TRUE)))
  maxBinWidth 
})


## additional aggregation options reactive
plotAddAggByOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  setdiff(origVars(), plotSemiAutoAggByBase())
})


## plot reactive
plotInput <- reactive({
  
  ## load dataset to use (already subsetted/filtered)
  dataset <- plotDF(); if (is.null(dataset)) return()

  ## load variables from control widgets
  semiAutoAggSet <- input$semiAutoAgg=='allowed'
  plotType <- input$plotType
  x <- input$x
  y <- input$y
  facetRow <- input$facetRow
  facetCol <- input$facetCol
  facetWrap <- input$facetWrap
  facetScale <- input$facetScale
  color <- input$color
  fill <- input$fill
  alpha <- input$alpha
  coordFlip <- input$coordFlip
  shape <- input$shape
  size <- input$size
  sizeMag <- input$sizeMag
  jitter <- input$jitter
  smooth <- input$smooth
  position <- input$position
  binWidth <- input$binWidth
  densBlkLineCond <- input$densBlkLineCond
  ptsOverlayCond <- input$ptsOverlayCond
  xlim <- input$xlim
  ylim <- input$ylim
  treatAsFacVarCol <- input$treatAsFacVarCol
  plotAggMeth <- input$plotAggMeth
  semiAutoAggOn <- ifelse(input$semiAutoAgg=='allowed', TRUE, FALSE)
  
  ## don't plot anything if any of universal control widgets is not loaded
  if (!universalWidgetsLoaded()) return() 
  if (!(x %in% xOpts())) return()

  ## ensure proper variable names (in case of semi-automatic aggregation)
  y <- y()
  color <- ensureProperVarName2(colnames=colnames(dataset), var=color, aggMeth=plotAggMeth, semiAutoAggOn=semiAutoAggOn)
  size <- ensureProperVarName2(colnames=colnames(dataset), var=size, aggMeth=plotAggMeth, semiAutoAggOn=semiAutoAggOn)

  ## scatter plot
  if (plotType=='scatter')  {
    if (!scatterWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotScatter(dataset, x, y, color, treatAsFacVarCol, shape, size, smooth, jitter, alpha, sizeMag)
  }

  ## line plot
  else if (plotType=='line') {
    if (!lineWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotLine(dataset, x, y, color, alpha)

    ## line plot with points overlay
    if (!linePtsOverlayWidgetsLoaded()) return()
    if (ptsOverlayCond) {
      p <- plotPointsOverlay(p, shape, size, smooth, jitter, alpha, sizeMag)
    }
  }
  
  ## bar plot
  else if (plotType=='bar') {
    if (!barWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotBar(dataset, x, y, fill, position, alpha)
  }
  
  ## histogram
  else if (plotType=='histogram') {
    if (!histogramWidgetsLoaded()) return()
    p <- plotHistogram(dataset, x, fill, position, binWidth, alpha)
  }
    
  ## density plot
  else if (plotType=='density') {
    if (!densityWidgetsLoaded()) return()
    p <- plotDensity(dataset, x, fill, densBlkLineCond, alpha)
  }    
  
  ## box plot
  else if (plotType=='box') {
    if (!boxWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotBox(dataset, x, y, fill, alpha)
  }
  
  ## path plot
  else if (plotType=='path') {
    if (!pathWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotPath(dataset, x, y, alpha)
    
    ## path plot with points overlay
    if (!pathPtsOverlayWidgetsLoaded()) return()
    if (ptsOverlayCond)
      p <- plotPointsOverlay(p, shape, size, smooth, jitter, alpha, sizeMag)
  }
  
  ## plot advanced controls
  if (input$showAdvCtrlWgts) {

    ## if faceting is selected
    if (!noFacetSelected()) {
      
      facetCol <- ifelse(facetCol=='None', '.', facetCol)
      facetRow <- ifelse(facetRow=='None', '.', facetRow)
      facetWrap <- ifelse(facetWrap=='None', '.', facetWrap)
      
      ## facet grids
      if (facetGridSelected()) {
        facetGrids <- paste(facetRow, '~', facetCol)
        p <- p + facet_grid(facets=facetGrids, scales=facetScale)
      } 
      
      ## facet wrap
      else if (facetWrapSelected()) {
        p <- p + facet_wrap(facets=facetWrap, scales=facetScale)
      }
    }

    ## coordinate flip
    if (coordFlip) {
      p <- p + coord_flip()
    }
  }
    
  ## return
  p
})

