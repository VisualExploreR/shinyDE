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



#### display conditional reactives
## display y condition reactive
displayYCond <- reactive({
  if (is.null(input$plotType)) return()  
  display <- TRUE
  return (!(input$plotType %in% c('histogram', 'density')))
})
  
## display color condition reactive
displayColCond <- reactive({
  if (is.null(input$plotType)) return()
  return (any(input$plotType %in% c('line', 'scatter', 'path'))) 
})

## display treat-as-a-factor-variable (for color) condition reactive
displayTreatAsFacVarColCond <- reactive({
  if (is.null(input$plotType)) return()
  return (any(input$plotType %in% c('scatter'))) 
})

## display fill condition reactive
displayFillCond <- reactive({
  if (is.null(input$plotType)) return()
  return (any(input$plotType %in% c('box', 'histogram', 'bar', 'density')))
})


## display position condition reactive
displayPosCond <- reactive({
  if (is.null(input$plotType)) return()
  return (any(input$plotType %in% c('histogram', 'bar')))
})

## display size magnifier condition reactive
displaySizeMag <- reactive({
  if (is.null(input$plotType)) return()  
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- TRUE
  } else if (any(input$plotType %in% c('line', 'path'))) {    
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- TRUE
  }
  display
})

## display shape condition reactive
displayShapeCond <- reactive({
  if (is.null(input$plotType)) return()  
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- TRUE
  } else if (any(input$plotType %in% c('line', 'path'))) {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- TRUE
  }
  display
})

## display size condition reactive
displaySizeCond <- reactive({
  if (is.null(input$plotType)) return()  
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- TRUE
  } else if (any(input$plotType %in% c('line', 'path'))) {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- TRUE
  }
  display
})

## display jitter condition reactive
displayJitCond <- reactive({
  if (is.null(input$plotType)) return()  
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- TRUE
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- TRUE
  }
  display
})

## display smooth condition reactive
displaySmthCond <- reactive({
  if (is.null(input$plotType)) return()  
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- TRUE
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- TRUE
  }
  display  
})

## display bin width condition reactive
displayBinWidthCond <- reactive({
  if (is.null(input$plotType)) return() 
  if (is.null(input$x)) return()
  return (input$plotType=='histogram')
})

## display density blakc line condition reactive
displayDensBlkLineCond <- reactive({
  if (is.null(input$plotType)) return()
  return (input$plotType=='density')
})

## display points overlay checkbox condition reactive
displayPtsOverlayCond <- reactive({
  if (is.null(input$plotType)) return()
  return (input$plotType %in% c('line', 'path'))
})

## display additional aggregation select field condition reactive
displayPlotAddAggBy <- reactive({
  if (is.null(input$semiAutoAgg)) return()
  return (input$semiAutoAgg=='allowed')
})



#### widgets loaded conditional reactives
generalWidgetsLoaded <- reactive({
  wgtCtrls <- c('x', 'facetRow', 'facetCol', 'color', 'plotType', 'alpha', 'coordFlip', 'semiAutoAgg', 'plotAggMeth')
  checkWidgetsLoaded(input, wgtCtrls)
})

scatterWidgetsLoaded <- reactive({
  if (!generalWidgetsLoaded()) return(FALSE)
  wgtCtrls <- c('y', 'color', 'treatAsFacVarCol', 'shape', 'size', 'sizeMag', 'jitter', 'smooth', 'sizeMag', 'xlim', 'ylim')
  checkWidgetsLoaded(input, wgtCtrls)
})

lineWidgetsLoaded <- reactive({
  if (!generalWidgetsLoaded()) return(FALSE)
  wgtCtrls <- c('y', 'color')
  checkWidgetsLoaded(input, wgtCtrls)
})

linePtsOverlayWidgetsLoaded <- reactive({
  if (!generalWidgetsLoaded()) return(FALSE)
  wgtCtrls <- c('shape', 'size', 'jitter', 'smooth', 'ptsOverlayCond')
  checkWidgetsLoaded(input, wgtCtrls)
})

barWidgetsLoaded <- reactive({
  if (!generalWidgetsLoaded()) return(FALSE)
  wgtCtrls <- c('y', 'fill', 'position')
  checkWidgetsLoaded(input, wgtCtrls)
})

histogramWidgetsLoaded <- reactive({
  if (!generalWidgetsLoaded()) return(FALSE)
  wgtCtrls <- c('fill', 'position', 'binWidth')
  checkWidgetsLoaded(input, wgtCtrls)
})

densityWidgetsLoaded <- reactive({
  if (!generalWidgetsLoaded()) return(FALSE)
  wgtCtrls <- c('fill', 'color', 'densBlkLineCond')
  checkWidgetsLoaded(input, wgtCtrls)
})

boxWidgetsLoaded <- reactive({
  if (!generalWidgetsLoaded()) return(FALSE)
  wgtCtrls <- c('y', 'fill')
  checkWidgetsLoaded(input, wgtCtrls)
})

pathWidgetsLoaded <- reactive({
  if (!generalWidgetsLoaded()) return(FALSE)
  checkWidgetsLoaded(input, 'y')
})

pathPtsOverlayWidgetsLoaded <- reactive({
  if (!generalWidgetsLoaded()) return(FALSE)
  wgtCtrls <- c('shape', 'size', 'ptsOverlayCond')
  checkWidgetsLoaded(input, wgtCtrls)    
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
  
  ## don't plot anything if any of the general control pieces are missing (i.e. not loaded)
  if (!generalWidgetsLoaded()) return() 
  if (!(x %in% xOpts())) return()
  
  ## ensure proper variable names (in case of semi-automatic aggregation)
  y <- y()
  color <- ensureProperVarName2(colnames=colnames(dataset), var=color, aggMeth=plotAggMeth, semiAutoAggOn=semiAutoAggOn)
  size <- ensureProperVarName2(colnames=colnames(dataset), var=size, aggMeth=plotAggMeth, semiAutoAggOn=semiAutoAggOn)
  
  ## scatter plot
  if (plotType=='scatter')  {
    if (!scatterWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    #if (!checkVarAndLimCompatible(dataset, x, xlim)) return()
    #if (!checkVarAndLimCompatible(dataset, y, ylim)) return()
    
    p <- plotScatter(dataset, x, y, color, treatAsFacVarCol, shape, size, alpha, jitter, smooth, sizeMag, xlim, ylim)
  }

  ## line plot
  else if (plotType=='line') {
    if (!lineWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    #if (!checkVarAndLimCompatible(dataset, x, xlim)) return()
    #if (!checkVarAndLimCompatible(dataset, y, ylim)) return()
    
    p <- plotLine(dataset, x, y, color, alpha, xlim, ylim)

    ## line plot with points overlay
    if (!linePtsOverlayWidgetsLoaded()) return()
    if (ptsOverlayCond)
      p <- plotPointsOverlay(p, shape, size, alpha, jitter, smooth, sizeMag)
  }
  
  ## bar plot
  else if (plotType=='bar') {
    if (!barWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    #if (!checkVarAndLimCompatible(dataset, x, xlim)) return()
    #if (!checkVarAndLimCompatible(dataset, y, ylim)) return()
    
    p <- plotBar(dataset, x, y, fill, position, alpha, xlim, ylim)
  }
  
  ## histogram
  else if (plotType=='histogram') {
    if (!histogramWidgetsLoaded()) return()
    range1 <- range(dataset[[x]], na.rm=TRUE)
    range2 <- xlim
    if (!checkTwoRangesOverlap(range1, range2)) return()
    
    p <- plotHistogram(dataset, x, fill, position, binWidth, alpha, xlim)
  }
    
  ## density plot
  else if (plotType=='density') {
    if (!densityWidgetsLoaded()) return()
    p <- plotDensity(dataset, x, fill, alpha, densBlkLineCond, xlim)
  }    
  
  ## box plot
  else if (plotType=='box') {
    if (!boxWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    #if (!checkVarAndLimCompatible(dataset, x, xlim)) return()
    #if (!checkVarAndLimCompatible(dataset, y, ylim)) return()
    p <- plotBox(dataset, x, y, fill, alpha, xlim, ylim)
  }
  
  ## path plot
  else if (plotType=='path') {
    if (!pathWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotPath(dataset, x, y, alpha, xlim, ylim)
    
    ## path plot with points overlay
    if (!pathPtsOverlayWidgetsLoaded()) return()
    if (ptsOverlayCond)
      p <- plotPointsOverlay(p, shape, size, alpha, jitter, smooth, sizeMag)
  }
  
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
    
  ## return
  p
})

