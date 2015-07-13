## x options reactive
xOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  xOpts <- names(dataset)
  #    if (!is.null(input$plotType)) 
  #      if (input$plotType=='line') 
  #        xOpts <- setdiff(xOpts, input$color)
  xOpts
})

## y options reactive
yOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  yOpts <- names(dataset)
  #    if (!is.null(input$plotType)) 
  #      if (input$plotType=='line') 
  #        yOpts <- setdiff(yOpts, c(input$x, input$color))
  yOpts
})

## color options reactive
colOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  colOpts <- c('None', factorVars())
  #     if (!is.null(input$plotType)) 
  #       if (input$plotType=='line') 
  #         colOpts <- setdiff(colOpts, input$x)
  colOpts
})

## fill options reactive
fillOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  fillOpts <- c('None', factorVars())
  fillOpts      
})

## facet options reactive
facetOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  facetOpts <- c(None='.', factorVars())
  #     if (!is.null(input$plotType)) 
  #       if (input$plotType=='line')
  #         facetOpts <- setdiff(facetOpts, c(input$x, input$y))
  facetOpts
})

## size options reactive
sizeOpts <- reactive({
  c('None', numericVars())
})

## shape options reactive
shapeOpts <- reactive({
  #c('None', factorVars())
  dataset <- dataset(); if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)  
  vars <- setdiff(varsUniqValsCntLOE6, numericVars())
  c('None', vars)
})

## histogram max bin width reactive
histMaxBinWidth <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
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

## display facet wrap condition reactive
displayFacetWrapCond <- reactive({
  #if (is.null(input$facetRow) | is.null(input$facetCol)) return()  
  #if (all(c(input$facetRow, input$facetCol) %in% '.')) {
  TRUE
})

## display facet scale condition reactive
displayFacetScaleCond <- reactive({
  #if (is.null(input$facetRow) | is.null(input$facetCol) | is.null(input$facetWrap)) return()
  #if (!all(c(input$facetRow, input$facetCol, input$facetWrap) %in% '.')) {
  TRUE
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



## plot reactive
plotInput <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()

  ## control variables
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
  
  ## don't plot anything if any of the general control pieces are missing (i.e. not loaded)
  wgtCtrls <- c('x', 'facetRow', 'facetCol', 'color', 'plotType', 'alpha', 'coordFlip')
  wgtsLoaded <- checkWidgetsLoaded(input, wgtCtrls)
  if (!wgtsLoaded) return()
  if (!(x %in% xOpts()) | !(y %in% yOpts())) return()
  
  ## ensure proper y variable name (in case of semi-automatic aggregation)
  #print(y)
  y <- ensureProperYVarName(dataset, y)
  #print(y)

#   print('----')
#   names(dataset)
#   print(color)
#   color <- ensureProperYVarName(dataset, color)
#   print(color)
  
  ## scatter plot
  if (plotType=='scatter') {
    wgtCtrls <- c('shape', 'size', 'sizeMag', 'jitter', 'smooth', 'sizeMag')
    wgtsLoaded <- checkWidgetsLoaded(input, wgtCtrls)
    if (!wgtsLoaded) return()
    p <- plotScatter(dataset, x, y, shape, size, alpha, jitter, smooth, sizeMag, nrows())
  }
  
  ## line plot
  else if (plotType=='line') {
    p <- plotLine(dataset=dataset, x=x, y=y, color=color, alpha=alpha)

    ## line plot with points overlay
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) {
      wgtCtrls <- c('shape', 'size', 'jitter', 'smooth')
      wgtsLoaded <- checkWidgetsLoaded(input, wgtCtrls)
      if (!wgtsLoaded) return()
      p <- plotPointsOverlay(p, shape, size, alpha, jitter, smooth, sizeMag)
    }
  }
  
  ## bar plot
  else if (plotType=='bar') {
    wgtCtrls <- c('fill', 'position')
    wgtsLoaded <- checkWidgetsLoaded(input, wgtCtrls)
    if (!wgtsLoaded) return()
    p <- plotBar(dataset, x, y, fill, position, alpha)
  }
  
  ## histogram
  else if (plotType=='histogram') {
    wgtCtrls <- c('fill', 'position', 'binWidth')
    wgtsLoaded <- checkWidgetsLoaded(input, wgtCtrls)
    if (!wgtsLoaded) return()
    p <- plotHistogram(dataset, x, fill, position, binWidth, alpha)
  }
    
  ## density plot
  else if (plotType=='density') {
    wgtCtrls <- c('fill', 'color', 'densBlkLineCond')
    wgtsLoaded <- checkWidgetsLoaded(input, wgtCtrls)
    if (!wgtsLoaded) return()
    p <- plotDensity(dataset, x, fill, alpha, densBlkLineCond)
  }    
  
  ## box plot
  else if (plotType=='box') {
    wgtCtrls <- c('fill')
    wgtsLoaded <- checkWidgetsLoaded(input, wgtCtrls)
    if (!wgtsLoaded) return()
    p <- plotBox(dataset, x, y, fill, alpha)
  }
  
  ## path plot
  else if (plotType=='path') {
    p <- plotPath(dataset, x, y, alpha)
    
    ## path plot with points overlay
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) {
      wgtCtrls <- c('shape', 'size')
      wgtsLoaded <- checkWidgetsLoaded(input, wgtCtrls)
      if (!wgtsLoaded) return()
      p <- plotPointsOverlay(p, shape, size, alpha, jitter, smooth, sizeMag)
    }
  }

  ## plot colors
  if (color != 'None') {
    p <- p + aes_string(color=color)      
  }
  
  ## facet grids
  facetGrids <- paste(facetRow, '~', facetCol)
  if (facetGrids != '. ~ .')
    p <- p + facet_grid(facetGrids) 
  
  ## facet wrap
  # facetWrap <- paste('~', facetWrap)
  
  ## coordinate flip
  if (coordFlip) {
    p <- p + coord_flip()
  }
    
  ## return
  p
})





